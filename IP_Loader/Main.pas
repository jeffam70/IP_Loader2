unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack;

type
  {Define XBee WiFi's udp commands}
  {IMPORTANT: Do not rearrange, append, or delete from this list without similarly modifying the ATCmd constant array}
  udpCommand = (udpIPAddr, udpIPPort, udpNodeID, udpRES, udpOutputMask, udpOutputState, udpIO2Timer);

const
  {Define XBee WiFi's AT commands}
  ATCmd : array[low(udpCommand)..high(udpCommand)] of Word =
    (
    {udpIPAddr}          Byte('M') + Byte('Y') shl 8,  {XBee's IP Address}
    {udpIPPort}          Byte('C') + Byte('0') shl 8,  {Xbee's UDP/IP Port}
    {udpNodeID}          Byte('N') + Byte('I') shl 8,  {Friendly node identifier string}
    {udpRES}             Byte('D') + Byte('2') shl 8,  {Designated reset pin}
    {udpOutputMask}      Byte('O') + Byte('M') shl 8,  {Output mask for all I/O pins (each 1 = output pin, each 0 = input pin)}
    {udpOutputState}     Byte('I') + Byte('O') shl 8,  {Output state for all I/O pins (each 1 = high, each 0 = low).  Period affected by updIO2Timer}
    {udpIO2Timer}        Byte('T') + Byte('2') shl 8   {I/O 2 state timer}
    );

  {Set of UDP Commands that take string parameters (instead of numeric parameters)}
  udpStrCommands : set of udpCommand = [udpIPAddr, udpNodeID];

type
  {Define XBee Transmission Packet - the format of packets transmitted from networked host to XBee}
  PxbTxPacket = ^TxbTxPacket;
  TxbTxPacket = packed record
    {Application Header}
    Number1        : Word;  //Can be any random number
    Number2        : Word;  //Must be Number1 ^ $4242
    PacketID       : Byte;  //Reserved: $00 for now
    EncryptionPad  : Byte;  //Reserved: $00 for now
    CommandID      : Byte;  //$00 = Data, $02 = Remote Command, $03 = General Purpose Memory Command, $04 = I/O Sample
    CommandOptions : Byte;  //Bit 0 : Encrypt (Reserved), Bit 1 : Request Packet ACK, Bits 2..7 : (Reserved)
    {Command Data}
    FrameID        : Byte;  //1
    ConfigOptions  : Byte;  //0 = Queue command only; must follow with AC command to apply changes, 2 = Apply Changes immediately
    ATCommand      : Word;  //2-ASCII_character AT command
    { TODO : Does Tx need parameter sized to 20 elements?  or 21 (for null-termination)? }
    ParameterValue : array[0..63+1] of Byte;  //[Array] If present, indicates the value to set in the given command. If no characters present, command is queried.
  end;

  {Define XBee Reception Packet - the format of packets transmitted from XBee to networked host}
  PxbRxPacket = ^TxbRxPacket;
  TxbRxPacket = packed record
    {Application Header}
    Number1        : Word;  //Can be any random number (copied from command packet if this is a response)
    Number2        : Word;  //Must be Number1 ^ $4242 (copied from command packet if this is a response)
    PacketID       : Byte;  //Reserved: $00 for now
    EncryptionPad  : Byte;  //Reserved: $00 for now
    CommandID      : Byte;  //$80 = Response to Data, $82 = Response to Remote Command, $83 = Response to General Purpose Memory Command
    CommandOptions : Byte;  //$00 (always)
    {Command Data}
    FrameID        : Byte;  //1 (copied from command packet)
    ATCommand      : Word;  //2-ASCII_character AT command (copied from command packet)
    Status         : Byte;  //0 = OK, 1 = ERROR, 2 = Invalid Command, 3 = Invalid Parameter
    ParameterValue : array[0..63+1] of Byte;  //[Array] If present, contains the binary or ASCII-format data requested by the command packet
  end;

  TForm1 = class(TForm)
    TCPClient: TIdTCPClient;
    UDPClient: TIdUDPClient;
    Button1: TButton;
    Button2: TButton;
    IPAddr: TEdit;
    IPAddrLabel: TLabel;
    Port: TEdit;
    PortLabel: TLabel;
    IdentifyButton: TButton;
    ResetPulseButton: TButton;
    NodeID: TEdit;
    IdIOHandlerStack1: TIdIOHandlerStack;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GenerateResetSignal;
    function  GetXBee(Command: udpCommand; var Str: String; TargetIP: String = ''): Boolean; overload;
    function  GetXBee(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    function  SetXBee(Command: udpCommand; Str: String; TargetIP: String = ''): Boolean; overload;
    function  SetXBee(Command: udpCommand; Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    procedure PrepareBuffer(Command: udpCommand; Parameter: String; RequestPacketAck: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  {IP Buffer}
  TxBuf        : TIdBytes;                                          {Raw transmit packet (resized per packet)}
  RxBuf        : TIdBytes;                                          {Raw receive packet stream (fixed to MaxPacketSize)}
  RxPacketSize : Integer;                                           {Size of last actual received packet (inside of RxBuf)}
  PTxBuf       : PxbTxPacket;                                       {Pointer to structured transmit packet (shares memory space with TxBuf)}
  PRxBuf       : PxbRxPacket;                                       {Pointer to structured receive packet (shares memory space with RxBuf)}

const
  {Network Header Metrics}
  RemoteCommand  = $02;
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  pinOutLow      = $04;
  pinOutHigh     = $05;

implementation

{$R *.fmx}

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------- Event Methods ------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {Upon start up, bind UDP to 0xBEE port ($BEE) to receive all responses from XBee Wi-Fi modules
   because some features have been seen to respond only to a source port of $BEE.}
  UDPClient.BoundPort := $BEE;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button1Click(Sender: TObject);
//var
//  IP : TIDStack;
begin
  SetXBee(udpRES, pinOutHigh);
//  IP := TIDStack.Create;
//  IP.NewInstance;
//  IPAddr.Text := IP.LocalAddress;
//  IP.Free;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetXBee(udpRES, pinOutLow);
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.IdentifyButtonClick(Sender: TObject);
begin
  IPAddr.Text := '....';
{ TODO : Finish redoing IdentifyButtonClick }
//  if SendUDP(udpGetIP, False, '192.168.1.255') then
//    begin
//    IPAddr.Text := inttostr(PRxBuf.ParameterValue[0]) + '.' + inttostr(PRxBuf.ParameterValue[1]) + '.' + inttostr(PRxBuf.ParameterValue[2]) + '.' + inttostr(PRxBuf.ParameterValue[3]);
//    if SendUDP(udpNodeIdentifier) then NodeID.Text := StrPas(PAnsiChar(@PRxBuf.ParameterValue[0]));
//    if SendUDP(udpPort) then Port.Text := inttostr(PRxBuf.ParameterValue[0] shl 8 + PRxBuf.ParameterValue[1]);
//    end
//  else
//    IPAddr.Text := '?.?.?.?';
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.ResetPulseButtonClick(Sender: TObject);
begin
  GenerateResetSignal;
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.GenerateResetSignal;
{Generate Reset Pulse}
var
  Value : Cardinal;
begin
  if GetXBee(udpOutputMask, Value) then
    if (Value = $7FFF) or SetXBee(udpOutputMask, $7FFF) then                              {Ensure output mask is proper (default, in this case)}
      if GetXBee(udpIO2Timer, Value) then
        if (Value = 1) or SetXBee(udpIO2Timer, 1) then                                    {Ensure DIO2's timer is set to 100 ms}
          SetXBee(udpOutputState, $0000);                                                 {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.GetXBee(Command: udpCommand; var Str: String; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute string (Str) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be see in PRxBuf.}
begin
{ TODO : Test GetXBee String well! }
  Str := '';
  Result := SetXBee(Command, '', TargetIP);
  if Result then Str := StrPas(PAnsiChar(@PRxBuf.ParameterValue[0]));
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.GetXBee(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute value (Num) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be see in PRxBuf.}
var
  Idx : Cardinal;
begin
  Num := 0;
  Result := SetXBee(Command, '', TargetIP);
  if Result then
    for Idx := 0 to RxPacketSize-(sizeof(TxbRxPacket)-sizeof(PRxBuf.ParameterValue))-1 do Num := Num shl 8 + PRxBuf.ParameterValue[Idx];
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SetXBee(Command: udpCommand; Str: String; TargetIP: String = ''): Boolean;
{Set XBee attribute to string (Str) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be see in PRxBuf.}
var
  IP            : String;
  RequiredRx    : Cardinal;         {bit 0 = Packet ACK received, bit 1 = Command response received, bit 2:3 = $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter}
  NeedPacketAck : Boolean;          {True = need to request packet acknowledgement, False = no need for packet acknowledgement}

    {----------------}

    function IsXBeeResponse: Boolean;
    {Return true if UDP packet is an XBee Wi-Fi response packet (Number2 is Number1 ^ $4242)}
    begin
      Result := (PRxBuf.Number1 xor $4242 = PRxBuf.Number2);
    end;

    {----------------}

    function UDPResponse(Timeout: Integer): Boolean;
    {Check for UDP packet for up to Timeout ms.  Return True/False (packet received) and store packet size in Count.}
    begin
      RxPacketSize := UDPClient.ReceiveBuffer(RxBuf, Timeout);
      Result := RxPacketSize > 0;
    end;

    {----------------}

begin
{ TODO : Test SetXBee String well! }
  self.Caption := 'Transmitting...';
  Result := False;                                                                                         {Initialize result to false}
  RequiredRx := 0;                                                                                         {Initialize required reception to none}
  { TODO : Make NeedPacketAct "right." }
  NeedPacketAck := True;
  PrepareBuffer(Command, Str, NeedPacketAck);                                                              {Prepare command packet}
  try
    {Try to transmit; IP exceptions handled}
    UDPClient.SendBuffer(ifthen(TargetIP <> '', TargetIP, IPAddr.Text), $BEE, TxBuf);                      {Send to TargetIP or GUI-displayed IP}
    while (RequiredRx < ($2 + ord(NeedPacketAck))) and UDPResponse(2000) do
      begin {Process each UDP packet received}
      if IsXBeeResponse then
        begin {It's an XBee Response packet}
        if (PRxBuf.CommandID = $80) then RequiredRx := RequiredRx or $1;                                   {Note that we received XBee Wi-Fi UDP ACK packet}
        if (PRxBuf.CommandID = (PTxBuf.CommandID or $80)) and (PRxBuf.ATCommand = PTxBuf.ATCommand) then
          RequiredRx := RequiredRx or $2 or (PRxBuf.Status shl 2);                                         {Note that we received XBee Wi-Fi UDP command response packet}
        end;
      end;
    case RequiredRx and $3 of
      0 : self.Caption := 'No Response';
      1 : self.Caption := 'Only Ack received';
      2 : self.Caption := 'Only Response received';
      3 : self.Caption := 'Both Ack and Response received';
    end;
    if RequiredRx and $3 > 0 then
      case (RequiredRx shr 2) and $3 of
        0 : self.Caption := self.Caption + ' - Okay';
        1 : self.Caption := self.Caption + ' - Error';
        2 : self.Caption := self.Caption + ' - Invalid Command';
        3 : self.Caption := self.Caption + ' - Invalid Parameter';
      end;
    Result := RequiredRx = ($2 + ord(NeedPacketAck));
  except
    {Handle known exceptions}
    on E: EIdSocketError do
      begin
      if E.LastError = 10013 then
        showmessage('Communication Error: Invalid IP Address')
      else
        showmessage('Communication Error: ' + #$D#$A +E.Message);
      end;
  end;
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SetXBee(Command: udpCommand; Num: Cardinal; TargetIP: String = ''): Boolean;
{Set XBee attribute to value (Str) and return True if successful; False otherwise.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be see in PRxBuf.}
begin
  Result := SetXBee(Command, inttostr(Num), TargetIP);
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.PrepareBuffer(Command: udpCommand; Parameter: String; RequestPacketAck: Boolean);
{Resize and fill TxBuf (pointed to by PTxBuf) with Command, Parameter, and RequestPacketAck attributes.}
var
  ParamLength : Cardinal;
  ParamValue  : Cardinal;
  Temp        : Cardinal;
begin
  ParamLength := 0;
  if Command in udpStrCommands then                                                    {If command is a string-type command}
    ParamLength := Length(Parameter)                                                   {  note length in bytes/characters}
  else
    begin                                                                              {Else}
    if Parameter <> '' then                                                            {  if parameter not empty}
      begin
      Temp := strtoint(Parameter);                                                     {    convert Parameter string to value, and...}
      ParamValue := 0;
      repeat
        Inc(ParamLength);                                                              {    note length in bytes of numeric value (1..4) and }
        ParamValue := (ParamValue shl 8) + (Temp and $FF);                             {    rearrange for big-endian storage}
        Temp := Temp shr 8;
      until Temp = 0;
      end;
    end;
  SetLength(TxBuf, 8+1+1+2+ParamLength);                                               {Size TxBuf for Application Header (8 bytes) plus FrameID (1) plus ConfigOptions (1) + ATCommand (2) + ParameterValue (0..19)}
  PTxBuf := PxbTxPacket(@TxBuf[0]);                                                    {Point PTxBuf at TxBuf}
  PTxBuf.Number1 := Random($FFFF);                                                     {Randomize Number1 ID}
  PTxBuf.Number2 := PTxBuf.Number1 xor $4242;                                          {Prepare Number2 to match requirements}
  PTxBuf.PacketID := 0;                                                                {PacketID (always 0)}
  PTxBuf.EncryptionPad := 0;                                                           {EncryptionPad (always 0)}
  PTxBuf.CommandID := RemoteCommand;                                                   {Set to be remote command}
  PTxBuf.CommandOptions := ord(RequestPacketAck)*$02;                                  {Set to request (or not request) acknowledgement}
  PTxBuf.FrameID := FrameID;                                                           {Set Frame ID}
  PTxBuf.ConfigOptions := ApplyCommand;                                                {Set to apply the command}
  PTxBuf.ATCommand := ATCmd[Command];                                                  {Set AT Command}
  if True then
  if Parameter <> '' then                                                              {If Parameter not empty}
    if Command in udpStrCommands then
      Move(Parameter, PTxBuf.ParameterValue[0], ParamLength)                           {  append Parameter as a string}
    else
      Move(ParamValue, PTxBuf.ParameterValue[0], ParamLength);                         {  or as a numeric value}
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  Randomize;                                     {Initializ the random seed}
  SetLength(RxBuf, 1500);                        {Set receive buffer length}
  PRxBuf := PxbRxPacket(@RxBuf[0]);              {Point PRxBuf at RxBuf}

Finalization
  SetLength(RxBuf, 0);

end.
