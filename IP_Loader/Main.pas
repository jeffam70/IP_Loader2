unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack,
  FMX.ListBox;

type
  {Define XBee WiFi's udp commands}
  {IMPORTANT: Do not rearrange, append, or delete from this list without similarly modifying the ATCmd constant array}
  udpCommand = (udpMacHigh, udpMacLow, udpSSID, udpIPAddr, udpIPMask, udpIPGateway, udpIPPort, udpNodeID, udpMaxRFPayload, udpRES, udpOutputMask,
  udpOutputState, udpIO2Timer, udpSerialBaud, udpSerialParity, udpSerialStopBits);

const
  {Define XBee WiFi's AT commands}
  ATCmd : array[low(udpCommand)..high(udpCommand)] of Word =
    ({NOTES: [R] - read only, [R/W] = read/write, [s] - string, [b] - binary number, [sb] - string or binary number}
    {udpMacHigh}         Byte('S') + Byte('H') shl 8,  {[Rb] XBee's Mac Address (highest 16-bits)}
    {udpMacLow}          Byte('S') + Byte('L') shl 8,  {[Rb] XBee's Mac Address (lowest 32-bits)}
    {udpSSID}            Byte('I') + Byte('D') shl 8,  {[Rs/Ws] SSID (0 to 31 printable ASCII characters)}
    {udpIPAddr}          Byte('M') + Byte('Y') shl 8,  {[Rb*/Wsb] XBee's IP Address (32-bits); *Read-only in DHCP mode}
    {udpIPMask}          Byte('M') + Byte('K') shl 8,  {[Rb*/Wsb] XBee's IP Mask (32-bits); *Read-only in DHCP mode}
    {udpIPGateway}       Byte('G') + Byte('W') shl 8,  {[Rb*/Wsb] XBee's IP Gateway (32-bits); *Read-only in DHCP mode}
    {udpIPPort}          Byte('C') + Byte('0') shl 8,  {[Rb/Wb] Xbee's UDP/IP Port (16-bits)}
    {udpNodeID}          Byte('N') + Byte('I') shl 8,  {[Rs/Ws] Friendly node identifier string (20 printable ASCII characters)}
    {udpMaxRFPayload}    Byte('N') + Byte('P') shl 8,  {[Rb] Maximum RF Payload (16-bits; in bytes)}
    {udpIO2State}        Byte('D') + Byte('2') shl 8,  {[Rb/Wb] Designated reset pin (3-bits; 0=Disabled, 1=SPI_CLK, 2=Analog input, 3=Digital input, 4=Digital output, 5=Digital output)}
    {udpOutputMask}      Byte('O') + Byte('M') shl 8,  {[Rb/Wb] Output mask for all I/O pins (each 1=output pin, each 0=input pin) (15-bits on TH, 20-bits on SMT)}
    {udpOutputState}     Byte('I') + Byte('O') shl 8,  {[Rb/Wb] Output state for all I/O pins (each 1=high, each 0=low) (15-bits on TH, 20-bits on SMT).  Period affected by updIO2Timer}
    {udpIO2Timer}        Byte('T') + Byte('2') shl 8,  {[Rb/Wb] I/O 2 state timer (100 ms units; $0..$1770)}
    {udpSerialBaud}      Byte('B') + Byte('D') shl 8,  {[Rb/Wb] serial baud rate ($1=2400, $2=4800, $3=9600, $4=19200, $5=38400, $6=57600, $7=115200, $8=230400, $9=460800, $A=921600)}
    {udpSerialParity}    Byte('N') + Byte('B') shl 8,  {[Rb/Wb] serial parity ($0=none, $1=even, $2=odd)}
    {udpSerialStopBits}  Byte('S') + Byte('B') shl 8   {[Rb/Wb] serial stop bits ($0=one stop bit, $1=two stop bits)}
    );

  {Set of UDP Commands that take string parameters (instead of numeric parameters)}
  udpStrCommands : set of udpCommand = [udpNodeID];

type
  {Define Parameter Value}
  TParamValue = array[0..63+1] of Byte;

  {Define XBee Transmission Packet - the format of packets transmitted from networked host to XBee}
  PxbTxPacket = ^TxbTxPacket;
  TxbTxPacket = packed record
    {Application Header}
    Number1        : Word;         //Can be any random number
    Number2        : Word;         //Must be Number1 ^ $4242
    PacketID       : Byte;         //Reserved: $00 for now
    EncryptionPad  : Byte;         //Reserved: $00 for now
    CommandID      : Byte;         //$00 = Data, $02 = Remote Command, $03 = General Purpose Memory Command, $04 = I/O Sample
    CommandOptions : Byte;         //Bit 0 : Encrypt (Reserved), Bit 1 : Request Packet ACK, Bits 2..7 : (Reserved)
    {Command Data}
    FrameID        : Byte;         //1
    ConfigOptions  : Byte;         //0 = Queue command only; must follow with AC command to apply changes, 2 = Apply Changes immediately
    ATCommand      : Word;         //2-ASCII_character AT command
    ParameterValue : TParamValue;  //[Array] (if present) is value to set in the given command, otherwise, command is queried.
  end;

  {Define XBee Reception Packet - the format of packets transmitted from XBee to networked host}
  PxbRxPacket = ^TxbRxPacket;
  TxbRxPacket = packed record
    {Application Header}
    Number1        : Word;         //Can be any random number (copied from command packet if this is a response)
    Number2        : Word;         //Must be Number1 ^ $4242 (copied from command packet if this is a response)
    PacketID       : Byte;         //Reserved: $00 for now
    EncryptionPad  : Byte;         //Reserved: $00 for now
    CommandID      : Byte;         //$80 = Response to Data, $82 = Response to Remote Command, $83 = Response to General Purpose Memory Command
    CommandOptions : Byte;         //$00 (always)
    {Command Data}
    FrameID        : Byte;         //1 (copied from command packet)
    ATCommand      : Word;         //2-ASCII_character AT command (copied from command packet)
    Status         : Byte;         //0 = OK, 1 = ERROR, 2 = Invalid Command, 3 = Invalid Parameter
    ParameterValue : TParamValue;  //[Array] If present, contains the binary or ASCII-format data requested by the command packet
  end;

  {Define response; used to record or more responses (ParameterValues) from incoming response packets}
  TResponse = record
    Length : Cardinal;
    Status : Byte;
    Data   : TParamValue;
  end;

  {Define simple string list}
  TSimpleStringList = array of String;

  {Define simple number list}
  TSimpleNumberList = array of Cardinal;

  {Define XBee Info record}
  PXBee = ^ TXBee;
  TXBee = record
    PCPort      : String;                     {Pseudo-Communication Port (derived from MacAddr}
    IPAddr      : String;                     {IP Address}
    IPPort      : Cardinal;                   {IP Port}
    MacAddrHigh : Cardinal;                   {Upper 16 bits of MAC address}
    MacAddrLow  : Cardinal;                   {Lower 32 bits of MAC address}
    NodeID      : String;                     {Friendly Node ID}
  end;

  TForm1 = class(TForm)
    TCPClient: TIdTCPClient;
    UDPClient: TIdUDPClient;
    Button1: TButton;
    Button2: TButton;
    PCPortLabel: TLabel;
    IPPort: TEdit;
    PortLabel: TLabel;
    IdentifyButton: TButton;
    ResetPulseButton: TButton;
    NodeID: TEdit;
    IdIOHandlerStack1: TIdIOHandlerStack;
    PCPortCombo: TComboBox;
    IPAddr: TEdit;
    MACAddr: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PCPortComboChange(Sender: TObject);
  private
    { Private declarations }
    procedure GenerateResetSignal;
    function  GetXBee(Command: udpCommand; var Str: String; TargetIP: String = ''): Boolean; overload;
    function  GetXBee(Command: udpCommand; var StrList: TSimpleStringList; TargetIP: String = ''): Boolean; overload;
    function  GetXBee(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    function  GetXBee(Command: udpCommand; var NumList: TSimpleNumberList; TargetIP: String = ''): Boolean; overload;
    function  SetXBee(Command: udpCommand; Str: String; TargetIP: String = ''; ExpectMultiple: Boolean = False): Boolean; overload;
    function  SetXBee(Command: udpCommand; Num: Cardinal; TargetIP: String = ''): Boolean; overload;
    procedure PrepareBuffer(Command: udpCommand; Parameter: String; RequestPacketAck: Boolean);
    function FormatIPAddr(IPAddr: Cardinal): String;
    function FormatMACAddr(MacAddrHigh, MacAddrLow: Cardinal): String;
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
  ResponseList : array of TResponse;                                {Dynamic array of received responses (from packet's parameter field); multiple XBee may respond to broadcast message}
  XBeeList     : array of TXBee;                                    {Dynamic array of XBee Info records}

const
  {Network Header Metrics}
  RemoteCommand  = $02;
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  pinOutLow      = $04;
  pinOutHigh     = $05;
  {Misc}
  Timeout        = 1000;

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
var
  Idx  : Cardinal;
  Nums : TSimpleNumberList;
  PXB   : PXBee;
begin
  PCPortCombo.Clear;
  SetLength(XBeeList, 0);
  MacAddr.Text := '';
  IPAddr.Text := '';
  IPPort.Text := '';
  NodeID.Text := '';
{ TODO : Harden IdentifyButtonClick for interim errors.  Handle gracefully. }
  if GetXBee(udpIPAddr, Nums, '192.168.1.255') then
    begin
    for Idx := 0 to High(Nums) do
      begin
      SetLength(XBeeList, Length(XBeeList)+1);
      PXB := @XBeeList[High(XBeeList)];
      PXB.IPAddr := FormatIPAddr(Nums[Idx]);
      if GetXBee(udpIPPort, PXB.IPPort, PXB.IPAddr) then
        if GetXBee(udpMacHigh, PXB.MacAddrHigh, PXB.IPAddr) then
          if GetXBee(udpMacLow, PXB.MacAddrLow, PXB.IPAddr) then
            if GetXBee(udpNodeID, PXB.NodeID, PXB.IPAddr) then
              begin
              PXB.PCPort := 'XB' + rightstr(inttostr(PXB.MacAddrLow), 2);
              PCPortCombo.Items.AddObject(PXB.PCPort, TObject(PXB));
              end;
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.ResetPulseButtonClick(Sender: TObject);
begin
  GenerateResetSignal;
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.PCPortComboChange(Sender: TObject);
begin
  IPAddr.Text := XBeeList[PCPortCombo.Selected.Index].IPAddr;
  IPPort.Text := inttostr(XBeeList[PCPortCombo.Selected.Index].IPPort);
  NodeID.Text := XBeeList[PCPortCombo.Selected.Index].NodeID;
  MacAddr.Text := FormatMACAddr(XBeeList[PCPortCombo.Selected.Index].MacAddrHigh, XBeeList[PCPortCombo.Selected.Index].MacAddrLow);
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
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
begin
  Str := '';
  Result := SetXBee(Command, '', TargetIP);                                               {Send packet}
  if Result then Str := StrPas(PAnsiChar(@ResponseList[0].Data));                         {Copy data to Str}
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.GetXBee(Command: udpCommand; var StrList: TSimpleStringList; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute strings (StrList) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a string in Str, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
{ TODO : Test GetXBee StrList }
  SetLength(StrList, 0);
  Result := SetXBee(Command, '', TargetIP, True);                                         {Send packet}
  if Result then                                                                          {If response received}
    begin                                                                                 {Copy data to StrList}
    for Idx := 0 to High(ResponseList) do
      begin
      SetLength(StrList, Length(StrList)+1);
      StrList[High(StrList)] := StrPas(PAnsiChar(@ResponseList[0].Data));
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.GetXBee(Command: udpCommand; var Num: Cardinal; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute value (Num) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  Idx : Cardinal;
begin
  Num := 0;
  Result := SetXBee(Command, '', TargetIP);
  if Result then for Idx := 0 to ResponseList[0].Length-1 do Num := Num shl 8 + ResponseList[0].Data[Idx];
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.GetXBee(Command: udpCommand; var NumList: TSimpleNumberList; TargetIP: String = ''): Boolean;
{Retrieve XBee attribute values (NumList) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 Though it parses and returns the response as a cardinal in Num, the full response packet can be seen in PRxBuf.}
var
  RIdx : Cardinal;
  NIdx : Cardinal;
begin
  SetLength(NumList, 0);
  Result := SetXBee(Command, '', TargetIP, True);                                         {Send packet}
  if Result then                                                                          {If response received}
    begin                                                                                 {Copy data to NumList}
    for RIdx := 0 to High(ResponseList) do
      begin
      SetLength(NumList, Length(NumList)+1);
      NumList[High(NumList)] := 0;
      for NIdx := 0 to ResponseList[RIdx].Length-1 do
        NumList[High(NumList)] := NumList[High(NumList)] shl 8 + ResponseList[RIdx].Data[NIdx];
      end;
    end;
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SetXBee(Command: udpCommand; Str: String; TargetIP: String = ''; ExpectMultiple: Boolean = False): Boolean;
{Set XBee attribute to string (Str) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.  Set ExpectMultiple true if multiple responses possible, such as
 when a broadcast packet is being transmitted.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
var
  RLIdx         : Integer;          // response list index
  IP            : String;
  RequiredRx    : Cardinal;         // bit 0 = Packet ACK received, bit 1 = Command response received
  Status        : Cardinal;         // $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter
const
  {Required Rx codes}
  PacketAck  = $01;                     {RequiredRx's code for packet ACK received}
  CommandRsp = $02;                     {RequiredRx's code for command response received}
  GotItAll   = PacketAck + CommandRsp;  {Code for all required receipts}
  MuliRsp    = $04;                     {Code for multiple responses possible}

    {----------------}

    function UDPResponse(Timeout: Integer): Boolean;
    {Check for UDP packet for up to Timeout ms.  Return True/False (packet received) and store packet size in Count.}
    begin
      RxPacketSize := UDPClient.ReceiveBuffer(RxBuf, Timeout);
      Result := RxPacketSize > 0;
    end;

    {----------------}

begin
{ TODO : Remove dugging Info }
  self.Caption := 'Transmitting...';
  Result := False;                                                                                         {Initialize result to false}
  RequiredRx := 0;                                                                                         {Initialize required reception to none}
  Status := 0;                                                                                             {Initialize status}
  RLIdx := -1;                                                                                             {Clear response list index}
  SetLength(ResponseList, 0);                                                                              {Clear the response list}
  PrepareBuffer(Command, Str, True);                                                                       {Prepare command packet}
  try
    {Try to transmit; IP exceptions handled}
    UDPClient.SendBuffer(ifthen(TargetIP <> '', TargetIP, IPAddr.Text), $BEE, TxBuf);                      {Send to TargetIP or GUI-displayed IP}
    {Transmitted fine, retrieve}
    while (RequiredRx < GotItAll+ord(ExpectMultiple)*MuliRsp) and UDPResponse(Timeout) do                  {For every UDP Packet received (until we've received the expected packets)}
      begin {Process each UDP packet received}
      if PRxBuf.Number1 xor $4242 = PRxBuf.Number2 then                                                    {if packet is an XBee Wi-Fi response packet (Number2 is Number1 ^ $4242)}
        begin {It's an XBee Response packet}
        if (PRxBuf.CommandID = $80) then RequiredRx := RequiredRx or PacketAck;                            {Note when we received XBee Wi-Fi UDP ACK packet}
        if (PRxBuf.CommandID = (PTxBuf.CommandID or $80)) and (PRxBuf.ATCommand = PTxBuf.ATCommand) then
          begin
          RequiredRx := RequiredRx or CommandRsp or (PRxBuf.Status shl 2);                                 {Note when we received XBee Wi-Fi UDP command response packet}
          Status := Status or PRxBuf.Status;
          inc(RLIdx);
          SetLength(ResponseList, RLIdx+1);                                                                {Make room for response}
          ResponseList[RLIdx].Length := RxPacketSize-(sizeof(TxbRxPacket)-sizeof(PRxBuf.ParameterValue));  {Note response length}
          ResponseList[RLIdx].Status := PRxBuf.Status;                                                     {Note response status}
          Move(PRxBuf.ParameterValue, ResponseList[RLIdx].Data, ResponseList[RLIdx].Length);               {Save response data}
          ResponseList[RLIdx].Data[ResponseList[RLIdx].Length] := 0;                                       {Null-terminate (in case it's a string)}
          end;
        end;
      end;
    case RequiredRx of
      0 : self.Caption := 'No Response';
      1 : self.Caption := 'Only Ack received';
      2 : self.Caption := 'Only Response received';
      3 : self.Caption := 'Both Ack and Response received';
    end;
    case Status of
      0 : self.Caption := self.Caption + ' - Okay';
      1 : self.Caption := self.Caption + ' - Error';
      2 : self.Caption := self.Caption + ' - Invalid Command';
      3 : self.Caption := self.Caption + ' - Invalid Parameter';
    end;
    Result := (RequiredRx = PacketAck + CommandRsp) and (Status = 0);
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
{Set XBee attribute to value (Num) and return True if successful; False otherwise.
 Set TargetIP if other than IPAddr.Text should be used.
 This method creates and sends a UDP packet, and checks and validates the response (if any).
 The full response packet can be seen in PRxBuf.}
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

function TForm1.FormatIPAddr(IPAddr: Cardinal): String;
{Return IP Address in standard string format}
begin
  Result := Format('%d.%d.%d.%d', [IPAddr shr 24 and $FF, IPAddr shr 16 and $FF, IPAddr shr 8 and $FF, IPAddr and $FF]);
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.FormatMACAddr(MacAddrHigh, MacAddrLow: Cardinal): String;
{Return MAC Address (48-bit number in MacAddrHigh:MaccAddrLow) in standard string format}
var
  Idx : Cardinal;
begin
  Result := Format('%.2x:%.2x:%.2x:%.2x:%.2x:%.2x', [MacAddrHigh shr 8 and $FF, MacAddrHigh and $FF, MacAddrLow shr 24 and $FF, MacAddrLow shr 16 and $FF, MacAddrLow shr 8 and $FF, MacAddrLow and $FF]);
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  Randomize;                                     {Initialize the random seed}
  SetLength(RxBuf, 1500);                        {Set receive buffer length}
  PRxBuf := PxbRxPacket(@RxBuf[0]);              {Point PRxBuf at RxBuf}

Finalization
  SetLength(RxBuf, 0);

end.
