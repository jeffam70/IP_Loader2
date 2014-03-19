unit Main;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Edit,
  IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal, IdStack;

type
  udpCommand = (udpRESLow, udpRESHigh, udpGetIP, udpOutputMask, udpDIO2RPulse, udpDIO2Timer, udpNodeIdentifier, udpPort);

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
    ParameterValue : array[0..19] of Byte;  //[Array] If present, indicates the value to set in the given command. If no characters present, command is queried.
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
    ParameterValue : array[0..19+1] of Byte;  //[Array] If present, contains the binary or ASCII-format data requested by the command packet
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdentifyButtonClick(Sender: TObject);
    procedure ResetPulseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure PrepareBuffer(Command: udpCommand; RequestPacketAck: Boolean);
    function  SendUDP(Command: udpCommand; RequestPacketAck: Boolean = True; TargetIP: String = ''): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  {IP Buffer}
  TxBuf  : TIdBytes;                                          {Raw transmit packet (resized per packet)}
  RxBuf  : TIdBytes;                                          {Raw receive packet stream (fixed size)}
  PTxBuf : PxbTxPacket;                                       {Pointer to structured transmit packet (shares memory space with TxBuf)}
  PRxBuf : PxbRxPacket;                                       {Pointer to structured receive packet (shares memory space with RxBuf)}

const
  {Network Header Metrics}
  RemoteCommand  = $02;
  RequestAckNack : array[false..true] of byte = ($00, $02);
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  DigitalOutLow  = $04;
  DigitalOutHigh = $05;
  {End of packet marker}
  NULL           = $00;

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
begin
  SendUDP(udpRESHigh);
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.Button2Click(Sender: TObject);
begin
  SendUDP(udpRESLow);
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.IdentifyButtonClick(Sender: TObject);
begin
  IPAddr.Text := '....';
  if SendUDP(udpGetIP, False, '192.168.1.255') then
    begin
    IPAddr.Text := inttostr(PRxBuf.ParameterValue[0]) + '.' + inttostr(PRxBuf.ParameterValue[1]) + '.' + inttostr(PRxBuf.ParameterValue[2]) + '.' + inttostr(PRxBuf.ParameterValue[3]);
    if SendUDP(udpNodeIdentifier) then NodeID.Text := StrPas(PAnsiChar(@PRxBuf.ParameterValue[0]));
    if SendUDP(udpPort) then Port.Text := inttostr(PRxBuf.ParameterValue[0] shl 8 + PRxBuf.ParameterValue[1]);
    end
  else
    IPAddr.Text := '?.?.?.?';
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.ResetPulseButtonClick(Sender: TObject);
begin
  if SendUDP(udpOutputMask) then    {Ensure output mask is proper (default, in this case)}
    if SendUDP(udpDIO2Timer) then   {Ensure DIO2's timer is set to 100 ms}
      SendUDP(udpDIO2RPulse);       {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.PrepareBuffer(Command: udpCommand; RequestPacketAck: Boolean);
const
  CmdStream : array[low(udpCommand)..high(udpCommand), 0..6] of byte =
    (
    {udpRESLow}          ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutLow, NULL ),
    {udpRESHigh}         ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutHigh, NULL ),
    {udpGetIP}           ( $04, FrameID, ApplyCommand, Byte('M'), Byte('Y'), NULL, NULL ),
    {udpOutputMask}      ( $06, FrameID, ApplyCommand, Byte('O'), Byte('M'), $7F, $FF ),  {Set output mask to allow all pins to be active}
    {udpDIO2RPulse}      ( $06, FrameID, ApplyCommand, Byte('I'), Byte('O'), $00, $00 ),  {Set DIO2 to low (for DIO2Timer*100ms period)}
    {udpDIO2Timer}       ( $06, FrameID, ApplyCommand, Byte('T'), Byte('2'), $00, $01 ),
    {udpNodeIdentifier}  ( $04, FrameID, ApplyCommand, Byte('N'), Byte('I'), NULL, NULL ),
    {udpPort}            ( $04, FrameID, ApplyCommand, Byte('C'), Byte('0'), NULL, NULL )//,
//    {udpApplyChanges}    ( $04, FrameID, ApplyCommand, Byte('A'), Byte('C'), NULL, NULL )
    );
begin
  SetLength(TxBuf, 8+CmdStream[Command][0]);                                           {Size TxBuf for Application Header (8 bytes) plus command length}
  FillChar(TxBuf[0], 8, 0);                                                            {Clear Application Header}
  PTxBuf := PxbTxPacket(@TxBuf[0]);                                                    {Point PTxBuf at TxBuf}
  PTxBuf.Number1 := Random($FFFF);                                                     {Randomize Number1 ID}
  PTxBuf.Number2 := PTxBuf.Number1 xor $4242;                                          {Prepare Number2 to match requirements}
  PTxBuf.CommandID := RemoteCommand;                                                   {Set to be remote command}
  PTxBuf.CommandOptions := RequestAckNack[RequestPacketAck];                           {Set to request (or not request) acknowledgement}
  Move(CmdStream[Command][1], TxBuf[8], CmdStream[Command][0]);                        {Append command stream}
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SendUDP(Command: udpCommand; RequestPacketAck: Boolean = True; TargetIP: String = ''): Boolean;
{Create and send UDP packet, check and validate response (if any) and return True if successful and response in RxBuf;
 returns false otherwise.}
var
  IP         : String;
  Count      : Integer;
  RequiredRx : Cardinal;         {bit 0 = Packet ACK received, bit 1 = Command response received, bit 2:3 = $0-OK, $1-Error, $2=Invalid command, $3=Invalid parameter}

    {----------------}

    function IsXBeeResponse: Boolean;
    {Return true if UDP packet is an XBee Wi-Fi response packet (Number2 is Number1 ^ $4242)}
    begin
      Result := PRxBuf.Number1 xor $4242 = PRxBuf.Number2;
    end;

    {----------------}

    function UDPResponse(Timeout: Integer): Boolean;
    {Check for UDP packet for up to Timeout ms.  Return True/False (packet received) and store packet size in Count.}
    begin
      Count := UDPClient.ReceiveBuffer(RxBuf, Timeout);
      Result := Count > 0;
    end;

    {----------------}

begin
  self.Caption := 'Transmitting...';
  Result := False;                                                                                         {Initialize result to false}
  RequiredRx := 0;                                                                                         {Initialize required reception to none}
  PrepareBuffer(Command, RequestPacketAck);                                                                {Prepare command packet}
  try
    {Try to transmit; IP exceptions handled}
    UDPClient.SendBuffer(ifthen(TargetIP <> '', TargetIP, IPAddr.Text), $BEE, TxBuf);                      {Send to TargetIP or GUI-displayed IP}
    while (RequiredRx < ($2 + ord(RequestPacketAck))) and UDPResponse(2000) do
      begin {Process each UDP packet received}
      if IsXBeeResponse then
        begin {It's an XBee Response packet}
        if (PRxBuf.CommandID = $80) then RequiredRx := RequiredRx or $1;                                   {Received XBee Wi-Fi UDP ACK packet}
        if (PRxBuf.CommandID = (PTxBuf.CommandID or $80)) and (PRxBuf.ATCommand = PTxBuf.ATCommand) then   {Received XBee Wi-Fi UDP command response packet}
          RequiredRx := RequiredRx or $2 or (PRxBuf.Status shl 2);
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
    Result := RequiredRx = ($2 + ord(RequestPacketAck));
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

Initialization
  Randomize;                                     {Initializ the random seed}
  SetLength(RxBuf, 1500);                        {Set receive buffer length}
  PRxBuf := PxbRxPacket(@RxBuf[0]);              {Point PRxBuf at RxBuf}

Finalization
  SetLength(RxBuf, 0);

end.
