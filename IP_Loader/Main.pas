unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, FMX.Edit;

type
  udpCommand = (udpRESLow, udpRESHigh, udpGetIP, udpOutputMask, udpDIO2RPulse, udpDIO2Timer, udpApplyChanges);

  {Define XBee Transmission Packet - the format of packets transmitted from networked host to XBee}
  PxbTxPacket = ^TxbTxPacket;
  TxbTxPacket = packed record
    {Application Header}
    Number1        : Word;
    Number2        : Word;
    PacketID       : Byte;
    EncryptionPad  : Byte;
    CommandID      : Byte;
    CommandOptions : Byte;
    {Command Data}
    FrameID        : Byte;
    ConfigOptions  : Byte;
    ATCommand      : Word;
    ParameterValue : Byte;
  end;

  {Define XBee Reception Packet - the format of packets transmitted from XBee to networked host}
  PxbRxPacket = ^TxbRxPacket;
  TxbRxPacket = packed record
    {Application Header}
    Number1        : Word;
    Number2        : Word;
    PacketID       : Byte;
    EncryptionPad  : Byte;
    CommandID      : Byte;
    CommandOptions : Byte;
    {Command Data}
    FrameID        : Byte;
    ATCommand      : Word;
    Status         : Byte;
    ParameterValue : Byte;
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
    Button3: TButton;
    ResetPulseButton: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  PRxBuf : PxbRxPacket;                                       {Pointer to structured receive packet (shares memory space with RxBuf)}

const
  {Network Header Metrics}
  RemoteCommand  = $02;
//  RequestNoACK   = $00;
//  RequestACK     = $02;
  RequestAckNack : array[false..true] of byte = ($00, $02);
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  DigitalOutLow  = $04;
  DigitalOutHigh = $05;
  {End of packet marker}
  NULL           = $00;

  {Network Header}
  NetHeader : array[0..6] of byte = ( $42, $42, $00, $00, $00, $00, RemoteCommand);


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

procedure TForm1.Button3Click(Sender: TObject);
begin
  SendUDP(udpGetIP, False, '192.168.1.255');
end;

{----------------------------------------------------------------------------------------------------}

procedure TForm1.ResetPulseButtonClick(Sender: TObject);
begin
  SendUDP(udpOutputMask);   {Ensure output mask is proper (default, in this case)}
  SendUDP(udpDIO2Timer);    {Ensure DIO2's timer is set to 100 ms}
  SendUDP(udpDIO2RPulse);   {Start reset pulse}
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.PrepareBuffer(Command: udpCommand; RequestPacketAck: Boolean);
//var
//  X : PXBeeTxPacket;
const
  CmdStream : array[low(udpCommand)..high(udpCommand), 0..6] of byte =
    (
    {udpRESLow}       ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutLow, NULL ),
    {udpRESHigh}      ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutHigh, NULL ),
    {udpGetIP}        ( $04, FrameID, ApplyCommand, Byte('M'), Byte('Y'), NULL, NULL ),
    {udpOutputMask}   ( $06, FrameID, ApplyCommand, Byte('O'), Byte('M'), $7F, $FF ),  {Set output mask to allow all pins to be active}
    {udpDIO2RPulse}   ( $06, FrameID, ApplyCommand, Byte('I'), Byte('O'), $00, $00 ),  {Set DIO2 to low (for DIO2Timer*100ms period)}
    {udpDIO2Timer}    ( $06, FrameID, ApplyCommand, Byte('T'), Byte('2'), $00, $01 ),
    {udpApplyChanges} ( $04, FrameID, ApplyCommand, Byte('A'), Byte('C'), NULL, NULL )
    );
begin
  SetLength(TxBuf, Length(NetHeader)+1+CmdStream[Command][0]);
  Move(NetHeader[0], TxBuf[0], Length(NetHeader));
  TxBuf[Length(NetHeader)] := RequestAckNack[RequestPacketAck];

//  X := PXBeeTxPacket(@TxBuf[0]);
//  X.Header.Number1 := X.Header.Number1 + 1;

  Move(CmdStream[Command][1], TxBuf[Length(NetHeader)+1], CmdStream[Command][0]);
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SendUDP(Command: udpCommand; RequestPacketAck: Boolean = True; TargetIP: String = ''): Boolean;
{Create and send UDP packet, check and validate response (if any) and return True if successful and response in RxBuf;
 returns false otherwise.}
var
  IP    : String;
  Count : Integer;

    {----------------}

    function IsXBeeResponse: Boolean;
    {Return true if UDP packet is an XBee Wi-Fi response packet}
    begin
      Result := PRxBuf.Number1 xor $4242 = PRxBuf.Number2;
//      Result := (RxBuf[0] shl 8 + RxBuf[1]) xor $4242 = (RxBuf[2] shl 8 + RxBuf[3]);
    end;

    {----------------}

begin
  self.Caption := '';
  Result := False;
  if TargetIP = '' then IP := IPAddr.Text else IP := TargetIP;
  PrepareBuffer(Command, RequestPacketAck);
  UDPClient.SendBuffer(IP, $BEE, TxBuf);
  if RequestPacketAck then
    begin
//    FillChar(RxBuf[0], Length(RxBuf), 0);
    Count := UDPClient.ReceiveBuffer(RxBuf, 2000);
    if (Count > 0) and IsXBeeResponse and (PRxBuf.CommandID = $80) then        {Received XBee Wi-Fi UDP ACK packet}
      self.Caption := 'Ack received'
    else
      self.Caption := 'No ack';
    end;
  if Command in [udpGetIP] then
    begin
    Count := UDPClient.ReceiveBuffer(RxBuf, 6000);
    Count := UDPClient.ReceiveBuffer(RxBuf, 6000);
    if IsXBeeResponse then                                                     {XBee Wi-Fi UDP response packet received}
      self.Caption := self.Caption + ', Response received'
    else
      self.Caption := self.Caption + ', No response';
    end;
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  SetLength(RxBuf, 1500);
  PRxBuf := PxbRxPacket(@RxBuf[0]);

Finalization
  SetLength(RxBuf, 0);

end.
