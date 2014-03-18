unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, FMX.Edit;

type
  udpCommand = (udpRESLow, udpRESHigh, udpGetIP);

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  TxBuf : TIdBytes;
  RxBuf : TIdBytes;

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

procedure TForm1.Button1Click(Sender: TObject);
begin
  SendUDP(udpRESHigh);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SendUDP(udpRESLow);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  SendUDP(udpGetIP, True, '192.168.1.255');
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TForm1.PrepareBuffer(Command: udpCommand; RequestPacketAck: Boolean);
const
  CmdStream : array[low(udpCommand)..high(udpCommand), 0..5] of byte =
    (
    {udpRESLow}  ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutLow ),
    {udpRESHigh} ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutHigh ),
    {udpGetIP}   ( $04, FrameID, ApplyCommand, Byte('M'), Byte('Y'), NULL )
    );
begin
  SetLength(TxBuf, Length(NetHeader)+1+CmdStream[Command][0]);
  Move(NetHeader[0], TxBuf[0], Length(NetHeader));
  TxBuf[Length(NetHeader)] := RequestAckNack[RequestPacketAck];
  Move(CmdStream[Command][1], TxBuf[Length(NetHeader)+1], CmdStream[Command][0]);
end;

{----------------------------------------------------------------------------------------------------}

function TForm1.SendUDP(Command: udpCommand; RequestPacketAck: Boolean = True; TargetIP: String = ''): Boolean;
{Create and send UDP packet, check and validate response (if any) and return True if successful and response in RxBuf;
 returns false otherwise.}
var
  IP : String;

    {----------------}

    function UDPPacketReceived: Boolean;
    {Return true if UDP packet received}
    begin
      Result := RxBuf[0] shl 24 + RxBuf[1] shl 16 + RxBuf[2] shl 8 + RxBuf[3] <> 0;
    end;

    {----------------}

    function UDPXBeeWiFiResponse: Boolean;
    {Return true if UDP packet is an XBee Wi-Fi response packet}
    begin
      Result := (RxBuf[0] shl 8 + RxBuf[1]) xor $4242 = (RxBuf[2] shl 8 + RxBuf[3]);
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
    FillChar(RxBuf[0], Length(RxBuf), 0);
    UDPClient.ReceiveBuffer(RxBuf, 2000);
    if UDPPacketReceived and UDPXBeeWiFiResponse and                                         {XBee Wi-Fi UDP response packet received}
      (RxBuf[4] = $55) and (RxBuf[5] = $01) and (RxBuf[6] = $80) then                        {and its status is good}
      self.Caption := 'Ack received'
    else
      self.Caption := 'No ack';
    end;
  if Command in [udpGetIP] then
    begin
    FillChar(RxBuf[0], Length(RxBuf), 0);
    UDPClient.ReceiveBuffer(RxBuf, 2000);
    if UDPPacketReceived and UDPXBeeWiFiResponse then                                        {XBee Wi-Fi UDP response packet received}
      self.Caption := self.Caption + ', Response received'
    else
      self.Caption := self.Caption + ', No response';
    end;
end;

{----------------------------------------------------------------------------------------------------}

Initialization
  SetLength(RxBuf, 1500);

Finalization
  SetLength(RxBuf, 0);

end.
