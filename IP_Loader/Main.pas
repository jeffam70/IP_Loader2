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
    procedure PrepareBuffer(Command: udpCommand);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  {IP Buffer}
  Buffer : TIdBytes;

const
  {Network Header Metrics}
  RemoteCommand  = $02;
  RequestNoACK   = $00;
  RequestACK     = $02;
  {Command Metrics}
  FrameID        = $01;
  QueueCommand   = $00;
  ApplyCommand   = $02;
  DigitalOutLow  = $04;
  DigitalOutHigh = $05;
  {End of packet marker}
  NULL           = $00;

  {Network Header}
  NetHeader : array[0..7] of byte = ( $42, $42, $00, $00, $00, $00, RemoteCommand, RequestNoACK );


implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PrepareBuffer(udpRESHigh);
  UDPClient.SendBuffer(IPAddr.Text, $BEE, Buffer);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  PrepareBuffer(udpRESLow);
  UDPClient.SendBuffer(IPAddr.Text, $BEE, Buffer);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PrepareBuffer(udpGetIP);
  UDPClient.SendBuffer('192.168.1.255'{IPAddr.Text}, $BEE, Buffer);
  UDPClient.ReceiveBuffer(Buffer, 2000);
  IPAddr.Text := 'Hello';
end;

{------ Private ------}
procedure TForm1.PrepareBuffer(Command: udpCommand);
const
  CmdStream : array[low(udpCommand)..high(udpCommand), 0..5] of byte =
    (
    {udpRESLow}  ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutLow ),
    {udpRESHigh} ( $05, FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutHigh ),
    {udpGetIP}   ( $04, FrameID, ApplyCommand, Byte('M'), Byte('Y'), NULL )
    );
begin
  SetLength(Buffer, Length(NetHeader)+CmdStream[Command][0]);
  Move(NetHeader[0], Buffer[0], Length(NetHeader));
  Move(CmdStream[Command][1], Buffer[Length(NetHeader)], CmdStream[Command][0]);
end;

end.
