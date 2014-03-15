unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, FMX.Edit;

type
  udpCommand = (udpRESLow, udpRESHigh);

  TForm1 = class(TForm)
    TCPClient: TIdTCPClient;
    UDPClient: TIdUDPClient;
    Button1: TButton;
    Button2: TButton;
    IPAddr: TEdit;
    IPAddrLabel: TLabel;
    Port: TEdit;
    PortLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  FrameID        = Byte($01);
  QueueCommand   = $00;
  ApplyCommand   = $02;
  DigitalOutLow  = $04;
  DigitalOutHigh = $05;

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

{------ Private ------}
procedure TForm1.PrepareBuffer(Command: udpCommand);
const
  CmdStream : array[low(udpCommand)..high(udpCommand), 0..4] of byte =
    (
    {udpRESLow}  ( FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutLow ),
    {udpRESHigh} ( FrameID, ApplyCommand, Byte('D'), Byte('2'), DigitalOutHigh )
    );
begin
  SetLength(Buffer, Length(NetHeader)+Length(CmdStream[Command]));
  Move(NetHeader[0], Buffer[0], Length(NetHeader));
  Move(CmdStream[Command][0], Buffer[Length(NetHeader)], Length(CmdStream[Command]));
  UDPClient.SendBuffer(IPAddr.Text, $BEE, Buffer);
  SetLength(Buffer, 0);
end;

end.
