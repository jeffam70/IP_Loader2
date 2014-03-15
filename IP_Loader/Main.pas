unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, IdUDPBase, IdUDPClient, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdGlobal, FMX.Edit;

type
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
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  Buffer : TIdBytes;
const
  LEDOn : array[0..12] of byte = ( $42, $42, $00, $00, $00, $00, $02, $00, $01, $02, $44, $32, $05 );
begin
  SetLength(Buffer, Length(LEDOn));
  Move(LEDOn[0], Buffer[0], Length(LEDOn));
  UDPClient.SendBuffer(IPAddr.Text, $BEE, Buffer);
  SetLength(Buffer, 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LEDOff : TIdBytes;
begin
  SetLength(LEDOff, 13);
  LEDOff[00] := $42;
  LEDOff[01] := $42;
  LEDOff[02] := $00;
  LEDOff[03] := $00;
  LEDOff[04] := $00;
  LEDOff[05] := $00;
  LEDOff[06] := $02;
  LEDOff[07] := $00;
  LEDOff[08] := $01;
  LEDOff[09] := $02;
  LEDOff[10] := $44;
  LEDOff[11] := $32;
  LEDOff[12] := $04;

//  Buffer[0] := Byte($42);
//  strpcopy(Buffer[0], chr($42)+chr($42)+chr($00)+chr($00)+chr($00)+chr($02));
  UDPClient.SendBuffer(IPAddr.Text, $BEE, LEDOff);

  SetLength(LEDOff, 0);
end;

end.
