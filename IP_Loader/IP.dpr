program IP;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  XBeeWiFi in 'XBeeWiFi.pas',
  Timer in 'Timer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
