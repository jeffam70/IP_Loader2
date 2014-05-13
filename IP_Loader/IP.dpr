program IP;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  XBeeWiFi in 'XBeeWiFi.pas',
  Time in 'Time.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
