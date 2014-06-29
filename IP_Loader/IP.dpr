program IP;

uses
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  XBeeWiFi in 'XBeeWiFi.pas',
  Time in 'Time.pas',
  Advanced in 'Advanced.pas' {AdvancedSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAdvancedSearchForm, AdvancedSearchForm);
  Application.Run;
end.
