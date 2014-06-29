program PropellerStream;

uses
  FMX.Forms,
  Main in 'Main.pas' {PropellerStreamForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPropellerStreamForm, PropellerStreamForm);
  Application.Run;
end.
