program SSHDemo;

uses
  ExceptionLog,
  Forms,
  fuMain in 'fuMain.pas' {fmMain},
  fuLogin in 'fuLogin.pas' {fmLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
