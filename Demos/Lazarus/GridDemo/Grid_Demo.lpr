program Grid_Demo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MainFrm in 'MainFrm.pas' {Form1},
  ConnFrm in 'ConnFrm.pas' {ConnectDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TConnectDlg, ConnectDlg);
  Application.Run;
end.
