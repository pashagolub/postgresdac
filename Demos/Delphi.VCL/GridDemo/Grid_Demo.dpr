program Grid_Demo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  ConnFrm in 'ConnFrm.pas' {ConnectDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TConnectDlg, ConnectDlg);
  Application.Run;
end.
