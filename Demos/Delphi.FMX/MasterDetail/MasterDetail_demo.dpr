program MasterDetail_demo;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  ConnFrmFMX in 'ConnFrmFMX.pas' {PConnDlgFMX};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPConnDlgFMX, PConnDlgFMX);
  Application.Run;
end.
