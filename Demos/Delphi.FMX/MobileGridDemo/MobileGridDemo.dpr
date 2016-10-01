program MobileGridDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmMobileGrid in 'fmMobileGrid.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
