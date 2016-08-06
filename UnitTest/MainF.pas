unit MainF;
{$I pSQLDAC.inc}
interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PSQLDbTables,
  {$IFDEF MOBILE}
    DUNitX.Loggers.MobileGUI
  {$ELSE}
    DUNitX.Loggers.GUIX
  {$ENDIF},
  FMX.Controls.Presentation, FMX.StdCtrls,
  PSQLFMXConnFrm, TestHelper;

type
TPSQLRunner = {$IFDEF MOBILE}TMobileGUITestRunner{$ELSE}TGUIXTestRunner{$ENDIF};
  TMainForm = class(TForm)
    BtnStart: TButton;
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPSQLDatabase : TPSQLDatabase;
    FConnFrm : TPSQLConnForm;
    FTestFrm : TPSQLRunner;
    procedure RunTests;
  public
    property Database : TPSQLDatabase read FPSQLDatabase write FPSQLDatabase;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.BtnStartClick(Sender: TObject);
begin
  FConnFrm.GetDatabaseProperty(Database);

  {$IFDEF MOBILE}
  FConnFrm.ShowModal(
    procedure(ModalRes : TModalResult)
    begin
      if ModalRes = mrOk then
        RunTests;
    end
  );
  {$ELSE}
    if FConnFrm.ShowModal = mrOk then
      RunTests;
  {$ENDIF};
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FConnFrm := TPSQLConnForm.Create(nil);
  SetUpTestDatabase(FPSQLDatabase, 'PSQLDatabaseTest.conf');
  FConnFrm.Database := Database;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if assigned(FPSQLDatabase) then
  begin
    FPSQLDatabase.Close;
    ComponentToFile(TComponent(FPSQLDatabase), 'PSQLDatabaseTest.conf');
    FPSQLDatabase.DisposeOf;
  end;
  if Assigned(FConnFrm) then
    FConnFrm.DisposeOf;
  if Assigned(FTestFrm) then
    FTestFrm.DisposeOf;
end;

procedure TMainForm.RunTests;
begin
  if not Assigned(FTestFrm) then
    FTestFrm := TPSQLRunner.Create(nil);
  FTestFrm.Position := TFormPosition.ScreenCenter;
  FTestFrm.Show;
end;


end.
