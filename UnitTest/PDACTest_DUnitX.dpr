program PDACTest_DUnitX;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  PSQLDatabaseTest in 'PSQLDatabaseTest.pas',
  PSQLQueryTest in 'PSQLQueryTest.pas',
  PSQLBatchTest in 'PSQLBatchTest.pas',
  PSQLTableTest in 'PSQLTableTest.pas',
  PSQLFieldsTest in 'PSQLFieldsTest.pas',
  PSQLToolsTest in 'PSQLToolsTest.pas',
  PSQLBlobsTest in 'PSQLBlobsTest.pas',
  PSQLDumpTest in 'PSQLDumpTest.pas',
  PSQLNotifyTest in 'PSQLNotifyTest.pas',
  PSQLCopyTest in 'PSQLCopyTest.pas',
  PSQLErrorsTest in 'PSQLErrorsTest.pas',
  PSQLTypesTest in 'PSQLTypesTest.pas',
  TestXHelper in 'TestXHelper.pas',
  {$IFDEF MOBILE}
    DUNitX.Loggers.MobileGUI
  {$ELSE}
    DUNitX.Loggers.GUIX
  {$ENDIF};

type
  TPSQLRunner = {$IFDEF MOBILE}TMobileGUITestRunner{$ELSE}TGUIXTestRunner{$ENDIF};

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  TestDBSetup := TTestDBSetup.Create;
  TestDBSetup.SetUp();
  with TPSQLRunner.Create(Application) do
  begin
    Position := TFormPosition.ScreenCenter;
    Show;
  end;
  Application.Run;
end.
