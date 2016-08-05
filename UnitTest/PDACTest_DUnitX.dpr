program PDACTest_DUnitX;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  MainF in 'MainF.pas' {MainForm},
  TestHelper in 'TestHelper.pas',
  PSQLDatabaseTest in 'PSQLDatabaseTest.pas',
  PSQLQueryTest in 'PSQLQueryTest.pas',
  PSQLBatchTest in 'PSQLBatchTest.pas',
  PSQLTableTest in 'PSQLTableTest.pas',
  PSQLFieldsTest in 'PSQLFieldsTest.pas',
  PSQLToolsTest in 'PSQLToolsTest.pas',
  PSQLBlobsTest in 'PSQLBlobsTest.pas',
  PSQLDumpTest in 'PSQLDumpTest.pas',
  PSQLNotifyTest in 'PSQLNotifyTest.pas',
  PSQLCopyTest in 'PSQLCopyTest.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
