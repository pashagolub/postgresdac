program PDACTest_DUnitX;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainF in 'MainF.pas' {MainForm},
  TestHelper in 'TestHelper.pas',
  PSQLDatabaseTest in 'PSQLDatabaseTest.pas',
  PSQLQueryTest in 'PSQLQueryTest.pas',
  PSQLBatchTest in 'PSQLBatchTest.pas',
  PSQLTableTest in 'PSQLTableTest.pas',
  PSQLFieldsTest in 'PSQLFieldsTest.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
