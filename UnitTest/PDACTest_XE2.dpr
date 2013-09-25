program PDACTest_XE2;
{$I PSQLDAC.inc}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestExtensions,
  PSQLDatabaseTest in 'PSQLDatabaseTest.pas',
  PSQLQueryTest in 'PSQLQueryTest.pas',
  TestHelper in 'TestHelper.pas',
  PSQLFieldsTest in 'PSQLFieldsTest.pas',
  PSQLToolsTest in 'PSQLToolsTest.pas',
  PSQLBlobsTest in 'PSQLBlobsTest.pas',
  PSQLDumpTest in 'PSQLDumpTest.pas',
  PSQLNotifyTest in 'PSQLNotifyTest.pas',
  PSQLTypesTest in 'PSQLTypesTest.pas',
  PSQLErrorsTest in 'PSQLErrorsTest.pas';

{$R *.RES}

begin
  {$IFDEF DELPHI_12}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

