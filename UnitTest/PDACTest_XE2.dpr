program PDACTest_XE2;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

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
  TestPSQLFields in 'TestPSQLFields.pas',
  PSQLToolsTest in 'PSQLToolsTest.pas',
  PSQLBlobsTest in 'PSQLBlobsTest.pas',
  PSQLDumpTest in 'PSQLDumpTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

