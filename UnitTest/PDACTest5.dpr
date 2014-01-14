program PDACTest5;
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

{$D+ $R+}

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
  PSQLCopyTest in 'PSQLCopyTest.pas',
  PSQLErrorsTest in 'PSQLErrorsTest.pas';

{$R *.RES}
var
  GUI :TGUITestRunner;

begin
  Application.Initialize;
  Application.CreateForm(TGUITestRunner, GUI);
  //GUI.Suite := test;
  GUI.Show;

{  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    //GUITestRunner.RunRegisteredTests;
    GUITestRunner.RunRegisteredTestsModeless;}
end.

