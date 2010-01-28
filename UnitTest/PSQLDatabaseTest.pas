unit PSQLDatabaseTest;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, Db, Windows, PSQLAccess, ExtCtrls, Controls, Classes, PSQLDbTables,
  PSQLTypes, SysUtils, DbCommon, Variants, Graphics, StdVCL, TestExtensions,
  Forms, PSQLConnFrm;

type
  //Setup decorator
  TDbSetup = class(TTestSetup)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TPSQLDatabase

  TestTPSQLDatabase = class(TTestCase)
  published
    procedure HookUp;
    procedure TestExecute;
    procedure TestGetBackendPID;
    procedure TestSelectString;
    procedure TestSelectString1;
    procedure TestSelectStringDef;
    procedure TestSelectStringDef1;
    procedure TestSelectStrings;
    procedure TestSelectStrings1;
    procedure TestCommit;
    procedure TestGetCharsets;
    procedure TestGetDatabases;
    procedure TestGetSchemaNames;
    procedure TestGetStoredProcNames;
    procedure TestGetTableNames;
    procedure TestGetTablespaces;
    procedure TestGetUserNames;
    procedure TestReset;
    procedure TestRollback;
    procedure TestStartTransaction;
  end;

var
  FPSQLDatabase: TPSQLDatabase;

implementation

uses TestHelper;

procedure TestTPSQLDatabase.HookUp;
begin
 Check(True);
end;

procedure TestTPSQLDatabase.TestExecute;
var
  ReturnValue: Integer;
  Cursor: phDBICur;
  Cache: Boolean;
  Params: TParams;
  SQL: string;
begin
  SQL := 'SELECT version()';
  ReturnValue := FPSQLDatabase.Execute(SQL);
  Check(ReturnValue = 0);
end;

procedure TestTPSQLDatabase.TestGetBackendPID;
var
  ReturnValue: Integer;
begin
  ReturnValue := FPSQLDatabase.GetBackendPID;
  Check(ReturnValue > InvalidOID);
end;

procedure TestTPSQLDatabase.TestSelectString;
var
  ReturnValue: string;
  aFieldName: string;
  IsOk: Boolean;
  aSQL: string;
begin
  aSQL := 'SELECT 12345 as column1';
  aFieldName := 'column1';
  ReturnValue := FPSQLDatabase.SelectString(aSQL, IsOk, aFieldName);
  Check(IsOk);
  CheckEquals('12345', ReturnValue);
end;

procedure TestTPSQLDatabase.TestSelectString1;
var
  ReturnValue: string;
  aFieldNumber: Integer;
  IsOk: Boolean;
  aSQL: string;
begin
  aSQL := 'SELECT 12345 as column1';
  aFieldNumber := 0;
  ReturnValue := FPSQLDatabase.SelectString(aSQL, IsOk, aFieldNumber);
  Check(IsOk);
  CheckEquals('12345', ReturnValue);
end;

procedure TestTPSQLDatabase.TestSelectStringDef;
var
  ReturnValue: string;
  aFieldName: string;
  aDefaultValue: string;
  aSQL: string;
begin
  aSQL := 'SELECT 12345 as column1';
  aFieldName := 'column1';
  ReturnValue := FPSQLDatabase.SelectStringDef(aSQL, aDefaultValue, aFieldName);
  CheckEquals('12345', ReturnValue);
  aSQL := 'SELECT 12345 as column1';
  aFieldName := 'WRONG_COL_NAME';
  aDefaultValue := 'MyDefaultValue';
  ReturnValue := FPSQLDatabase.SelectStringDef(aSQL, aDefaultValue, aFieldName);
  CheckEquals(aDefaultValue, ReturnValue);
end;

procedure TestTPSQLDatabase.TestSelectStringDef1;
var
  ReturnValue: string;
  aFieldNumber: Integer;
  aDefaultValue: string;
  aSQL: string;
begin
  aSQL := 'SELECT 12345 as column1';
  aFieldNumber := 0;
  ReturnValue := FPSQLDatabase.SelectStringDef(aSQL, aDefaultValue, aFieldNumber);
  CheckEquals('12345', ReturnValue);
  aSQL := 'SELECT 12345 as column1';
  aFieldNumber := -1234214;
  aDefaultValue := 'MyDefaultValue';
  ReturnValue := FPSQLDatabase.SelectStringDef(aSQL, aDefaultValue, aFieldNumber);
  CheckEquals(aDefaultValue, ReturnValue);
end;

procedure TestTPSQLDatabase.TestSelectStrings;
var
  aFieldName: string;
  aList: TStrings;
  aSQL: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.SelectStrings(aSQL, aList, aFieldName);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestSelectStrings1;
var
  aFieldNumber: Integer;
  aList: TStrings;
  aSQL: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.SelectStrings(aSQL, aList, aFieldNumber);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestCommit;
begin
  FPSQLDatabase.Commit;
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetCharsets;
var
  List: TStrings;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetCharsets(List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetDatabases;
var
  List: TStrings;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetDatabases(Pattern, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetSchemaNames;
var
  List: TStrings;
  SystemSchemas: Boolean;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetSchemaNames(Pattern, SystemSchemas, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetStoredProcNames;
var
  List: TStrings;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetStoredProcNames(Pattern, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetTableNames;
var
  List: TStrings;
  SystemTables: Boolean;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetTableNames(Pattern, SystemTables, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetTablespaces;
var
  List: TStrings;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetTablespaces(Pattern, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestGetUserNames;
var
  List: TStrings;
  Pattern: string;
begin
  Check(False);
  // TODO: Setup method call parameters
  FPSQLDatabase.GetUserNames(Pattern, List);
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestReset;
begin
  Check(False);
  FPSQLDatabase.Reset;
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestRollback;
begin
  Check(False);
  FPSQLDatabase.Rollback;
  // TODO: Validate method results
end;

procedure TestTPSQLDatabase.TestStartTransaction;
begin
  Check(False);
  FPSQLDatabase.StartTransaction;
  // TODO: Validate method results
end;

{ MainFormSetup }

procedure TDbSetup.SetUp;
begin
  inherited;
  SetUpTestDatabase(FPSQLDatabase, 'PSQLDatabaseTest.conf');
end;

procedure TDbSetup.TearDown;
begin
  inherited;
  ComponentToFile(FPSQLDatabase, 'PSQLDatabaseTest.conf');
  FPSQLDatabase.Free;
end;

initialization
  //PaGo: Register any test cases with setup decorator
  RegisterTest(TDbSetup.Create(TestTPSQLDatabase.Suite, 'Database Setup'));

  // Register any test cases with the test runner
  //RegisterTest(TestTPSQLDatabase.Suite);
end.

