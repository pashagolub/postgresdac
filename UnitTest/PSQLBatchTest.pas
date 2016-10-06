unit PSQLBatchTest;
{$I PSQLDAC.inc}
{$IFDEF DUNITX}
  {$M+}
{$ENDIF}

{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  PSQLTypes, Classes, PSQLDbTables, SysUtils, PSQLBatch
  {$IFNDEF DUNITX}
    ,TestFramework, TestExtensions
  {$ELSE}
    ,DUnitX.TestFramework
  {$ENDIF};

type

  {$IFDEF DUNITX}[TestFixture]{$ENDIF}
  TestTPSQLBatchExecute = class({$IFNDEF DUNITX}TTestCase{$ELSE}TObject{$ENDIF})
  private
    FPSQLBatchExecute: TPSQLBatchExecute;
  public
    {$IFNDEF DUNITX}
    procedure SetUp; override;
    procedure TearDown; override;
    {$ELSE}
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
    {$ENDIF}
  published
    procedure TestExecDollarQuoting;
    {$IFDEF DUNITX}
    [SetupFixture]
    procedure SetupFixture;
    {$ENDIF}
  end;

implementation

uses TestHelper{$IFDEF DUNITX}, MainF{$ENDIF};

procedure InternalSetUp;
begin
end;

procedure TestTPSQLBatchExecute.SetUp;
begin
  FPSQLBatchExecute := TPSQLBatchExecute.Create(nil);
  FPSQLBatchExecute.Database := TestDBSetup.Database;
end;

{$IFDEF DUNITX}
procedure TestTPSQLBatchExecute.SetupFixture;
begin
  QryDB := MainForm.Database;
  InternalSetUp;
end;
{$ENDIF}

procedure TestTPSQLBatchExecute.TearDown;
begin
  FPSQLBatchExecute.Free;
  FPSQLBatchExecute := nil;
end;

procedure TestTPSQLBatchExecute.TestExecDollarQuoting;
var IsOK: boolean;
begin
  FPSQLBatchExecute.SQL.Text := 'CREATE OR REPLACE FUNCTION bizdays(date,date) '+
                          'RETURNS BIGINT '+
                          'LANGUAGE SQL AS '+
                          '$_$ '+
                          'SELECT count(*) FROM '+
                          '(SELECT extract(''dow'' FROM $1+x) AS dow '+
                          'FROM generate_series(0,$2-$1) x) AS foo '+
                          'WHERE dow BETWEEN 1 AND 5; '+
                          '$_$; ';

  FPSQLBatchExecute.ExecSQL;
  DACCheck(TestDBSetup.Database.SelectString('SELECT bizdays(now, now)', IsOk) > '', 'Creating function as dollar-quoted string failed');
end;

initialization
{$IFDEF DUNITX}
  TDUnitX.RegisterTestFixture(TestTPSQLBatchExecute);
{$ENDIF}
end.

