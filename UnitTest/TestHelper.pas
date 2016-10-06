unit TestHelper;
interface

uses Classes, SysUtils, PSQLDbTables, PSQLTypes,
  {$IFNDEF DUNITX}
  Forms, PSQLConnFrm, Controls, TestFramework, TestExtensions
  {$ELSE}
  DUnitX.TestFramework
  {$ENDIF};

type
  {$IFNDEF DUNITX}
  //Setup decorator
  TTestDBSetup = class(TTestSetup, ITestSuite)
  protected
    function CountTestsInterfaces: Integer;
    function CountEnabledTestInterfaces: Integer;
  public
    Database: TPSQLDatabase;
    Query: TPSQLQuery;

    procedure SetUp; override;
    procedure TearDown; override;

    function CountTestCases: Integer; override;
    function CountEnabledTestCases: Integer; override;

    function GetName: string; override;
    procedure RunTest(ATestResult: TTestResult); override;
    procedure AddTest(test: ITest);
    procedure AddSuite(suite : ITestSuite);
  published
    // this method is not executed (for TTestSetup)
    procedure TestDBSetupTest;
  end;
  {$ENDIF}


  TDACTestCase = class helper for TTestCase
  public
    procedure DACCheck(Condition: Boolean; Msg: String);
    procedure DACIsTrue(Condition: Boolean);
  end;

var
  TestDBSetup: TTestDBSetup;

procedure ComponentToFile(Component: TComponent; Name: string);
procedure FileToComponent(Name: string; Component: TComponent);
procedure SetUpTestDatabase(var DB: TPSQLDatabase; ConfFileName: string);
//procedure DACIsTrue(Condition: Boolean);
//procedure DACCheck(Condition: Boolean; Msg: String);


implementation

uses PSQLDatabaseTest, PSQLBatchTest, PSQLBlobsTest, PSQLCopyTest, PSQLDumpTest, PSQLErrorsTest,
  PSQLFieldsTest, PSQLNotifyTest, PSQLQueryTest, PSQLTableTest, PSQLToolsTest, PSQLTypesTest;

{ TTestDBSetup }

procedure TTestDBSetup.TestDBSetupTest;
begin
  CheckTrue(True);
end;

procedure TTestDBSetup.AddSuite(suite: ITestSuite);
begin
  AddTest(suite);
end;

procedure TTestDBSetup.AddTest(test: ITest);
begin
  Assert(Assigned(test));

  FTests.Add(test);
end;

function TTestDBSetup.CountEnabledTestCases: Integer;
begin
  Result := inherited CountEnabledTestCases;
  if Enabled then
    Inc(Result, CountEnabledTestInterfaces);
end;

function TTestDBSetup.CountEnabledTestInterfaces: Integer;
var
  i: Integer;
begin
  Result := 0;
  // skip FIRST test case (it is FTest)
  for i := 1 to FTests.Count - 1 do
    if (FTests[i] as ITest).Enabled then
      Inc(Result, (FTests[i] as ITest).CountEnabledTestCases);
end;

function TTestDBSetup.CountTestCases: Integer;
begin
  Result := inherited CountTestCases;
  if Enabled then
    Inc(Result, CountTestsInterfaces);
end;

function TTestDBSetup.CountTestsInterfaces: Integer;
var
  i: Integer;
begin
  Result := 0;
  // skip FIRST test case (it is FTest)
  for i := 1 to FTests.Count - 1 do
    Inc(Result, (FTests[i] as ITest).CountTestCases);
end;

function TTestDBSetup.GetName: string;
begin
  Result := FTestName;
end;

procedure TTestDBSetup.RunTest(ATestResult: TTestResult);
var
  i: Integer;
begin
  inherited;
  // skip FIRST test case (it is FTest)
  for i := 1 to FTests.Count - 1 do
    (FTests[i] as ITest).RunWithFixture(ATestResult);
end;

procedure TTestDBSetup.SetUp;
var i: integer;
begin
  inherited;
  SetUpTestDatabase(Database, 'PDACTest.conf');
  Query := TPSQLQuery.Create(nil);
  Query.Database := TestDBSetup.Database;
  Query.ParamCheck := False;
  TestDBSetup.Database.ErrorVerbosity := evVERBOSE;
  //TestDBSetup.Database.Execute('SET TimeZone to ''America/Caracas'''); // for the complex timezone -04:30
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS requestlive_test ' +
                '(' +
                '  id serial NOT NULL PRIMARY KEY,' + //Serial will create Sequence -> not Required
                '  intf integer NOT NULL,' + //NotNull ->Required
                '  string character varying(100) NOT NULL DEFAULT ''abc'',' + //NotNull + Default -> not Required
                '  datum timestamp without time zone,' + //not Required etc.
                '  notes text,' +
                '  graphic oid,' +
                '  b_graphic bytea,' +
                '  b boolean,' +
                '  floatf real,' +
                '  datef date,' +
                '  timef time,' +
                '  numf numeric' +
                ')');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS required_test ' +
                '(' +
                '  id serial NOT NULL PRIMARY KEY,' + //Serial will create Sequence -> not Required
                '  intf integer NOT NULL,' + //NotNull ->Required
                '  string character varying(100) NOT NULL DEFAULT ''abc'',' + //NotNull + Default -> not Required
                '  datum timestamp without time zone)'); //not Required.
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS testtable' +
                '(  col1 character varying(10) NOT NULL,' +
                '  col2 character varying(10) NOT NULL,' +
                '  CONSTRAINT pk_testtable PRIMARY KEY (col1, col2) )');
  TestDBSetup.Database.Execute('INSERT INTO testtable VALUES (''10'', ''20''), (''11'', ''21'')' );
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS uuid_test_case_table(uuidf uuid NOT NULL PRIMARY KEY)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS geometry_test_case_table(id int4 PRIMARY KEY, p point, c circle, b box, l lseg)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS range_test_case_table('+
                'id int4 PRIMARY KEY, numr numrange, '+
                'intr int4range, dater daterange, '+
                'tsr tsrange, tstzr tstzrange)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE tools_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'sfield TEXT DEFAULT now()::text,' +
                'tfield timestamp DEFAULT now(),'  +
                'rfield real DEFAULT random())');
  TestDBSetup.Database.Execute('CREATE INDEX rfield_idx ON tools_test_case_table (rfield)');
  for i := 0 to 100 do
    TestDBSetup.Database.Execute('INSERT INTO tools_test_case_table DEFAULT VALUES');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS blobs_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'byteaf bytea,' +
                'oidf oid,'  +
                'memof text)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE server_tasks(process text, PID int4, session varchar, session_num int4, memory varchar)');
end;

procedure TTestDBSetup.TearDown;
begin
  ComponentToFile(Database, 'PDACTest.conf');
  Query.Free;
  Database.Free;
  inherited;
end;

procedure SetUpTestDatabase(var DB: TPSQLDatabase; ConfFileName: string);
{$IFNDEF DUNITX}
var
  Frm: TPSQLConnForm;
{$ENDIF}
begin
  DB := TPSQLDatabase.Create(nil);
  if FileExists(ConfFileName) then
  try
    FileToComponent(ConfFileName, DB);
    DB.Close;
  except
    on E: EPSQLDatabaseError do //nothing, failed connection
  end;
{$IFNDEF DUNITX}
  Application.CreateForm(TPSQLConnForm, Frm);
  try
    with Frm do
     begin
      GetDatabaseProperty(DB);
      if ShowModal = mrOk then
        SetDatabaseProperty(DB)
      else
        begin
         FreeAndNil(DB);
         raise EInvalidOperation.Create('Test cancelled by operator!');
        end;
     end;
  finally
   Frm.Free;
  end;
  DB.Open;
{$ENDIF}
end;

procedure ComponentToFile(Component: TComponent; Name: string);

var
  BinStream: TMemoryStream;
  StrStream: TFileStream;
begin

  BinStream := TMemoryStream.Create;
  try
    StrStream := TFileStream.Create(Name, fmCreate);
    try
      BinStream.WriteComponent(Component);
      {$IFNDEF NEXTGEN}
      BinStream.Seek(0, soFromBeginning);
      {$ELSE}
      BinStream.Position := 0;
      {$ENDIF}
      ObjectBinaryToText(BinStream, StrStream);
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure FileToComponent(Name: string; Component: TComponent);
var
  StrStream:TFileStream;
  BinStream: TMemoryStream;
begin
  StrStream := TFileStream.Create(Name, fmOpenRead);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      {$IFNDEF NEXTGEN}
      BinStream.Seek(0, soFromBeginning);
      {$ELSE}
      BinStream.Position := 0;
      {$ENDIF}
      BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure TDACTestCase.DACIsTrue(Condition: Boolean);
begin
  {$IFNDEF DUNITX}
  Check(Condition);
  {$ELSE}
  Assert.IsTrue(Condition);
  {$ENDIF}
end;

procedure TDACTestCase.DACCheck(Condition: Boolean; Msg: String);
begin
  {$IFNDEF DUNITX}
  Check(Condition, Msg);
  {$ELSE}
  Assert.IsTrue(Condition, Msg);
  {$ENDIF}
end;

function DBSuite: ITestSuite;
begin
  Result := TTestSuite.Create('Tests on server');

  Result.AddTest(TestTPSQLDatabase.Suite);
  Result.AddTest(TestTPSQLQuery.Suite);
  Result.AddTest(TestTPSQLTable.Suite);
  Result.AddTest(TestTPSQLBlobs.Suite);
  Result.AddTest(TestTPSQLErrors.Suite);
  Result.AddTest(TestTCustomPSQLCopy.Suite);
  Result.AddTest(TestTPSQLGuidField.Suite);
  Result.AddTest(TestGeometricFields.Suite);
  Result.AddTest(TestTPSQLRangeField.Suite);
  Result.AddTest(TestNativeNumericField.Suite);
  Result.AddTest(TestTPSQLNotify.Suite);
  Result.AddTest(TestTPSQLTools.Suite);
  Result.AddTest(TestTPSQLDump.Suite);
  Result.AddTest(TestTPSQLRestore.Suite);
end;

initialization
  TestDBSetup := TTestDBSetup.Create(DBSuite(), 'DB Classes');
  RegisterTest(TestDBSetup);
end.
