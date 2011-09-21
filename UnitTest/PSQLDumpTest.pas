unit PSQLDumpTest;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, Db, PSQLDump, PSQLTypes, BDE, Classes, SysUtils, PSQLDbTables, DbTables,
  Variants, Windows, Math, PSQLAboutFrm, TestExtensions;

type
  //Setup decorator
  TDbSetup = class(TTestSetup)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TPSQLDump
  TestTPSQLDump = class(TTestCase)
  strict private
    FPSQLDump: TPSQLDump;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDumpToStream;
    procedure TestDumpToStream1;
    procedure TestDumpToStream2;
    procedure TestDumpToFile;
    procedure TestDumpToFile1;
  end;

  // Test methods for class TPSQLRestore
  TestTPSQLRestore = class(TTestCase)
  strict private
    FPSQLRestore: TPSQLRestore;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRestoreFromFile;
    procedure TestRestoreFromFile1;
  end;

var QryDb: TPSQLDatabase;

implementation

uses TestHelper;

procedure TestTPSQLDump.SetUp;
begin
  FPSQLDump := TPSQLDump.Create(nil);
end;

procedure TestTPSQLDump.TearDown;
begin
  FPSQLDump.Free;
  FPSQLDump := nil;
end;

procedure TestTPSQLDump.TestDumpToStream;
var
  Stream: TStream;
begin
  // TODO: Setup method call parameters
  FPSQLDump.DumpToStream(Stream);
  // TODO: Validate method results
end;

procedure TestTPSQLDump.TestDumpToStream1;
var
  Log: TStrings;
  Stream: TStream;
begin
  // TODO: Setup method call parameters
  FPSQLDump.DumpToStream(Stream, Log);
  // TODO: Validate method results
end;

procedure TestTPSQLDump.TestDumpToStream2;
var
  LogFileName: string;
  Stream: TStream;
begin
  // TODO: Setup method call parameters
  FPSQLDump.DumpToStream(Stream, LogFileName);
  // TODO: Validate method results
end;

procedure TestTPSQLDump.TestDumpToFile;
var
  Log: TStrings;
  FileName: string;
begin
  // TODO: Setup method call parameters
  FPSQLDump.DumpToFile(FileName, Log);
  // TODO: Validate method results
end;

procedure TestTPSQLDump.TestDumpToFile1;
var
  LogFileName: string;
  FileName: string;
begin
  // TODO: Setup method call parameters
  FPSQLDump.DumpToFile(FileName, LogFileName);
  // TODO: Validate method results
end;

procedure TestTPSQLRestore.SetUp;
begin
  FPSQLRestore := TPSQLRestore.Create(nil);
end;

procedure TestTPSQLRestore.TearDown;
begin
  FPSQLRestore.Free;
  FPSQLRestore := nil;
end;

procedure TestTPSQLRestore.TestRestoreFromFile;
var
  Log: TStrings;
  FileName: string;
begin
  // TODO: Setup method call parameters
  FPSQLRestore.RestoreFromFile(FileName, Log);
  // TODO: Validate method results
end;

procedure TestTPSQLRestore.TestRestoreFromFile1;
var
  LogFileName: string;
  FileName: string;
begin
  // TODO: Setup method call parameters
  FPSQLRestore.RestoreFromFile(FileName, LogFileName);
  // TODO: Validate method results
end;

{ TDbSetup }

procedure TDbSetup.SetUp;
begin
  inherited;
  SetUpTestDatabase(QryDB, 'PSQLBlobs.conf');
  QryDB.Execute('CREATE TABLE blobs_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'byteaf bytea,' +
                'oidf oid,'  +
                'memof text)');
end;

procedure TDbSetup.TearDown;
begin
  inherited;
  ComponentToFile(QryDB, 'PSQLBlobs.conf');
  QryDB.Execute('DROP TABLE blobs_test_case_table');
  QryDB.Free;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTPSQLDump.Suite);
  RegisterTest(TestTPSQLRestore.Suite);
end.

