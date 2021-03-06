unit PSQLCopyTest;
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
  PSQLAccess, Classes, PSQLCopy, PSQLDbTables, PSQLTypes, SysUtils, IOUtils,
  {$IFNDEF DUNITX}
  TestFramework, TestExtensions, TestHelper
  {$ELSE}
  DUnitX.TestFramework, Types, TestXHelper
  {$ENDIF};

type

  {$IFDEF DUNITX}[TestFixture]{$ENDIF}
  TestTCustomPSQLCopy = class({$IFNDEF DUNITX}TTestCase{$ELSE}TTestXCase{$ENDIF})
  private
    {$IFDEF DUNITX}
    FRSTestTask : TResourceStream;
    {$ENDIF}
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
    procedure TestLoadFromStream;
    procedure TestSaveToStream;
    procedure TestLoadFromStrings;
    procedure TestSaveToStrings;
    procedure TestLoadFromClientSideFile;
    procedure TestSaveToClientSideFile;
    procedure TestLoadFromServerSideFile;
    procedure TestSaveToServerSideFile;
    procedure TestLoadFromProgram;
    procedure TestSaveToProgram;
    procedure TestForceQuote;
    {$IFDEF DUNITX}
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    {$ENDIF}
  end;

implementation

var
  FPSQLCopy: TPSQLCopy;
  FilePath: string; //= 'TestOut\';

{$IFNDEF DUNITX}
procedure TestTCustomPSQLCopy.TestLoadFromStream;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FilePath + 'tasks.csv', fmOpenRead);
  try
    Stream.Position := 0;
    FPSQLCopy.Tablename := 'server_tasks';
    FPSQLCopy.DataFormat := cfCSV;
    FPSQLCopy.LoadFromStream(Stream);
    Check(FPSQLCopy.RowsAffected > 0);
  finally
    Stream.Free;
  end;
end;
{$ELSE}
procedure TestTCustomPSQLCopy.TestLoadFromStream;
begin
  FPSQLCopy.Tablename := 'server_tasks';
  FPSQLCopy.DataFormat := cfCSV;
  FPSQLCopy.LoadFromStream(FRSTestTask);
  Check(FPSQLCopy.RowsAffected > 0);
  FRSTestTask.Position := 0;
end;
{$ENDIF}

procedure TestTCustomPSQLCopy.TestSaveToStream;
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    FPSQLCopy.SQL.Text := 'SELECT * FROM generate_series(1, 10)';
    FPSQLCopy.SaveToStream(Stream);
    Check(Stream.Size > 0);
  finally
    {$IFNDEF NEXTGEN}
    Stream.Free;
    {$ELSE}
    Stream.DisposeOf;
    {$ENDIF}
  end;
end;

procedure TestTCustomPSQLCopy.TestLoadFromStrings;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create;
  try
    {$IFNDEF DUNITX}
    Strings.LoadFromFile(FilePath + 'tasks.csv');
    {$ELSE}
    Strings.LoadFromStream(FRSTestTask);
    {$ENDIF}
    FPSQLCopy.Tablename := 'server_tasks';
    FPSQLCopy.DataFormat := cfCSV;
    FPSQLCopy.LoadFromStrings(Strings);
    Check(FPSQLCopy.RowsAffected = Strings.Count);
  finally
    {$IFNDEF DUNITX}
    Strings.Free;
    {$ELSE}
    Strings.DisposeOf;
    {$ENDIF}
  end;
end;

procedure TestTCustomPSQLCopy.TestSaveToStrings;
var
  Strings: TStrings;
begin
  Strings := TStringList.Create();
  try
    FPSQLCopy.SQL.Text := 'SELECT * FROM generate_series(1, 10)';
    FPSQLCopy.SaveToStrings(Strings);
    Check(Strings.Count = 10, 'SaveToStrings failed');
  finally
    {$IFNDEF DUNITX}
    Strings.Free;
    {$ELSE}
    Strings.DisposeOf;
    {$ENDIF}
  end;
end;

procedure InternalSetUp;
begin


end;

procedure InternalTearDown;
begin
  {$IFNDEF DUNITX}
  FPSQLCopy.Free;
  {$ELSE}
  FPSQLCopy.DisposeOf;
  {$ENDIF}
end;

{$IFDEF DUNITX}
procedure TestTCustomPSQLCopy.SetupFixture;
begin
  FRSTestTask := TResourceStream.Create(HInstance, 'tasks_csv', RT_RCDATA);
  FRSTestTask.Position := 0;
  InternalSetUp;
end;

procedure TestTCustomPSQLCopy.TearDownFixture;
begin
  FRSTestTask.Free;
  InternalTearDown;
end;
{$ENDIF}

procedure TestTCustomPSQLCopy.SetUp;
begin
  FPSQLCopy := TPSQLCopy.Create(nil);
  FPSQLCopy.Database := TestDBSetup.Database;
end;

procedure TestTCustomPSQLCopy.TearDown;
begin
  FreeAndNil(FPSQLCopy);
end;

procedure TestTCustomPSQLCopy.TestForceQuote;
begin
  FPSQLCopy.Options := FPSQLCopy.Options + [coCSV, coQuote];
  FPSQLCopy.Quote := '`';
  TestSaveToStream();
end;

procedure TestTCustomPSQLCopy.TestLoadFromClientSideFile;
var
  FileName: string;
begin
{$IFDEF DUNITX}
  FRSTestTask.SaveToFile(FilePath + 'tasks.csv');
{$ENDIF}
  FileName := FilePath + 'tasks.csv';
  FPSQLCopy.Tablename := 'server_tasks';
  FPSQLCopy.DataFormat := cfCSV;
  FPSQLCopy.LoadFromClientSideFile(FileName);
end;

procedure TestTCustomPSQLCopy.TestSaveToClientSideFile;
var
  FileName: string;
begin
  FileName := FilePath + 'TasksCopyOutput.bin';
  DeleteFile(FileName);
  FPSQLCopy.Tablename := 'server_tasks';
  FPSQLCopy.DataFormat := cfBinary;
  FPSQLCopy.SaveToClientSideFile(FileName);
  Check(FileExists(FileName), 'Output file doesn''t exist');
end;

procedure TestTCustomPSQLCopy.TestLoadFromServerSideFile;
//var
//  FileName: string;
begin
  // TODO: Setup method call parameters
  // FPSQLCopy.LoadFromServerSideFile(FileName);
  // TODO: Validate method results
  Check(True);
end;

procedure TestTCustomPSQLCopy.TestSaveToServerSideFile;
var
  FileName: string;
  QueryRes: string;
begin
  FPSQlCopy.SQL.Text := 'SELECT * FROM generate_series(1, 11)';
  FileName := TestDBSetup.Database.SelectStringDef('SHOW data_directory', 'C:') + '/loglist.txt';
  FPSQLCopy.SaveToServerSideFile(FileName);
  QueryRes := TestDBSetup.Database.SelectStringDef('SELECT now() - s.modification < ''10 minutes'' FROM pg_stat_file(' + QuotedStr(FileName) + ') s', 'f');
  Check(QueryRes = 't');
end;

procedure TestTCustomPSQLCopy.TestLoadFromProgram;
var
  CommandLine: string;
begin
  FPSQLCopy.Tablename := 'server_tasks';
  CommandLine := 'tasklist /fo csv /nh';
  FPSQLCopy.Options := [coHeader];
  FPSQLCopy.DataFormat := cfCSV;
  FPSQLCopy.Encoding := 'WIN866';
  FPSQLCopy.LoadFromProgram(CommandLine);
  Check(FPSQLCopy.RowsAffected > 0);
end;

procedure TestTCustomPSQLCopy.TestSaveToProgram;
var
  CommandLine: string;
  QueryRes: string;
begin
  FPSQlCopy.SQL.Text := 'SELECT * FROM generate_series(1, 11)';
  CommandLine := 'find "1" > loglist.txt';
  FPSQLCopy.SaveToProgram(CommandLine);
  QueryRes := TestDBSetup.Database.SelectStringDef('SELECT now()-s.modification<''10 minutes'' FROM pg_stat_file(''loglist.txt'') s', 'f');
  Check(QueryRes = 't');
end;

initialization

{$IFDEF DUNITX}
  TDUnitX.RegisterTestFixture(TestTCustomPSQLCopy);
{$ENDIF}
  FilePath := {$IFDEF DUNITX}
                      TPath.GetDocumentsPath + PathDelim
                     {$ELSE}
                     'TestData\'
                     {$ENDIF};
end.

