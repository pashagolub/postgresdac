unit PSQLBlobsTest;
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
  PSQLAccess, PSQLDbTables, PSQLTypes, SysUtils, Db, Classes, Types, IOUtils,
  {$IFNDEF DUNITX}
  TestFramework, TestExtensions, Winapi.Windows, TestHelper
  {$ELSE}
  DUnitX.TestFramework, TestXHelper
  {$ENDIF};

type

  {$IFDEF DUNITX}[TestFixture]{$ENDIF}
  TestTPSQLBlobs = class({$IFNDEF DUNITX}TTestCase{$ELSE}TTestXCase{$ENDIF})
  private
    {$IFDEF DUNITX}
    FRSTestBmp: TResourceStream;
    {$ENDIF}
    FPSQLQuery: TPSQLQuery;
    FPSQLUpdate: TPSQLUpdateSQL;
{$HINTS OFF}
    procedure InternalSetUp;
{$HINTS ON}
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
    procedure TestQueryInsertAndRead;
    procedure TestUpdateObjInsert;
    procedure TestEmptyBLOBsInsertAsParams;
    procedure TestBlobSizeAnsi;
    procedure TestBlobSizeUnicode;
    procedure TestMemoEmptyValue;
    {$IFDEF DUNITX}
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    {$ENDIF}
  end;

implementation

  {$IFNDEF DUNITX}
  function FileSize(const aFilename: String): Int64;
  var
    info: TWin32FileAttributeData;
  begin
    result := -1;
    if NOT GetFileAttributesEx(PChar(aFileName), GetFileExInfoStandard, @info) then
      EXIT;
    result := info.nFileSizeLow or (info.nFileSizeHigh shl 32);
  end;
  {$ENDIF}

procedure TestTPSQLBlobs.InternalSetUp;
begin
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS blobs_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'byteaf bytea,' +
                'oidf oid,'  +
                'memof text)');
end;

procedure TestTPSQLBlobs.SetUp;
begin
  FPSQLQuery := TPSQLQuery.Create(nil);
  FPSQLQuery.Database := TestDBSetup.Database;
  TestDBSetup.Database.Execute('TRUNCATE blobs_test_case_table');
end;

procedure TestTPSQLBlobs.TearDown;
begin
{$IFNDEF NEXTGEN}
  FPSQLQuery.Free;
{$ELSE}
  FPSQLQuery.DisposeOf;
{$ENDIF}
  FPSQLQuery := nil;
end;

procedure TestTPSQLBlobs.TestBlobSizeAnsi;
begin
  TestDBSetup.Database.CharSet := 'SQL_ASCII';
  try
    FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
    FPSQLQuery.RequestLive := True;
    FPSQLQuery.Open;
    FPSQLQuery.Insert;
    {$IFNDEF DUNITX}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromFile('TestData\test.bmp');
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromFile('TestData\test.bmp');
    {$ELSE}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromStream(FRSTestBmp);
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromStream(FRSTestBmp);
    {$ENDIF}
    (FPSQLQuery.FieldByName('memof') as TBlobField).AsString := 'test-test';

    FPSQLQuery.Post;

    FPSQLQuery.First;

    {$IFNDEF DUNITX}
    Check((FPSQLQuery.FieldByName('byteaf') as TBlobField).BlobSize = FileSize('TestData\test.bmp'), 'Failed to read byteaf.BlobSize');
    Check((FPSQLQuery.FieldByName('oidf') as TBlobField).BlobSize = FileSize('TestData\test.bmp'), 'Failed to read oidf.BlobSize');
    {$ELSE}
    Check((FPSQLQuery.FieldByName('byteaf') as TBlobField).BlobSize = FRSTestBmp.Size, 'Failed to read byteaf.BlobSize');
    Check((FPSQLQuery.FieldByName('oidf') as TBlobField).BlobSize = FRSTestBmp.Size, 'Failed to read oidf.BlobSize');
    {$ENDIF}

    Check((FPSQLQuery.FieldByName('memof') as TBlobField).BlobSize = Length('test-test') * SizeOf(AnsiDACByteChar), 'Failed to read memof.BlobSize');
    FPSQLQuery.Close;
  finally
    TestDBSetup.Database.CharSet := 'UNICODE';
  end;
end;

procedure TestTPSQLBlobs.TestBlobSizeUnicode;
begin
  TestDBSetup.Database.CharSet := 'UNICODE';
  FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  FPSQLQuery.Insert;
  {$IFNDEF DUNITX}
  (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromFile('TestData\test.bmp');
  (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromFile('TestData\test.bmp');
  {$ELSE}
  (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromStream(FRSTestBmp);
  (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromStream(FRSTestBmp);
  {$ENDIF}

  (FPSQLQuery.FieldByName('memof') as TBlobField).AsString := 'test-test';
  FPSQLQuery.Post;

  FPSQLQuery.First;

  {$IFNDEF DUNITX}
  Check((FPSQLQuery.FieldByName('byteaf') as TBlobField).BlobSize = FileSize('TestData\test.bmp'), 'Failed to read byteaf.BlobSize');
  Check((FPSQLQuery.FieldByName('oidf') as TBlobField).BlobSize = FileSize('TestData\test.bmp'), 'Failed to read oidf.BlobSize');
  {$ELSE}
  Check((FPSQLQuery.FieldByName('byteaf') as TBlobField).BlobSize = FRSTestBmp.Size, 'Failed to read byteaf.BlobSize');
  Check((FPSQLQuery.FieldByName('oidf') as TBlobField).BlobSize = FRSTestBmp.Size, 'Failed to read oidf.BlobSize');
  {$ENDIF}

  Check((FPSQLQuery.FieldByName('memof') as TBlobField).BlobSize = Length('test-test') * SizeOf(Char), 'Failed to read memof.BlobSize');
  FPSQLQuery.Close;
end;

procedure TestTPSQLBlobs.TestEmptyBLOBsInsertAsParams;
var
  MS: TMemoryStream;
begin
  FPSQLQuery.SQL.Text := 'INSERT INTO blobs_test_case_table VALUES (DEFAULT, :b, :o, :m)';
  MS := TMemoryStream.Create;
  try
    FPSQLQuery.ParamByName('b').LoadFromStream(MS, ftBlob);
    FPSQLQuery.ParamByName('o').DataTypeOID := FIELD_TYPE_OID;
    FPSQLQuery.ParamByName('o').LoadFromStream(MS, ftBlob);
    FPSQLQuery.ParamByName('m').LoadFromStream(MS, ftBlob);
    FPSQLQuery.ExecSQL;
    FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
    FPSQLQuery.Open;
    FPSQLQuery.First;

    Check((FPSQLQuery.FieldByName('byteaf') as TBlobField).BlobSize = 0, 'byteaf field must be empty');
    Check((FPSQLQuery.FieldByName('oidf') as TBlobField).BlobSize = 0, 'oidf field must be empty');
    Check((FPSQLQuery.FieldByName('memof') as TBlobField).AsString = '', 'memof field must be empty');
  finally
    {$IFNDEF NEXTGEN}
    MS.Free;
    {$ELSE}
    MS.DisposeOf;
    {$ENDIF}
    FPSQLQuery.Close;
  end;
end;

procedure TestTPSQLBlobs.TestMemoEmptyValue;
begin
  //compatibility issue: memos with empty strings are evaulated as NULLs
  FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  FPSQLQuery.Insert;
  (FPSQLQuery.FieldByName('memof') as TBlobField).AsString := '';
  Check(FPSQLQuery.FieldByName('memof').IsNull, 'memof field must NULL before Post with empty text value');
  FPSQLQuery.Post;
  FPSQLQuery.First;
  Check(FPSQLQuery.FieldByName('memof').IsNull, 'memof field must NULL after Post with empty text value');
  FPSQLQuery.Close;
end;

procedure TestTPSQLBlobs.TestQueryInsertAndRead;
var
  OutputPathStr: String;
  {$IFDEF DUNITX}
  FS: TStringStream;
  {$ENDIF}
begin
  {$IFNDEF DUNITX}
    OutputPathStr := 'TestOutput\test.bmp';
  {$ELSE}
    OutputPathStr := TPath.GetDocumentsPath + PathDelim + 'test.bmp';
    FS := TStringStream.Create;
  {$ENDIF}
  try
    FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
    FPSQLQuery.RequestLive := True;
    FPSQLQuery.Open;
    Check(FPSQLQuery.FieldByName('byteaf') is TBlobField, 'Wrong field class for byteaf');
    Check(FPSQLQuery.FieldByName('oidf') is TBlobField, 'Wrong field class for oidf');
    Check(FPSQLQuery.FieldByName('memof') is TBlobField, 'Wrong field class for memof');
    FPSQLQuery.Insert;
    {$IFNDEF DUNITX}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromFile('TestData\test.bmp');
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromFile('TestData\test.bmp');
    {$ELSE}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromStream(FRSTestBmp);
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromStream(FRSTestBmp);
    {$ENDIF}
    (FPSQLQuery.FieldByName('memof') as TBlobField).AsString := 'test-test';
    FPSQLQuery.Post;

    FPSQLQuery.First;
    Check(not FPSQLQuery.FieldByName('byteaf').IsNull, 'byteaf field must be NOT NULL');
    Check(not FPSQLQuery.FieldByName('oidf').IsNull, 'oidf field must be NOT NULL');
    Check(not FPSQLQuery.FieldByName('memof').IsNull, 'memof field must be NOT NULL');

    (FPSQLQuery.FieldByName('byteaf') as TBlobField).SaveToFile(OutputPathStr);
    (FPSQLQuery.FieldByName('oidf') as TBlobField).SaveToFile(OutputPathStr);
    Check(FileExists(OutputPathStr), 'byteaf cannot save file to disk');
    Check(FileExists(OutputPathStr), 'oidf cannot save file to disk');

    {$IFNDEF DUNITX}
    Check(FileSize('TestData\test.bmp') = FileSize(OutputPathStr), 'file sizes differs');
    {$ELSE}
    FS.LoadFromFile(OutputPathStr);
    Check(FRSTestBmp.Size = FS.Size);
    {$ENDIF}

    Check(FPSQLQuery.FieldByName('memof').AsString = 'test-test', 'Failed to read memof field');

  finally
    FPSQLQuery.Close;
    {$IFDEF DUNITX}
    FS.DisposeOf;
    {$ENDIF}
  end;
end;

procedure TestTPSQLBlobs.TestUpdateObjInsert;
begin
  FPSQLUpdate := TPSQLUpdateSQL.Create(nil);
  try
    FPSQLUpdate.SQL[ukInsert].Text := 'INSERT INTO blobs_test_case_table VALUES (DEFAULT, :byteaf, :oidf, :memof)';
    FPSQLQuery.UpdateObject := FPSQLUpdate;
    FPSQLQuery.SQL.Text := 'SELECT * FROM blobs_test_case_table';
    FPSQLQuery.Open;
    Check(FPSQLQuery.FieldByName('byteaf') is TBlobField, 'Wrong field class for byteaf');
    Check(FPSQLQuery.FieldByName('oidf') is TBlobField, 'Wrong field class for oidf');
    Check(FPSQLQuery.FieldByName('memof') is TBlobField, 'Wrong field class for memof');
    FPSQLQuery.Insert;
    {$IFNDEF DUNITX}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromFile('TestData\test.bmp');
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromFile('TestData\test.bmp');
    {$ELSE}
    (FPSQLQuery.FieldByName('byteaf') as TBlobField).LoadFromStream(FRSTestBmp);
    (FPSQLQuery.FieldByName('oidf') as TBlobField).LoadFromStream(FRSTestBmp);
    {$ENDIF}
    (FPSQLQuery.FieldByName('memof') as TBlobField).AsString := 'test-test';

    FPSQLQuery.Post;
    FPSQLQuery.First;
    Check(not FPSQLQuery.FieldByName('byteaf').IsNull, 'byteaf field must be NOT NULL');
    Check(not FPSQLQuery.FieldByName('oidf').IsNull, 'oidf field must be NOT NULL');
    Check(not FPSQLQuery.FieldByName('memof').IsNull, 'memof field must be NOT NULL');
    FPSQLQuery.Close;
  finally
    {$IFNDEF NEXTGEN}
    FreeAndNil(FPSQLUpdate);
    {$ELSE}
    FPSQLQuery.DisposeOf;
    {$ENDIF}
  end;
end;

{$IFDEF DUNITX}
procedure TestTPSQLBlobs.SetupFixture;
begin
  FRsTestBmp := TResourceStream.Create(HInstance, 'test_bmp', RT_RCDATA);
  FRSTestBmp.Position := 0;
  InternalSetUp;
end;
procedure TestTPSQLBlobs.TearDownFixture;
begin
  FRSTestBmp.Free;
end;
{$ENDIF}

initialization
 {$IFDEF DUNITX}
  TDUnitX.RegisterTestFixture(TestTPSQLBlobs);
{$ENDIF}

end.

