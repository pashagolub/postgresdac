unit PSQLFieldsTest;
{$I PSQLDAC.inc}
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
  PSQLTypes, SysUtils, DbCommon, Graphics, StdVCL, TestExtensions,
  Forms, PSQLConnFrm, PSQLFields;

type

  //Setup decorator
  TDbSetup = class(TTestSetup)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TPSQLGuidField
  TestTPSQLGuidField = class(TTestCase)
  public
    procedure TearDown; override;
    procedure TestGUIDField;
    procedure TestGUIDInsert;
    procedure TestGUIDUpdate;
    procedure TestGUIDDelete;
  published
    procedure TestSelectUUID;
    procedure TesTGUIDField_ASCII;
    procedure TestGUIDInsert_ASCII;
    procedure TestGUIDUpdate_ASCII;
    procedure TestGUIDDelete_ASCII;
    procedure TestGUIDField_UTF8;
    procedure TestGUIDInsert_UTF8;
    procedure TestGUIDUpdate_UTF8;
    procedure TestGUIDDelete_UTF8;
  end;

  // Test methods for class TPSQLPointField
  TestTPSQLPointField = class(TTestCase)
  public
    procedure TearDown; override;
  published
    procedure TestSelectPoint;
    procedure TestInsertPoint;
    procedure TestUpdatePoint;
  end;

  // Test methods for class TPSQLRangeField
  TestTPSQLRangeField = class(TTestCase)
  public
    procedure TearDown; override;
  published
    procedure TestSelectEmptyRange;
    procedure TestSelectOpenRange;
    procedure TestSeelectClosedRange;
    procedure TestSelectUpperInfinityRange;
    procedure TestSelectLowerInfinityRange;
    procedure TestSelectRange;
    procedure TestInsertRange;
    procedure TestUpdateRange;
  end;

var
  FldDB: TPSQLDatabase;
  FldQry: TPSQLQuery;

implementation

uses TestHelper;

{$IFDEF DELPHI_5}
function CoCreateGuid(out guid: TGUID): HResult; stdcall; external 'ole32.dll' name 'CoCreateGuid';

function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(Guid);
end;
{$ENDIF}

procedure TestTPSQLGuidField.TearDown;
begin
 FldQry.Close;
 FldQry.SQL.Clear;
end;

procedure TestTPSQLGuidField.TestSelectUUID;
begin
  FldQry.SQL.Text := 'SELECT ''35c6c84e-4157-466c-0091-31a4714aca34''::uuid';
  FldQry.Open;
  Check(FldQry.Active, 'Cannot select UUID value');
end;

procedure TestTPSQLGuidField.TestGUIDDelete;
begin
  FldQry.RequestLive := True;
  FldQry.SQL.Text := 'SELECT * FROM uuid_test_case_table';
  FldQry.Open;
  FldQry.Delete;
  Check(FldQry.RowsAffected = 1, 'Cannot delete UUID ' + FldQry.Fields[0].ClassName);
end;

procedure TestTPSQLGuidField.TestGUIDDelete_ASCII;
begin
  fldDB.CharSet := 'SQL_ASCII';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDDelete;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDDelete;
end;

procedure TestTPSQLGuidField.TestGUIDDelete_UTF8;
begin
  fldDB.CharSet := 'UNICODE';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDDelete;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDDelete;
end;

procedure TestTPSQLGuidField.TestGUIDField;
var G1, G2: TGUID;
begin
  G1 := StringToGuid('{35c6c84e-4157-466c-0091-31a4714aca34}');
  FldQry.SQL.Text := 'SELECT ''35c6c84e-4157-466c-0091-31a4714aca34''::uuid';
  FldQry.Open;
  Check(FldQry.Active, 'Cannot select UUID value');
  Check(FldQry.Fields[0].AsString = UpperCase('{35c6c84e-4157-466c-0091-31a4714aca34}'), 'UUID value is corrupted in SQL_ASCII charset using TGUIDField');
  if FldQry.Fields[0] is TGUIDField then
   G2 := TGUIDField(FldQry.Fields[0]).AsGuid
  else
   G2 := TPSQLGUIDField(FldQry.Fields[0]).AsGuid;
  Check(IsEqualGUID(G1, G2), 'GUID comparison failed: ' + FldQry.Fields[0].ClassName);
end;

procedure TestTPSQLGuidField.TestGUIDInsert;
var G: TGUID;
begin
  FldQry.RequestLive := True;
  FldQry.SQL.Text := 'SELECT * FROM uuid_test_case_table';
  FldQry.Open;
  FldQry.Insert;
  Check(CreateGUID(G) = 0, 'GUID generation failed');
  if FldQry.Fields[0] is TGUIDField then
   TGUIDField(FldQry.Fields[0]).AsGuid := G
  else
   TPSQLGUIDField(FldQry.Fields[0]).AsGuid := G;
  FldQry.Post;
  Check(FldQry.RowsAffected = 1, 'Cannot insert UUID: ' + FldQry.Fields[0].ClassName);
end;

procedure TestTPSQLGuidField.TestGUIDInsert_ASCII;
begin
  fldDB.CharSet := 'SQL_ASCII';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDInsert;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDInsert;
end;

procedure TestTPSQLGuidField.TestGUIDInsert_UTF8;
begin
  fldDB.CharSet := 'UNICODE';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDInsert;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDInsert;
end;

procedure TestTPSQLGuidField.TestGUIDUpdate;
var G: TGUID;
begin
  FldQry.RequestLive := True;
  FldQry.SQL.Text := 'SELECT * FROM uuid_test_case_table';
  FldQry.Open;
  FldQry.Edit;
  CreateGUID(G);
  if FldQry.Fields[0] is TGUIDField then
   TGUIDField(FldQry.Fields[0]).AsGuid := G
  else
   TPSQLGUIDField(FldQry.Fields[0]).AsGuid := G;
  FldQry.Post;
  Check(FldQry.RowsAffected = 1, 'Cannot update UUID ' + FldQry.Fields[0].ClassName);
end;

procedure TestTPSQLGuidField.TestGUIDUpdate_ASCII;
begin
  fldDB.CharSet := 'SQL_ASCII';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDUpdate;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDUpdate;
end;

procedure TestTPSQLGuidField.TestGUIDUpdate_UTF8;
begin
  fldDB.CharSet := 'UNICODE';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDUpdate;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDUpdate;
end;

procedure TestTPSQLGuidField.TestGUIDField_ASCII;
begin
  fldDB.CharSet := 'SQL_ASCII';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDField;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDField;
end;

procedure TestTPSQLGuidField.TestGUIDField_UTF8;
begin
  fldDB.CharSet := 'UNICODE';
  FldQry.Options := FldQry.Options + [dsoUseGUIDField];
  TestGUIDField;
  FldQry.Options := FldQry.Options - [dsoUseGUIDField];
  TestGUIDField;
end;

{ TDbSetup }

procedure TDbSetup.SetUp;
begin
  inherited;
  SetUpTestDatabase(FldDB, 'PSQLQueryTest.conf');
  FldQry := TPSQLQuery.Create(nil);
  FldQry.Database := FldDB;
  FldQry.ParamCheck := False;
  FldDB.Execute('CREATE TEMP TABLE IF NOT EXISTS uuid_test_case_table(uuidf uuid NOT NULL PRIMARY KEY)');
  FldDB.Execute('CREATE TEMP TABLE IF NOT EXISTS geometry_test_case_table(id int4 PRIMARY KEY, p point, c circle, b box, l lseg)');
end;

procedure TDbSetup.TearDown;
begin
  inherited;
  FldDB.Close;
  ComponentToFile(FldDB, 'PSQLQueryTest.conf');
  FldQry.Free;
  FldDB.Free;
end;

{ TestTPSQLPointField }

procedure TestTPSQLPointField.TearDown;
begin
  inherited;
  FldQry.Close;
  FldQry.SQL.Clear;
end;

procedure TestTPSQLPointField.TestInsertPoint;
const
  P: TPSQLPoint = (X: 2.5; Y: 3.5);
begin
  FldQry.SQL.Text := 'SELECT * FROM geometry_test_case_table';
  FldQry.RequestLive := True;
  FldQry.Open;
  FldQry.Insert;
  FldQry.FieldByName('id').AsInteger := 1;
  TPSQLPointField(FldQry.FieldByName('p')).Value := P;
  FldQry.Post;
  Check(TPSQLPointField(FldQry.FieldByName('p')).Value = P, 'Wrong value for "point" field after insert');
end;

procedure TestTPSQLPointField.TestSelectPoint;
begin
  FldQry.SQL.Text := 'SELECT ''( 2.5 , 3.5 )''::point';
  FldQry.Open;
  Check(TPSQLPointField(FldQry.Fields[0]).Value.Y > TPSQLPointField(FldQry.Fields[0]).Value.X);
  Check(FldQry.Active, 'Cannot select "point" value');
end;

procedure TestTPSQLPointField.TestUpdatePoint;
const
  P: TPSQLPoint = (X: pi; Y: 2.818281828);
begin
  FldQry.SQL.Text := 'SELECT * FROM geometry_test_case_table';
  FldQry.RequestLive := True;
  FldQry.Open;
  if FldQry.RecordCount = 0 then TestInsertPoint;
  FldQry.Edit;
  TPSQLPointField(FldQry.FieldByName('p')).Value := P;
  FldQry.Post;
  Check(TPSQLPointField(FldQry.FieldByName('p')).Value = P, 'Wrong value for "point" field after update');
end;

{ TestTPSQLRangeField }

procedure TestTPSQLRangeField.TearDown;
begin
  inherited;
  FldQry.Close;
  FldQry.SQL.Clear;
end;

procedure TestTPSQLRangeField.TestInsertRange;
begin

end;

procedure TestTPSQLRangeField.TestSeelectClosedRange;
var
  i: Integer;
begin
  FldQry.SQL.Text := 'SELECT ''[3.3, 4.45]''::numrange, '+
                     ' ''[2010-01-01 14:45, 2010-01-01 15:45]''::tsrange,' +
                     ' ''[2010-01-01 14:45 UTC, 2010-01-01 15:45 PST]''::tstzrange,' +
                     ' ''[1, 2]''::int4range,' +
                     ' ''[22, 4567]::int8range,' +
                     ' ''[2010-01-01, 2010-01-12]::daterange';
  FldQry.Open;
  for i := 0 to FldQry.FieldCount - 1 do
   with (FldQry.Fields[i] as TPSQLRangeField).Value do
    Check((UpperBound.State = rbsInclusive) and
          (LowerBound.State = rbsInclusive), 'Range must be closed');
end;

procedure TestTPSQLRangeField.TestSelectEmptyRange;
var
  i: Integer;
begin
  FldQry.SQL.Text := 'SELECT ''empty''::numrange, '+
                     ' ''empty''::int4range,' +
                     ' ''empty''::int8range,' +
                     ' ''empty''::tsrange,' +
                     ' ''empty''::daterange,' +
                     ' ''empty''::tstzrange';
  FldQry.Open;
  for i := 0 to FldQry.FieldCount - 1 do
    Check((FldQry.Fields[i] as TPSQLRangeField).IsEmpty, 'Range field must be empty');
end;

procedure TestTPSQLRangeField.TestSelectLowerInfinityRange;
var
  i: Integer;
begin
  FldQry.SQL.Text := 'SELECT ''[ , 4.45]''::numrange, '+
                     ' ''[, 2010-01-01 15:45]''::tsrange,' +
                     ' ''[, 2010-01-01 15:45 PST]''::tstzrange,' +
                     ' ''[, 2]''::int4range,' +
                     ' ''[, 4567]::int8range,' +
                     ' ''[, 2010-01-12]::daterange';
  FldQry.Open;
  for i := 0 to FldQry.FieldCount - 1 do
   with (FldQry.Fields[i] as TPSQLRangeField).Value do
    Check((LowerBound.State = rbsInfinite), 'Range must gave infinite lower range');
end;

procedure TestTPSQLRangeField.TestSelectOpenRange;
var
  i: Integer;
begin
  FldQry.SQL.Text := 'SELECT ''(3,4)''::numrange, '+
                     ' ''(2010-01-01 14:45, 2010-01-01 15:45)''::tsrange,' +
                     ' ''(2010-01-01 14:45 UTC, 2010-01-01 15:45 PST)''::tstzrange';
  FldQry.Open;
  for i := 0 to FldQry.FieldCount - 1 do
   with (FldQry.Fields[i] as TPSQLRangeField).Value do
    Check((UpperBound.State = rbsExclusive) and
          (LowerBound.State = rbsExclusive), 'Range must be open');
end;

procedure TestTPSQLRangeField.TestSelectRange;
var R: TPSQLRange;
begin
  FldQry.SQL.Text := 'SELECT numrange(3.1, 5.2, ''()''), '+
                     ' int4range(1, 3, ''[)''), ' +
                     ' int4range(1, 1, ''()'') ';
  FldQry.Open;
  Check(FldQry.Active, 'Cannot select "point" value');
  R := TPSQLRangeField(FldQry.Fields[0]).Value;
  Check(not R.Empty, 'Range is empty');
  Check(R.LowerBound.State = rbsExclusive, 'Range lower bound must be exclusive');
  Check(R.UpperBound.State = rbsExclusive, 'Range lower bound must be exclusive');
  Check(R.LowerBound.AsFloat = 3.1, 'Wrong lower bound value');
  Check(R.UpperBound.AsFloat = 5.2, 'Wrong upper bound value');

  R := TPSQLRangeField(FldQry.Fields[1]).Value;
  Check(not R.Empty, 'Range is empty');
  Check(R.LowerBound.State = rbsInclusive, 'Range lower bound must be inclusive');
  Check(R.UpperBound.State = rbsExclusive, 'Range lower bound must be exclusive');
  Check(R.LowerBound.AsInteger = 1, 'Wrong lower bound value');
  Check(R.UpperBound.AsInteger = 3, 'Wrong upper bound value');

end;

procedure TestTPSQLRangeField.TestSelectUpperInfinityRange;
var
  i: Integer;
begin
  FldQry.SQL.Text := 'SELECT ''[3.3, ]''::numrange, '+
                     ' ''[2010-01-01 14:45, ]''::tsrange,' +
                     ' ''[2010-01-01 14:45 UTC, ]''::tstzrange,' +
                     ' ''[1, ]''::int4range,' +
                     ' ''[22, ]::int8range,' +
                     ' ''[2010-01-01, ]::daterange';
  FldQry.Open;
  for i := 0 to FldQry.FieldCount - 1 do
   with (FldQry.Fields[i] as TPSQLRangeField).Value do
    Check((UpperBound.State = rbsInfinite), 'Range must gave infinite upper range');
end;

procedure TestTPSQLRangeField.TestUpdateRange;
begin

end;

initialization
  //PaGo: Register any test cases with setup decorator
  RegisterTest(TDbSetup.Create(TestTPSQLGuidField.Suite));
  RegisterTest(TDbSetup.Create(TestTPSQLPointField.Suite));
  RegisterTest(TDbSetup.Create(TestTPSQLRangeField.Suite));
end.

