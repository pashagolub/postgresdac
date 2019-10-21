unit PSQLQueryTest;
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
  PSQLAccess, Classes, PSQLDbTables, PSQLTypes, SysUtils, Db, Data.FmtBcd,
  {$IFNDEF DELPHI_5}Variants{$ENDIF}
  {$IFNDEF DUNITX}
    ,TestFramework, Windows, ExtCtrls, Controls, DbCommon,
    Graphics, StdVCL, TestExtensions,
    Forms, TestHelper
  {$ELSE}
    ,DUnitX.TestFramework, TestXHelper
  {$ENDIF};

type

  {$IFDEF DUNITX}[TestFixture]{$ENDIF}
  TestTPSQLQuery = class({$IFNDEF DUNITX}TTestCase{$ELSE}TTestXCase{$ENDIF})
  private
    FPSQLQuery: TPSQLQuery;
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
  //parameters
    procedure CheckCorruptedParams;
  //dataset options
    procedure TestEmptyCharAsNullOption;
  //TField.AsXXX
    procedure TestAsInteger;
    procedure TestAsFloat;
    procedure TestAsString;
    procedure TestAsBoolean;
    procedure TestAsTime;
    procedure TestAsDate;
    procedure TestAsTimestamp;
  //RequestLive modifications
    procedure TestInsert;
    procedure TestUpdate;
    procedure TestDelete;
  //bookmarks
    procedure TestBookmarks;
    procedure TestOpenEmptyDataset;
  //locate
    procedure TestLocateStr;
    procedure TestLocateInt;
    procedure TestLookup;
  //TFieldDef properties populated
    procedure TestRequired;
  //date and time checks
    procedure TestTimeValues;
    procedure TestDateValues;
    procedure TestTimestampValues;
  //fetch on demand
    procedure TestFetchOnDemand;
    procedure TestFetchOnDemandExclusive;
  //dsoRefreshModifiedRecordOnly
    procedure TestRefreshModifiedInsert;
    procedure TestRefreshModifiedInsertNonEmptyTable;
    procedure TestRefreshModifiedUpdate;
    procedure TestRefreshModifiedDelete;
  //dsoNumericAsFloat
    procedure TestNumericAsFloat;
    procedure TestAffectedRows;
    {$IFDEF DUNITX}
    [SetupFixture]
    procedure SetupFixture;
    {$ENDIF}
  end;

implementation

uses DateUtils;

procedure InternalSetUp;
begin
end;

{$IFDEF DELPHI_5}
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  SecsPerHour   = SecsPerMin * MinsPerHour;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
  
function Today: TDateTime;
begin
  Result := Date;
end;

function Yesterday: TDateTime;
begin
  Result := Date - 1;
end;

function Tomorrow: TDateTime;
begin
  Result := Date + 1;
end;

function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
begin
  Result := (AValue >= Trunc(ABasis)) and
            (AValue < Trunc(ABasis) + 1);
end;

function IsToday(const AValue: TDateTime): Boolean;
begin
  Result := IsSameDay(AValue, Date);
end;

function TimeOf(const AValue: TDateTime): TDateTime;
begin
  Result := Frac(AValue);
end;

function DateTimeToMilliseconds(const ADateTime: TDateTime): Int64;
var
  LTimeStamp: TTimeStamp;
begin
  LTimeStamp := DateTimeToTimeStamp(ADateTime);
  Result := LTimeStamp.Date;
  Result := (Result * MSecsPerDay) + LTimeStamp.Time;
end;

function MinutesBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Abs(DateTimeToMilliseconds(ANow) - DateTimeToMilliseconds(AThen))
    div (MSecsPerSec * SecsPerMin);
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
var
  TS: TTimeStamp;
begin
  Result := False;
  if (Hour < HoursPerDay) and (Min < MinsPerHour) and (Sec < SecsPerMin) and (MSec < MSecsPerSec) then
  begin
    TS.Time :=  (Hour * (MinsPerHour * SecsPerMin * MSecsPerSec))
              + (Min * SecsPerMin * MSecsPerSec)
              + (Sec * MSecsPerSec)
              +  MSec;
    TS.Date := DateDelta; // This is the "zero" day for a TTimeStamp, days between 1/1/0001 and 12/30/1899 including the latter date
    Time := TimeStampToDateTime(TS);
    Result := True;
  end;
end;

function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word; out AValue: TDateTime): Boolean;
var
  LTime: TDateTime;
begin
  Result := TryEncodeDate(AYear, AMonth, ADay, AValue);
  if Result then
  begin
    Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
    if Result then
      if AValue >= 0 then
        AValue := AValue + LTime
      else
        AValue := AValue - LTime
  end;
end;

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  if not TryEncodeDateTime(AYear, AMonth, ADay,
                           AHour, AMinute, ASecond, AMilliSecond, Result) then
    raise EConvertError.Create('Cannot encode date');
end;
{$ENDIF}

procedure TestTPSQLQuery.CheckCorruptedParams;
begin
  FPSQLQuery.ParamCheck := True;
  FPSQLQuery.SQL.Text := 'SELECT now()::::TIMESTAMP(0) where AField = :AParam';
  Check(FPSQLQuery.ParamCount = 1, 'Wrong parameters parsing with ParamCheck');
end;

procedure TestTPSQLQuery.SetUp;
begin
  FPSQLQuery := TPSQLQuery.Create(nil);
  FPSQLQuery.Database := TestDBSetup.Database;
  FPSQLQuery.ParamCheck := False;
  TestDBSetup.Database.Execute('TRUNCATE requestlive_test');
end;

procedure TestTPSQLQuery.TearDown;
begin
{$IFNDEF NEXTGEN}
  FPSQLQuery.Free;
{$ELSE}
  FPSQLQuery.DisposeOf;
{$ENDIF};
  FPSQLQuery := nil;
end;

procedure TestTPSQLQuery.TestAffectedRows;
begin
  FPSQLQuery.SQL.Text := 'INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')';
  FPSQLQuery.ExecSQL;
  Check(FPSQLQuery.RowsAffected = 4, 'RowsAffected incorrect');
end;

procedure TestTPSQLQuery.TestAsBoolean;
begin
 FPSQLQuery.SQL.Text := 'SELECT True, False';
 FPSQLQuery.Open;
 Check(FPSQLQuery.Fields[0].AsBoolean and not FPSQLQuery.Fields[1].AsBoolean, 'Field value AsBoolean is incorrect');
 FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestAsDate;
begin
 FPSQLQuery.SQL.Text := 'SELECT current_date';
 FPSQLQuery.Open;
 Check(IsToday(FPSQLQuery.Fields[0].AsDateTime), 'Field value AsDate is incorrect');
 FPSQLQuery.Close
end;

procedure TestTPSQLQuery.TestAsFloat;
const D: Double = 12.8;
begin
 FPSQLQuery.SQL.Text := 'SELECT 12.8 :: float';
 FPSQLQuery.Open;
 Check(FPSQLQuery.Fields[0].AsFloat = D, 'Field value AsFloat is incorrect');
 FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestNumericAsFloat;
const D: Double = 12.8;
begin
 FPSQLQuery.Options := FPSQLQuery.Options + [dsoNumericAsFloat];
 FPSQLQuery.SQL.Text := 'SELECT 12.8 :: numeric';
 FPSQLQuery.Open;
 Check(FPSQLQuery.Fields[0] is TFloatField, 'Field shold be mapped as TFloatField');
 Check(FPSQLQuery.Fields[0].AsFloat = D, 'Field value AsFloat is incorrect');
 FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestAsInteger;
begin
 FPSQLQuery.SQL.Text := 'SELECT 12345';
 FPSQLQuery.Open;
 Check(FPSQLQuery.Fields[0].AsInteger = 12345, 'Field value AsInteger is incorrect');
 FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestAsString;
begin
 FPSQLQuery.SQL.Text := 'SELECT ''foo''::varchar(30), ''foo''::text ';
 FPSQLQuery.Open;
 Check(FPSQLQuery.Fields[0].AsString = 'foo', 'Field value AsString is incorrect');
 Check(FPSQLQuery.Fields[1].AsString = 'foo', 'Field memo value AsString is incorrect');
 FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestAsTime;
var ClientTime, ServerTime: TTime;
begin
 FPSQLQuery.SQL.Text := 'SELECT ''16:23''::time';
 FPSQLQuery.Open;
 ClientTime := Time();
 ServerTime := TimeOf(FPSQLQuery.Fields[0].AsDateTime);
 Check(StrToTime('16:23') = ServerTime), 'Field value AsTime is incorrect');
 FPSQLQuery.Close
end;

procedure TestTPSQLQuery.TestAsTimestamp;
begin
 FPSQLQuery.SQL.Text := 'SELECT LOCALTIMESTAMP';
 FPSQLQuery.Open;
 Check(MinutesBetween(Now(), FPSQLQuery.Fields[0].AsDateTime) < 5, 'Field value AsTimestamp is incorrect');
 FPSQLQuery.Close
end;

procedure TestTPSQLQuery.TestBookmarks;
var
   B: TBookmark;
   BookmarkedPos: longint;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM generate_series(1, 10)';
  FPSQLQuery.Open;
  FPSQLQuery.MoveBy(5);
  B := FPSQLQuery.GetBookmark;
  BookmarkedPos := FPSQLQuery.RecNo;
  FPSQLQuery.First;
  FPSQLQuery.GotoBookmark(B);
  Check(FPSQLQuery.RecNo = BookmarkedPos, 'GotoBookmark failed');
  Check(FPSQLQuery.BookmarkValid(B), 'BookmarkValid failed');
end;

procedure TestTPSQLQuery.TestDateValues;
  procedure CheckTime(ATime: TDateTime);
  begin
    FPSQLQuery.Edit;
    FPSQLQuery.FieldByName('intf').AsInteger := Random(MaxInt); //not null field
    FPSQLQuery.FieldByName('datef').AsDateTime := ATime;
    FPSQLQuery.Post;
    Check(FPSQLQuery.FieldByName('datef').AsDateTime = ATime, 'Cannot set DATE field to ' + DateTimeToStr(ATime));
  end;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  CheckTime(EncodeDate(1899, 12, 30));
  CheckTime(Date());
  CheckTime(EncodeDate(1623, 6, 19)); //Blaise Pascal was born this day
end;

procedure TestTPSQLQuery.TestDelete;
var aCount: integer;
begin
  TestDBSetup.Database.Execute('INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')');
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  aCount := FPSQLQuery.RecordCount;
  FPSQLQuery.Delete;
  Check(FPSQLQuery.RecordCount = aCount - 1, 'TPSQLQuery.Delete failed');
end;

procedure TestTPSQLQuery.TestEmptyCharAsNullOption;
begin
  FPSQLQuery.Options := [];
  FPSQLQuery.SQL.Text := 'SELECT ''''::varchar(30), ''text''::varchar(30) as col1';
  FPSQLQuery.Open;
  Check(not FPSQLQuery.Fields[0].IsNull, 'Field must be NOT NULL due to normal options');
  FPSQLQuery.Close;

  FPSQLQuery.Options := [dsoEmptyCharAsNull];
  FPSQLQuery.SQL.Text := 'SELECT ''''::varchar(30), ''text''::varchar(30) as col1';
  FPSQLQuery.Open;
  Check(FPSQLQuery.Fields[0].IsNull, 'IsNULL must be true due to dsoEmptyCharAsNull used');
  {$IFDEF DELPHI_12}
  Check(FPSQLQuery.Fields.FieldByName('col1').AsWideString = 'text', 'Field must be not empty if dsoEmptyCharAsNull enabled');
  {$ENDIF}
  FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestFetchOnDemand;
var T: cardinal;
  i: Integer;
begin
  FPSQLQuery.SQL.Text := 'SELECT g.s, repeat($$Pg$$, 25000) FROM generate_series(1, 250) g(s)';
  FPSQLQuery.Options := FPSQLQuery.Options + [dsoFetchOnDemand];
  T := GetTickCount();
  FPSQLQuery.Open;
  T := GetTickCount() - T;
  Check(T < 5000, 'Query should return control to application less then 5 seconds using fetch on demand');
  Check(FPSQLQuery.RecordCount = 1, 'Record count should be 1 since no other records fetched yet');
  for i := 1 to 125 do
    FPSQLQuery.Next;
  Check(FPSQLQuery.RecordCount = 126, 'Record count should be exactly the fetched rows number');
  FPSQLQuery.FetchAll;
  Check(FPSQLQuery.RecordCount = 250, 'Record count should equal to the result set size');
  FPSQLQuery.Close;
end;

procedure TestTPSQLQuery.TestFetchOnDemandExclusive;
var Q: TPSQLQuery;
begin
  Q := TPSQLQuery.Create(nil);
  try
    Q.SQL.Text := 'SELECT 1';
    Q.Database := TestDBSetup.Database;
    Q.Open;
    FPSQLQuery.SQL.Text := 'SELECT g.s, repeat($$Pg$$, 25000) FROM generate_series(1, 250) g(s)';
    FPSQLQuery.Options := FPSQLQuery.Options + [dsoFetchOnDemand];
    {$IFNDEF DUNITX}
    CheckException(TestFetchOnDemand, EDatabaseError, 'Open should failed for non-exsclusive use of Database');
    {$ELSE}
    Assert.WillRaise(TestFetchOnDemand, EDatabaseError, 'Open should failed for non-exsclusive use of Database');
    {$ENDIF}
  finally
    {$IFNDEF NEXTGEN}
    FreeAndNil(Q);
    {$ELSE}
    Q.DisposeOf;
    {$ENDIF}
  end;
end;

procedure TestTPSQLQuery.TestInsert;
var _Num: TBcd;
begin
  _Num := StrToBcd('98765432100123456789.98765432100123456789', PSQL_FS);
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  FPSQLQuery.Insert;
  FPSQLQuery.FieldByName('intf').AsInteger := Random(MaxInt);
  FPSQLQuery.FieldByName('string').AsString := 'test test';
  FPSQLQuery.FieldByName('datum').AsDateTime := Now();
  FPSQLQuery.FieldByName('b').AsBoolean := Boolean(Random(1));
  FPSQLQuery.FieldByName('floatf').AsFloat := {$IFDEF DELPHI_12}Random(){$ELSE}Random(MaxInt) / Random(MaxInt){$ENDIF};
  FPSQLQuery.FieldByName('numf').AsBCD := _Num;
  FPSQLQuery.Post;
  Check(FPSQLQuery.RecordCount = 1, 'TPSQLQuery.Insert failed');
  Check(_Num = FPSQLQuery.FieldByName('numf').AsBCD, 'Incorrect value for NUMERIC');
end;

procedure TestTPSQLQuery.TestLocateInt;
begin
  FPSQLQuery.SQL.Text := 'SELECT col1 FROM generate_series(11, 16) AS c(col1)';
  FPSQLQuery.Open;
  Check(FPSQLQuery.Locate('col1', '12', []), 'Locate failed with default options');
  Check(FPSQLQuery.RecNo = 2, 'Locate positioning failed with default options');

  Check(FPSQLQuery.Locate('col1', '13', [loPartialKey]), 'Locate failed with loPartialKey option');
  Check(FPSQLQuery.RecNo = 3, 'Locate positioning failed with loPartialKey option');

  Check(FPSQLQuery.Locate('col1', '14', [loCaseInsensitive]), 'Locate failed with loCaseInsensitive option');
  Check(FPSQLQuery.RecNo = 4, 'Locate positioning failed with loCaseInsensitive option');

  Check(FPSQLQuery.Locate('col1', '15', [loCaseInsensitive, loPartialKey]), 'Locate failed with full options');
  Check(FPSQLQuery.RecNo = 5, 'Locate positioning failed with full options');
end;

procedure TestTPSQLQuery.TestLocateStr;
begin
  FPSQLQuery.SQL.Text := 'SELECT col1, cash_words(col1::money)::varchar(50) AS col2 FROM generate_series(1, 6) AS g(col1)';
  FPSQLQuery.Open;
//single column
  Check(FPSQLQuery.Locate('col2', 'Two dollars and zero cents', []) and (FPSQLQuery.RecNo = 2),
          'Locate failed with default options');
  Check(FPSQLQuery.Locate('col2', 'Thr', [loPartialKey]) and (FPSQLQuery.RecNo = 3),
          'Locate failed with loPartialKey option');
  Check(FPSQLQuery.Locate('col2', 'FiV', [loCaseInsensitive, loPartialKey]) and (FPSQLQuery.RecNo = 5),
          'Locate failed with full options');
//multicolumn
  Check(FPSQLQuery.Locate('col1;col2', VarArrayOf([2, 'Two dollars and zero cents']), []) and (FPSQLQuery.RecNo = 2),
          'Multicolumn Locate failed with default options');
  Check(FPSQLQuery.Locate('col1;col2', VarArrayOf([3, 'Thr']), [loPartialKey]) and (FPSQLQuery.RecNo = 3),
          'Multicolumn Locate failed with loPartialKey option');
  Check(FPSQLQuery.Locate('col1;col2', VarArrayOf([5, 'FiV']), [loCaseInsensitive, loPartialKey]) and (FPSQLQuery.RecNo = 5),
          'Multicolumn Locate failed with full options');
end;

procedure TestTPSQLQuery.TestLookup;
var a, b: integer;
begin
  FPSQLQuery.SQL.Text := 'SELECT col1, cash_words(col1::money)::varchar(50) AS col2 FROM generate_series(1, 6) AS g(col1)';
  FPSQLQuery.Open;
//single column
  Check(FPSQLQuery.Lookup('col2', 'Two dollars and zero cents', 'col1') = 2,
          'Lookup failed');
  Check(FPSQLQuery.Lookup('col1', 2, 'col2') = 'Two dollars and zero cents',
          'Locate failed');

//multicolumn
  FPSQLQuery.SQL.Text := 'SELECT a, b, a*b AS res FROM generate_series(1,10) v(a), generate_series(1,10) w(b)';
  FPSQLQuery.Open;
  a := Random(10) + 1; b := Random(10) + 1;
  Check(FPSQLQuery.Lookup('a;b', VarArrayOf([IntToStr(a), b]), 'res') = a*b,
          'Multicolumn Lookup failed');
  a := Random(10) + 1; b := Random(10) + 1;
  Check(FPSQLQuery.Lookup('a;b', VarArrayOf([a, b]), 'res') = a*b,
          'Multicolumn Lookup failed');
  a := Random(10) + 1; b := Random(10) + 1;
  Check(FPSQLQuery.Lookup('a;b', VarArrayOf([IntToStr(a), IntToStr(b)]), 'res') = a*b,
          'Multicolumn Lookup failed');
end;

procedure TestTPSQLQuery.TestOpenEmptyDataset;
begin
  FPSQLQuery.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS empty_table( id int4, ss varchar, num numeric, dt timestamp)');
  FPSQLQuery.SQL.Text := 'SELECT * FROM empty_table';
  FPSQLQuery.Open;
  FPSQLQuery.Last;
  FPSQLQuery.Refresh;
end;

procedure TestTPSQLQuery.TestRefreshModifiedDelete;
var anID, aRecordCount: integer;
begin
  TestDBSetup.Database.Execute('INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')');
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Options := FPSQLQuery.Options + [dsoRefreshModifiedRecordOnly];
  FPSQLQuery.Open;
  FPSQLQuery.RecNo := Random(FPSQLQuery.RecordCount);
  anID := FPSQLQuery.FieldByName('id').AsInteger;
  aRecordCount := FPSQLQuery.RecordCount;
  FPSQLQuery.Delete;
  Check(aRecordCount - 1 = FPSQLQuery.RecordCount, 'Nothing deleted');
  Check(not FPSQLQuery.Locate('id', anID, []), 'Row not deleted properly');
end;

procedure TestTPSQLQuery.TestRefreshModifiedInsert;
var RowCount: integer;
    iVal: integer;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Options := FPSQLQuery.Options + [dsoRefreshModifiedRecordOnly];
  FPSQLQuery.Open;
  RowCount := FPSQLQuery.RecordCount;
  FPSQLQuery.Insert;
  iVal := Random(MaxInt);
  FPSQLQuery.FieldByName('intf').AsInteger := iVal;
  FPSQLQuery.FieldByName('string').AsString := 'test test inserted';
  FPSQLQuery.Post;

  Check(RowCount + 1 = FPSQLQuery.RecordCount, 'Nothing inserted');
  Check(FPSQLQuery.FieldByName('intf').AsInteger = iVal, 'Integer value is wrong after Insert');
  Check(FPSQLQuery.FieldByName('string').AsString = 'test test inserted', 'String value is wrong after Insert');
  Check(FPSQLQuery.FieldByName('datum').IsNull, 'Datum value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('notes').IsNull, 'Notes value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('graphic').IsNull, 'Graphic value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('b_graphic').IsNull, 'b_graphic value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('b').IsNull, 'b value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('floatf').IsNull, 'floatf value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('datef').IsNull, 'datef value must be NULL after Insert');
  Check(FPSQLQuery.FieldByName('timef').IsNull, 'timef value must be NULL after Insert');
end;

procedure TestTPSQLQuery.TestRefreshModifiedInsertNonEmptyTable;
begin
  TestDBSetup.Database.Execute('INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')');
  TestRefreshModifiedInsert();
end;

procedure TestTPSQLQuery.TestRefreshModifiedUpdate;
var
  iVal: Integer;
begin
  TestDBSetup.Database.Execute('INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')');
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Options := FPSQLQuery.Options + [dsoRefreshModifiedRecordOnly];
  FPSQLQuery.Open;
  FPSQLQuery.RecNo := Random(FPSQLQuery.RecordCount);
  FPSQLQuery.Edit;
  iVal := Random(MaxInt);
  FPSQLQuery.FieldByName('intf').AsInteger := iVal;
  FPSQLQuery.FieldByName('string').AsString := 'test test updated';
  FPSQLQuery.Post;
  Check(FPSQLQuery.FieldByName('intf').AsInteger = iVal, 'Integer value is wrong');
  Check(FPSQLQuery.FieldByName('string').AsString = 'test test updated', 'String value is wrong');
  Check(FPSQLQuery.FieldByName('datum').IsNull, 'Datum value must be NULL');
  Check(FPSQLQuery.FieldByName('notes').IsNull, 'Notes value must be NULL');
  Check(FPSQLQuery.FieldByName('graphic').IsNull, 'Graphic value must be NULL');
  Check(FPSQLQuery.FieldByName('b_graphic').IsNull, 'b_graphic value must be NULL');
  Check(FPSQLQuery.FieldByName('b').IsNull, 'b value must be NULL');
  Check(FPSQLQuery.FieldByName('floatf').IsNull, 'floatf value must be NULL');
  Check(FPSQLQuery.FieldByName('datef').IsNull, 'datef value must be NULL');
  Check(FPSQLQuery.FieldByName('timef').IsNull, 'timef value must be NULL');
end;

procedure TestTPSQLQuery.TestRequired;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test'; //single table query
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  Check(not FPSQLQuery.FieldDefs[0].Required, 'SERIAL should be not Required field');
  Check(FPSQLQuery.FieldDefs[1].Required, 'NOT NULL should be Required field');
  Check(not FPSQLQuery.FieldDefs[2].Required, 'NOT NULL + DEFAULT should be not Required field');
  Check(not FPSQLQuery.FieldDefs[3].Required, 'Simple definition should be not Required field');
  Check(not FPSQLQuery.FieldDefs[4].Required, 'Simple definition should be not Required field');
  FPSQLQuery.Close;
  FPSQLQuery.SQL.Text := 'SELECT r1.id, r1.intf, r1.string, r1.datum, ' +
                         'r2.id, r2.intf, r2.string, r2.datum ' +
                         'FROM requestlive_test r1, required_test r2'; //multi table query
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  Check(not FPSQLQuery.FieldDefs[0].Required, 'SERIAL should be not Required field');
  Check(FPSQLQuery.FieldDefs[1].Required, 'NOT NULL should be Required field');
  Check(not FPSQLQuery.FieldDefs[2].Required, 'NOT NULL + DEFAULT should be not Required field');
  Check(not FPSQLQuery.FieldDefs[3].Required, 'Simple definition should be not Required field');
  Check(not FPSQLQuery.FieldDefs[4+0].Required, 'SERIAL should be not Required field');
  Check(FPSQLQuery.FieldDefs[4+1].Required, 'NOT NULL should be Required field');
  Check(not FPSQLQuery.FieldDefs[4+2].Required, 'NOT NULL + DEFAULT should be not Required field');
  Check(not FPSQLQuery.FieldDefs[4+3].Required, 'Simple definition should be not Required field');
  FPSQLQuery.Close;
   
end;

procedure TestTPSQLQuery.TestTimestampValues;
  procedure CheckTimestamp(ATime: TDateTime);
  begin
    FPSQLQuery.Edit;
    FPSQLQuery.FieldByName('intf').AsInteger := Random(MaxInt); //not null field
    FPSQLQuery.FieldByName('datum').AsDateTime := ATime;
    FPSQLQuery.Post;
    Check(FPSQLQuery.FieldByName('datum').AsDateTime = ATime, 'Cannot set TIMESTAMP field to ' + DateTimeToStr(ATime));
  end;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  CheckTimestamp(Yesterday() + EncodeTime(0, 0, 0, 0));
  CheckTimestamp(Tomorrow() + EncodeTime(12, 0, 0, 0));
  CheckTimestamp(EncodeDate(1899, 12, 30) + EncodeTime(0, 0, 0, 0));
  CheckTimestamp(EncodeDate(1899, 12, 30) + EncodeTime(0, 0, 0, 100));
  CheckTimestamp(EncodeDate(1899, 12, 30) + EncodeTime(0, 0, 0, 1));
  CheckTimestamp(EncodeDate(1899, 12, 30) + EncodeTime(23, 59, 59, 999));
  CheckTimestamp(EncodeDateTime(1623, 6, 19, 12, 00, 00, 20)); //Blaise Pascal was born this day
end;

procedure TestTPSQLQuery.TestTimeValues;
  procedure CheckTime(ATime: TDateTime);
  begin
    FPSQLQuery.Edit;
    FPSQLQuery.FieldByName('intf').AsInteger := Random(MaxInt); //not null field
    FPSQLQuery.FieldByName('timef').AsDateTime := ATime;
    FPSQLQuery.Post;
    Check(FPSQLQuery.FieldByName('timef').AsDateTime = ATime, 'Cannot set TIME field to ' + DateTimeToStr(ATime));
  end;
begin
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  CheckTime(EncodeTime(0, 0, 0, 0));
  CheckTime(EncodeTime(0, 0, 0, 1));
  CheckTime(EncodeTime(0, 0, 0, 100));
  CheckTime(EncodeTime(23, 59, 59, 999));
end;

procedure TestTPSQLQuery.TestUpdate;
var _Num: TBcd;
begin
  _Num := StrToBcd('98765432100123456789.98765432100123456789', PSQL_FS);
  TestDBSetup.Database.Execute('INSERT INTO requestlive_test(intf, string) VALUES '+
                ' (1, ''test insert1''),' +
                ' (2, ''test insert2''),' +
                ' (3, ''test insert3''),' +
                ' (4, ''test insert4'')');
  FPSQLQuery.SQL.Text := 'SELECT * FROM requestlive_test';
  FPSQLQuery.RequestLive := True;
  FPSQLQuery.Open;
  FPSQLQuery.RecNo := Random(FPSQLQuery.RecordCount);
  FPSQLQuery.Edit;
  FPSQLQuery.FieldByName('intf').AsInteger := Random(MaxInt);
  FPSQLQuery.FieldByName('string').AsString := 'test test updated';
  FPSQLQuery.FieldByName('datum').AsDateTime := Now();
  FPSQLQuery.FieldByName('b').AsBoolean := Boolean(Random(1));
  FPSQLQuery.FieldByName('floatf').AsFloat := {$IFDEF DELPHI_12}Random(){$ELSE}Random(MaxInt) / Random(MaxInt){$ENDIF};
  FPSQLQuery.FieldByName('numf').AsBCD := _Num;
  FPSQLQuery.Post;
  Check(FPSQLQuery.FieldByName('string').AsString = 'test test updated', 'Incorrect value for VARCHAR after Update');
  Check(_Num = FPSQLQuery.FieldByName('numf').AsBCD, 'Incorrect value for NUMERIC after Update');
end;

{$IFDEF DUNITX}
procedure TestTPSQLQuery.SetupFixture;
begin
  InternalSetUp;
end;
{$ENDIF}

initialization
{$IFDEF DUNITX}
  TDUnitX.RegisterTestFixture(TestTPSQLQuery);
{$ENDIF}

end.

