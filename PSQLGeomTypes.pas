{$I pSQLDAC.inc}
unit PSQLGeomTypes;

interface

{$IFDEF DELPHI_12}
uses Types;

type
  TPSQLRangeBoundState = (rbsExclusive, rbsInclusive, rbsInfinite);

  TPSQLRangeBoundValue = record
  case Integer of
      0: (IVal: Integer);
      1: (FVal: Double);
      2: (DVal: TDateTime);
      3: (LVal: Int64);
    //4: (TVal: TSQLTimeStamp); for future use
  end;
  PPSQLRangeBoundValue = ^TPSQLRangeBoundValue;

  TPSQLRangeBound = packed record
  strict private
    var Value: TPSQLRangeBoundValue;
  public
    State: TPSQLRangeBoundState;
    property AsInteger: integer read Value.IVal write Value.IVal;
    property AsFloat: double read Value.FVal write Value.FVal;
    property AsLargeInt: Int64 read Value.LVal write Value.LVal;
    property AsDateTime: TDateTime read Value.DVal write Value.DVal;
    //property AsSQLTimeStamp: TSQLTimeStamp read Value.TVal write Value.TVal; for future use
    class operator Equal(RB1: TPSQLRangeBound; RB2: TPSQLRangeBound): Boolean;
  end;

  TPSQLRange = packed record
  private
    function GetEmpty: boolean;
  public
    LowerBound: TPSQLRangeBound;
    UpperBound: TPSQLRangeBound;
    property Empty: boolean read GetEmpty;
    procedure SetEmpty;
    class operator Equal(R1: TPSQLRange; R2: TPSQLRange): Boolean;
    constructor Create(const Value: string; const RangeType: cardinal);
  end;

  TPSQLPoint = packed record
    X: Double;
    Y: Double;
    class operator Equal(P1: TPSQLPoint; P2: TPSQLPoint): Boolean;
    class operator Implicit(P: TPoint): TPSQLPoint;
  end;

  TPSQLCircle = packed record
    R: Double;
    class operator Equal(C1: TPSQLCircle; C2: TPSQLCircle): Boolean;
    case Integer of
      0: (X, Y: Double);
      1: (Center: TPSQLPoint);
  end;

  TPSQLBox = packed record
    class operator Equal(B1: TPSQLBox; B2: TPSQLBox): Boolean;
    case Integer of
      0: (Right, Top, Left, Bottom: Double);
      1: (TopRight, BottomLeft: TPSQLPoint);
  end;

  TPSQLLSeg = packed record
    class operator Equal(L1: TPSQLLSeg; L2: TPSQLLSeg): Boolean;
    case Integer of
      0: (X1, Y1, X2, Y2: Double);
      1: (P1, P2: TPSQLPoint);
  end;

function SQLPointToPoint(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLPoint;
function PointToSQLPoint(Value: TPSQLPoint; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLCircleToCircle(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLCircle;
function CircleToSQLCircle(Value: TPSQLCircle; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLBoxToBox(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLBox;
function BoxToSQLBox(Value: TPSQLBox; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLLSegToLSeg(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLLSeg;
function LSegToSQLLSeg(Value: TPSQLLSeg; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLRangeToRange(Value: string; const RangeType: cardinal; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLRange;
function RangeToSQLRange(Value: TPSQLRange; const RangeType: cardinal; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): string;
{$ENDIF DELPHI_12}

implementation

{$IFDEF DELPHI_12}
uses PSQLAccess, PSQLTypes, SysUtils, StrUtils, Math;

function SQLPointToPoint(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLPoint;
var S, Xs, Ys: string;
    DelimPos: integer;
begin
 S := Copy(Value, 2, Length(Value) - 2); //eliminate brackets
 DelimPos := Pos(Delimiter, S);
 Xs := Copy(S, 1, DelimPos - 1);
 Ys := Copy(S, DelimPos + 1, MaxInt);
 if not UseSystemSeparator then
   begin
    Result.X := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Xs, PSQL_FS);
    Result.Y := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Ys, PSQL_FS);
   end
 else
   begin
    Result.X := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Xs);
    Result.Y := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Ys);
   end;
end;

function PointToSQLPoint(Value: TPSQLPoint; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  if UseSystemSeparator then
    Result := Format('(%g' + Delimiter +'%g)', [Value.X, Value.Y])
  else
    Result := '(' + SQLFloatToStr(Value.X) + Delimiter + SQLFloatToStr(Value.Y) + ')';
end;

function SQLCircleToCircle(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLCircle;
var S, Rs: string;
    DelimPos: integer;
begin
 S := Copy(Value, 2, Length(Value) - 2); //eliminate <> brackets
 DelimPos := Pos(')', S);
 Result.Center := SQLPointToPoint(Copy(S, 1, DelimPos), Delimiter, UseSystemSeparator);
 Rs := Copy(S, DelimPos + 2, MaxInt); //closing bracket plus delimiter
 if not UseSystemSeparator then
    Result.R := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Rs, PSQL_FS)
 else
    Result.R := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Rs);
end;

function CircleToSQLCircle(Value: TPSQLCircle; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := PointToSQLPoint(Value.Center, Delimiter, UseSystemSeparator);
  with Value do
    if UseSystemSeparator then
      Result := '<' + Result + Delimiter + Format('%g>', [R])
    else
      Result := '<' + Result + Delimiter + SQLFloatToStr(R) + '>';
end;

function SQLBoxToBox(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLBox;
var DelimPos: integer;
begin
  DelimPos := Pos(')', Value) + 1;
  Result.TopRight := SQLPointToPoint(Copy(Value, 1, DelimPos - 1), Delimiter, UseSystemSeparator);
  Result.BottomLeft := SQLPointToPoint(Copy(Value, DelimPos + 1, MaxInt), Delimiter, UseSystemSeparator);
end;

function BoxToSQLBox(Value: TPSQLBox; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := PointToSQLPoint(Value.TopRight, Delimiter, UseSystemSeparator) + Delimiter + PointToSQLPoint(Value.BottomLeft, Delimiter, UseSystemSeparator);
end;

function SQLLSegToLSeg(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLLSeg;
var
  DelimPos: integer;
  S: string;
begin
  S := Copy(Value, 2, Length(Value) - 2); //eliminate [] brackets
  DelimPos := Pos(')', S) + 1;
  Result.P1 := SQLPointToPoint(Copy(S, 1, DelimPos - 1), Delimiter, UseSystemSeparator);
  Result.P2 := SQLPointToPoint(Copy(S, DelimPos + 1, MaxInt), Delimiter, UseSystemSeparator);
end;

function LSegToSQLLSeg(Value: TPSQLLSeg; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := '[' + PointToSQLPoint(Value.P1, Delimiter, UseSystemSeparator) + Delimiter + PointToSQLPoint(Value.P2, Delimiter, UseSystemSeparator) + ']';
end;

function SQLRangeToRange(Value: string; const RangeType: cardinal; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLRange;
var
  DelimPos: integer;

  procedure SetBound(var B: TPSQLRangeBound; SVal: string);
  begin
    if SVal = EmptyStr then
      B.State := rbsInfinite
    else
      case RangeType of
        FIELD_TYPE_NUMRANGE:  if not UseSystemSeparator then
                                B.AsFloat := StrToFloat(SVal, PSQL_FS)
                              else
                                B.AsFloat := StrToFloat(SVal);
        FIELD_TYPE_INT4RANGE: B.AsInteger  := StrToInt(SVal);
        FIELD_TYPE_INT8RANGE: B.AsLargeInt := StrToInt64(SVal);
        FIELD_TYPE_DATERANGE: if UseSystemSeparator then
                                B.AsDateTime := StrToDate(SVal)
                              else
                                B.AsDateTime := SqlDateToDateTime(SVal, False);
        FIELD_TYPE_TSRANGE,
        FIELD_TYPE_TSTZRANGE: if UseSystemSeparator then
                                B.AsDateTime := StrToDateTime(SVal)
                              else
                                B.AsDateTime := SQLTimeStampToDateTime(SVal);
      end;
  end;

begin
  Result := Default(TPSQLRange);
  if SameText(Value, 'empty') or (Length(Value) <= 2) then
    Result.SetEmpty
  else
  begin
    Result.LowerBound.State := TPSQLRangeBoundState(ifthen(Value[1] = '[', ord(rbsInclusive), ord(rbsExclusive)));
    Result.UpperBound.State := TPSQLRangeBoundState(ifthen(Value[Length(Value)] = ']', ord(rbsInclusive), ord(rbsExclusive)));
    Value := Copy(Value, 2, Length(Value) - 2); //eliminate brackets
    DelimPos := Pos(Delimiter, Value);
    SetBound(Result.LowerBound, AnsiDequotedStr(Copy(Value, 1, DelimPos - 1), '"')); // eliminate quotes if needed
    SetBound(Result.UpperBound, AnsiDequotedStr(Copy(Value, DelimPos + 1, MaxInt), '"'));
  end;
end;

function RangeToSQLRange(Value: TPSQLRange; const RangeType: cardinal; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): string;

  function GetBound(const B: TPSQLRangeBound): string;
  begin
    case RangeType of
      FIELD_TYPE_INT4RANGE: Result := IntToStr(B.AsInteger);
      FIELD_TYPE_INT8RANGE: Result := IntToStr(B.AsLargeInt);
      FIELD_TYPE_NUMRANGE:  if UseSystemSeparator then
                              Result := FloatToStr(B.AsFloat)
                            else
                              Result := SQLFloatToStr(B.AsFloat);
      FIELD_TYPE_DATERANGE: if UseSystemSeparator then
                              Result := DateToStr(B.AsDateTime)
                            else
                              Result := DateTimeToSqlDate(B.AsDateTime, DATE_MODE);
      FIELD_TYPE_TSRANGE,
      FIELD_TYPE_TSTZRANGE: if UseSystemSeparator then
                              Result := DateTimeToStr(B.AsDateTime)
                            else
                              Result := DateTimeToSqlDate(B.AsDateTime, TIMESTAMP_MODE);
    end;
  end;

begin
  if Value.Empty then Exit('empty');
  if Value.LowerBound.State = rbsInfinite then
    Result := '['
  else
  begin
    Result := ifthen(Value.LowerBound.State = rbsInclusive, '[', '(');
    Result := Result + GetBound(Value.LowerBound);
  end;
  Result := Result + Delimiter;
  if Value.UpperBound.State = rbsInfinite then
    Result := Result + ']'
  else
  begin
    Result := Result + GetBound(Value.UpperBound);
    Result := Result + ifthen(Value.UpperBound.State = rbsInclusive, ']', ')');
  end;
end;

{ TPSQLRange }

class operator TPSQLRangeBound.Equal(RB1: TPSQLRangeBound; RB2: TPSQLRangeBound): Boolean;
begin
  Result := (RB1.State = RB2.State)
        and CompareMem(@RB1.Value, @RB2.Value, SizeOf(TPSQLRangeBoundValue)); //timestamp is the biggest field in record
end;

constructor TPSQLRange.Create(const Value: string; const RangeType: cardinal);
begin
  case RangeType of
    FIELD_TYPE_DATERANGE,
    FIELD_TYPE_NUMRANGE,
    FIELD_TYPE_INT4RANGE,
    FIELD_TYPE_INT8RANGE,
    FIELD_TYPE_TSRANGE,
    FIELD_TYPE_TSTZRANGE: Self := SQLRangeToRange(Value, RangeType);
  else
    raise EConvertError.Create(SInvalidRangeType);
  end;
end;

class operator TPSQLRange.Equal(R1: TPSQLRange; R2: TPSQLRange): Boolean;
begin
  Result := (R1.Empty = R2.Empty) and (R1.LowerBound = R2.LowerBound) and (R1.UpperBound = R2.UpperBound);
end;

function TPSQLRange.GetEmpty: boolean;
begin
  Result := (LowerBound.State = rbsExclusive) and (LowerBound = UpperBound);
end;

{ TPSQLPoint }

class operator TPSQLPoint.Equal(P1, P2: TPSQLPoint): Boolean;
begin
  Result := SameValue(P1.X, P2.X, 0) and SameValue(P1.Y, P2.Y, 0);
end;

class operator TPSQLPoint.Implicit(P: TPoint): TPSQLPoint;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

{ TPSQLCircle }

class operator TPSQLCircle.Equal(C1: TPSQLCircle; C2: TPSQLCircle): Boolean;
begin
  Result := (C1.Center = C2.Center) and SameValue(C1.R, C2.R);
end;

{ TPSQLBox }

class operator TPSQLBox.Equal(B1, B2: TPSQLBox): Boolean;
begin
  Result := (B1.TopRight = B2.TopRight) and (B1.BottomLeft = B2.BottomLeft);
end;

{ TPSQLLSeg }

class operator TPSQLLSeg.Equal(L1: TPSQLLSeg; L2: TPSQLLSeg): Boolean;
begin
  Result := (L1.P1 = L2.P1) and (L1.P2 = L2.P2);
end;

{TPSQLRange}

procedure TPSQLRange.SetEmpty;
begin
  UpperBound.State := rbsExclusive;
  LowerBound.State := rbsExclusive;
//  UpperBound.AsSQLTimeStamp := NullSQLTimeStamp;
//  LowerBound.AsSQLTimeStamp := NullSQLTimeStamp;
end;

{$ENDIF DELPHI_12}


end.
