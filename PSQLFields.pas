{$I pSQLDAC.inc}
unit PSQLFields;

interface

uses {$IFDEF UNDER_DELPHI_6}Windows{$ELSE}Types{$ENDIF},
     Classes, DB, PSQLTypes
     {$IFDEF DELPHI_12}, PSQLGeomTypes{$ENDIF};

{$IFDEF DELPHI_12}
  {$NOINCLUDE PSQLGeomTypes}
{$ENDIF}

type

 { TPSQLPGuidField }

  TPSQLGuidField = class(TGuidField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsGuid(): TGUID;
    procedure SetAsGuid(const Value: TGUID);
  public
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
  end;

{$IFDEF DELPHI_12}

 { TPSQLPointField }

  TPSQLPointField = class(TNumericField)
  private
    function GetAsTPoint: TPoint;
    procedure SetAsTPoint(const Value: TPoint);
  protected
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;    
    function GetValue(var Value: TPSQLPoint): Boolean;
    function GetAsPoint: TPSQLPoint;
    function GetAsString: string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsPoint(const Value: TPSQLPoint);
    procedure SetAsString(const Value: string); override;
  public
    property AsTPoint: TPoint read GetAsTPoint write SetAsTPoint;
    property Value: TPSQLPoint read GetAsPoint write SetAsPoint;
  end;

 { TPSQLCircleField }

  TPSQLCircleField = class(TNumericField)
  protected
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;    
    function GetValue(var Value: TPSQLCircle): Boolean;
    function GetAsCircle: TPSQLCircle;
    function GetAsString: string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsCircle(const Value: TPSQLCircle);
    procedure SetAsString(const Value: string); override;
  public
    property Value: TPSQLCircle read GetAsCircle write SetAsCircle;
  end;

 { TPSQLBoxField }

  TPSQLBoxField = class(TNumericField)
  private
    function GetAsTRect: TRect;
    procedure SetAsTRect(const Value: TRect);
  protected
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetValue(var Value: TPSQLBox): Boolean;
    function GetAsBox: TPSQLBox;
    function GetAsString: string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsBox(const Value: TPSQLBox);
    procedure SetAsString(const Value: string); override;
  public
    property AsTRect: TRect read GetAsTRect write SetAsTRect;
    property Value: TPSQLBox read GetAsBox write SetAsBox;
  end;

 { TPSQLLSegField }

  TPSQLLSegField = class(TNumericField)
  protected
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetValue(var Value: TPSQLLSeg): Boolean;
    function GetAsLSeg: TPSQLLSeg;
    function GetAsString: string; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsLSeg(const Value: TPSQLLSeg);
    procedure SetAsString(const Value: string); override;
  public
    property Value: TPSQLLSeg read GetAsLSeg write SetAsLSeg;
  end;

  { TPSQLRangeField }

  TPSQLRangeField = class(TNumericField)
  private
    function GetValue(var Value: TPSQLRange): Boolean; inline;
    function GetAsRange: TPSQLRange;
    procedure SetAsRange(const Value: TPSQLRange);
    function GetNativeRangeType: cardinal;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    function GetDefaultWidth: Integer; override;
  public
    function IsDiscrete: boolean; virtual;
    function IsEmpty: boolean; virtual;
    property Value: TPSQLRange read GetAsRange write SetAsRange;
  end;
  
{$ENDIF DELPHI_12}

{$IFDEF DELPHI_17}
  TPSQLBitConverter = class(TBitConverter)
    class function ToPSQLPoint(Value: TArray<Byte>): TPSQLPoint;
    class function ToPSQLCircle(Value: TArray<Byte>): TPSQLCircle;
    class function ToPSQLBox(Value: TArray<Byte>): TPSQLBox;
    class function ToPSQLLSeg(Value: TArray<Byte>): TPSQLLSeg;
    class function ToPSQLRange(Value: TArray<Byte>): TPSQLRange;
    class procedure FromPSQLPoint(Value: TPSQLPoint; Buffer: TArray<Byte>);
    class procedure FromPSQLCircle(Value: TPSQLCircle; Buffer: TArray<Byte>);
    class procedure FromPSQLBox(Value: TPSQLBox; Buffer: TArray<Byte>);
    class procedure FromPSQLLSeg(Value: TPSQLLSeg; Buffer: TArray<Byte>);
  end;
{$ENDIF DELPHI_17}


{$IFDEF DELPHI_12}
const
  OriginPoint: TPSQLPoint = (X: 0.0; Y: 0.0);
  OriginCircle: TPSQLCircle = (R: 0.0; X: 0.0; Y: 0.0);
  OriginBox: TPSQLBox = (Right: 0.0; Top: 0.0; Left: 0.0; Bottom: 0.0);
  OriginLSeg: TPSQLLSeg = (X1: 0.0; Y1: 0.0; X2: 0.0; Y2: 0.0);
{$ENDIF DELPHI_12}  

procedure Register;

function BadGUIDToGUID(const AStr: DACAString): DACAString;
{$IFDEF DELPHI_5}
function StringToGUID(const AStr: AnsiString): TGUID;
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
{$ENDIF}

implementation

uses SysUtils, Math, PSQLDBTables;

procedure Register;
begin
  RegisterClasses([TPSQLGuidField
        {$IFDEF DELPHI_12}
        ,TPSQLPointField, TPSQLCircleField, TPSQLBoxField, TPSQLLSegField, TPSQLRangeField
        {$ENDIF DELPHI_12}]);
end;

{ TPSQLGuidField }
type
  TFastGUID = packed record
    F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, FA, FB, FC, FD, FE, FF: Byte;
  end;

{$IFDEF DELPHI_17}
class function TPSQLBitConverter.ToPSQLPoint(Value: TArray<Byte>): TPSQLPoint;
begin
  Move(Value[0], Result, SizeOf(TPSQLPoint));
end;

class function TPSQLBitConverter.ToPSQLCircle(Value: TArray<Byte>): TPSQLCircle;
begin
  Move(Value[0], Result, SizeOf(TPSQLCircle));
end;

class function TPSQLBitConverter.ToPSQLBox(Value: TArray<Byte>): TPSQLBox;
begin
  Move(Value[0], Result, SizeOf(TPSQLBox));
end;

class function TPSQLBitConverter.ToPSQLLSeg(Value: TArray<Byte>): TPSQLLSeg;
begin
  Move(Value[0], Result, SizeOf(TPSQLLSeg));
end;

class function TPSQLBitConverter.ToPSQLRange(Value: TArray<Byte>): TPSQLRange;
begin
  Move(Value[0], Result, SizeOf(TPSQLRange));
end;

class procedure TPSQLBitConverter.FromPSQLPoint(Value: TPSQLPoint; Buffer: TArray<Byte>);
begin
  Move(Value, Buffer[0], SizeOf(TPSQLPoint));
end;

class procedure TPSQLBitConverter.FromPSQLCircle(Value: TPSQLCircle; Buffer: TArray<Byte>);
begin
  Move(Value, Buffer[0], SizeOf(TPSQLCircle));
end;

class procedure TPSQLBitConverter.FromPSQLBox(Value: TPSQLBox; Buffer: TArray<Byte>);
begin
  Move(Value, Buffer[0], SizeOf(TPSQLBox));
end;

class procedure TPSQLBitConverter.FromPSQLLSeg(Value: TPSQLLSeg; Buffer: TArray<Byte>);
begin
  Move(Value, Buffer[0], SizeOf(TPSQLLSeg));
end;
{$ENDIF}

{$IFDEF DELPHI_5}
  PWord         = ^Word;
  PSmallInt     = ^SmallInt;
  PByte         = ^Byte;
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
{$ENDIF}

const
  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

  Hex2IntHash: Array [Byte] of Byte = (0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,0,0,0,0,0,0,0,10,
    11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  Int2HexHash : Array [0..255] of Word = (
    $3030, $3130, $3230, $3330, $3430, $3530, $3630, $3730, $3830, $3930,
    $4130, $4230, $4330, $4430, $4530, $4630, $3031, $3131, $3231, $3331,
    $3431, $3531, $3631, $3731, $3831, $3931, $4131, $4231, $4331, $4431,
    $4531, $4631, $3032, $3132, $3232, $3332, $3432, $3532, $3632, $3732,
    $3832, $3932, $4132, $4232, $4332, $4432, $4532, $4632, $3033, $3133,
    $3233, $3333, $3433, $3533, $3633, $3733, $3833, $3933, $4133, $4233,
    $4333, $4433, $4533, $4633, $3034, $3134, $3234, $3334, $3434, $3534,
    $3634, $3734, $3834, $3934, $4134, $4234, $4334, $4434, $4534, $4634,
    $3035, $3135, $3235, $3335, $3435, $3535, $3635, $3735, $3835, $3935,
    $4135, $4235, $4335, $4435, $4535, $4635, $3036, $3136, $3236, $3336,
    $3436, $3536, $3636, $3736, $3836, $3936, $4136, $4236, $4336, $4436,
    $4536, $4636, $3037, $3137, $3237, $3337, $3437, $3537, $3637, $3737,
    $3837, $3937, $4137, $4237, $4337, $4437, $4537, $4637, $3038, $3138,
    $3238, $3338, $3438, $3538, $3638, $3738, $3838, $3938, $4138, $4238,
    $4338, $4438, $4538, $4638, $3039, $3139, $3239, $3339, $3439, $3539,
    $3639, $3739, $3839, $3939, $4139, $4239, $4339, $4439, $4539, $4639,
    $3041, $3141, $3241, $3341, $3441, $3541, $3641, $3741, $3841, $3941,
    $4141, $4241, $4341, $4441, $4541, $4641, $3042, $3142, $3242, $3342,
    $3442, $3542, $3642, $3742, $3842, $3942, $4142, $4242, $4342, $4442,
    $4542, $4642, $3043, $3143, $3243, $3343, $3443, $3543, $3643, $3743,
    $3843, $3943, $4143, $4243, $4343, $4443, $4543, $4643, $3044, $3144,
    $3244, $3344, $3444, $3544, $3644, $3744, $3844, $3944, $4144, $4244,
    $4344, $4444, $4544, $4644, $3045, $3145, $3245, $3345, $3445, $3545,
    $3645, $3745, $3845, $3945, $4145, $4245, $4345, $4445, $4545, $4645,
    $3046, $3146, $3246, $3346, $3446, $3546, $3646, $3746, $3846, $3946,
    $4146, $4246, $4346, $4446, $4546, $4646);

var
  HexUpperCase, HexLowerCase: Array[AnsiDACByteChar] of AnsiDACByteChar;

procedure InitHexUpperCase();
begin
{$IFNDEF MOBILE}
  HexUpperCase['a'] := 'A';
  HexUpperCase['b'] := 'B';
  HexUpperCase['c'] := 'C';
  HexUpperCase['d'] := 'D';
  HexUpperCase['e'] := 'E';
  HexUpperCase['f'] := 'F';

  HexUpperCase['A'] := 'A';
  HexUpperCase['B'] := 'B';
  HexUpperCase['C'] := 'C';
  HexUpperCase['D'] := 'D';
  HexUpperCase['E'] := 'E';
  HexUpperCase['F'] := 'F';

  HexUpperCase['0'] := '0';
  HexUpperCase['1'] := '1';
  HexUpperCase['2'] := '2';
  HexUpperCase['3'] := '3';
  HexUpperCase['4'] := '4';
  HexUpperCase['5'] := '5';
  HexUpperCase['6'] := '6';
  HexUpperCase['7'] := '7';
  HexUpperCase['8'] := '8';
  HexUpperCase['9'] := '9';
{$ELSE}
  HexUpperCase[Ord('a')] := Ord('A');
  HexUpperCase[Ord('b')] := Ord('B');
  HexUpperCase[Ord('c')] := Ord('C');
  HexUpperCase[Ord('d')] := Ord('D');
  HexUpperCase[Ord('e')] := Ord('E');
  HexUpperCase[Ord('f')] := Ord('F');

  HexUpperCase[Ord('A')] := Ord('A');
  HexUpperCase[Ord('B')] := Ord('B');
  HexUpperCase[Ord('C')] := Ord('C');
  HexUpperCase[Ord('D')] := Ord('D');
  HexUpperCase[Ord('E')] := Ord('E');
  HexUpperCase[Ord('F')] := Ord('F');

  HexUpperCase[Ord('0')] := Ord('0');
  HexUpperCase[Ord('1')] := Ord('1');
  HexUpperCase[Ord('2')] := Ord('2');
  HexUpperCase[Ord('3')] := Ord('3');
  HexUpperCase[Ord('4')] := Ord('4');
  HexUpperCase[Ord('5')] := Ord('5');
  HexUpperCase[Ord('6')] := Ord('6');
  HexUpperCase[Ord('7')] := Ord('7');
  HexUpperCase[Ord('8')] := Ord('8');
  HexUpperCase[Ord('9')] := Ord('9');
{$ENDIF}
end;

procedure InitHexLowerCase();
begin
{$IFNDEF MOBILE}
  HexLowerCase['a'] := 'a';
  HexLowerCase['b'] := 'b';
  HexLowerCase['c'] := 'c';
  HexLowerCase['d'] := 'd';
  HexLowerCase['e'] := 'e';
  HexLowerCase['f'] := 'f';

  HexLowerCase['A'] := 'a';
  HexLowerCase['B'] := 'b';
  HexLowerCase['C'] := 'c';
  HexLowerCase['D'] := 'd';
  HexLowerCase['E'] := 'e';
  HexLowerCase['F'] := 'f';

  HexLowerCase['0'] := '0';
  HexLowerCase['1'] := '1';
  HexLowerCase['2'] := '2';
  HexLowerCase['3'] := '3';
  HexLowerCase['4'] := '4';
  HexLowerCase['5'] := '5';
  HexLowerCase['6'] := '6';
  HexLowerCase['7'] := '7';
  HexLowerCase['8'] := '8';
  HexLowerCase['9'] := '9';
{$ELSE}
  HexLowerCase[Ord('a')] := Ord('a');
  HexLowerCase[Ord('b')] := Ord('b');
  HexLowerCase[Ord('c')] := Ord('c');
  HexLowerCase[Ord('d')] := Ord('d');
  HexLowerCase[Ord('e')] := Ord('e');
  HexLowerCase[Ord('f')] := Ord('f');

  HexLowerCase[Ord('A')] := Ord('a');
  HexLowerCase[Ord('B')] := Ord('b');
  HexLowerCase[Ord('C')] := Ord('c');
  HexLowerCase[Ord('D')] := Ord('d');
  HexLowerCase[Ord('E')] := Ord('e');
  HexLowerCase[Ord('F')] := Ord('f');

  HexLowerCase[Ord('0')] := Ord('0');
  HexLowerCase[Ord('1')] := Ord('1');
  HexLowerCase[Ord('2')] := Ord('2');
  HexLowerCase[Ord('3')] := Ord('3');
  HexLowerCase[Ord('4')] := Ord('4');
  HexLowerCase[Ord('5')] := Ord('5');
  HexLowerCase[Ord('6')] := Ord('6');
  HexLowerCase[Ord('7')] := Ord('7');
  HexLowerCase[Ord('8')] := Ord('8');
  HexLowerCase[Ord('9')] := Ord('9');
{$ENDIF}

end;

function BadGUIDToGUID(const AStr: DACAString): DACAString;
var
  C: PAnsiDACBytesChar;
  {$IFDEF MOBILE}
  M: TMarshaller;
  {$ENDIF}
begin
  Result := '{00000000-0000-0000-0000-000000000000}';
  if AStr = '' then Exit;
  {$IFDEF MOBILE}
  C := M.AsAnsi(AStr).ToPointer;
  {$ELSE}
  C := PAnsiDACChar(AStr);
  {$ENDIF}

  {$IFNDEF MOBILE}
  Result[2] := HexUpperCase[C^]; Inc(C);
  Result[3] := HexUpperCase[C^]; Inc(C);
  Result[4] := HexUpperCase[C^]; Inc(C);
  Result[5] := HexUpperCase[C^]; Inc(C);
  Result[6] := HexUpperCase[C^]; Inc(C);
  Result[7] := HexUpperCase[C^]; Inc(C);
  Result[8] := HexUpperCase[C^]; Inc(C);
  Result[9] := HexUpperCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[11] := HexUpperCase[C^]; Inc(C);
  Result[12] := HexUpperCase[C^]; Inc(C);
  Result[13] := HexUpperCase[C^]; Inc(C);
  Result[14] := HexUpperCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[16] := HexUpperCase[C^]; Inc(C);
  Result[17] := HexUpperCase[C^]; Inc(C);
  Result[18] := HexUpperCase[C^]; Inc(C);
  Result[19] := HexUpperCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[21] := HexUpperCase[C^]; Inc(C);
  Result[22] := HexUpperCase[C^]; Inc(C);
  Result[23] := HexUpperCase[C^]; Inc(C);
  Result[24] := HexUpperCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[26] := HexUpperCase[C^]; Inc(C);
  Result[27] := HexUpperCase[C^]; Inc(C);
  Result[28] := HexUpperCase[C^]; Inc(C);
  Result[29] := HexUpperCase[C^]; Inc(C);
  Result[30] := HexUpperCase[C^]; Inc(C);
  Result[31] := HexUpperCase[C^]; Inc(C);
  Result[32] := HexUpperCase[C^]; Inc(C);
  Result[33] := HexUpperCase[C^]; Inc(C);
  Result[34] := HexUpperCase[C^]; Inc(C);
  Result[35] := HexUpperCase[C^]; Inc(C);
  Result[36] := HexUpperCase[C^]; Inc(C);
  Result[37] := HexUpperCase[C^];
  {$ELSE}
  Result[2] := Chr(HexUpperCase[C^]); Inc(C);
  Result[3] := Chr(HexUpperCase[C^]); Inc(C);
  Result[4] := Chr(HexUpperCase[C^]); Inc(C);
  Result[5] := Chr(HexUpperCase[C^]); Inc(C);
  Result[6] := Chr(HexUpperCase[C^]); Inc(C);
  Result[7] := Chr(HexUpperCase[C^]); Inc(C);
  Result[8] := Chr(HexUpperCase[C^]); Inc(C);
  Result[9] := Chr(HexUpperCase[C^]); Inc(C);
  Inc(C); // skip -
  Result[11] := Chr(HexUpperCase[C^]); Inc(C);
  Result[12] := Chr(HexUpperCase[C^]); Inc(C);
  Result[13] := Chr(HexUpperCase[C^]); Inc(C);
  Result[14] := Chr(HexUpperCase[C^]); Inc(C);
  Inc(C); // skip -
  Result[16] := Chr(HexUpperCase[C^]); Inc(C);
  Result[17] := Chr(HexUpperCase[C^]); Inc(C);
  Result[18] := Chr(HexUpperCase[C^]); Inc(C);
  Result[19] := Chr(HexUpperCase[C^]); Inc(C);
  Inc(C); // skip -
  Result[21] := Chr(HexUpperCase[C^]); Inc(C);
  Result[22] := Chr(HexUpperCase[C^]); Inc(C);
  Result[23] := Chr(HexUpperCase[C^]); Inc(C);
  Result[24] := Chr(HexUpperCase[C^]); Inc(C);
  Inc(C); // skip -
  Result[26] := Chr(HexUpperCase[C^]); Inc(C);
  Result[27] := Chr(HexUpperCase[C^]); Inc(C);
  Result[28] := Chr(HexUpperCase[C^]); Inc(C);
  Result[29] := Chr(HexUpperCase[C^]); Inc(C);
  Result[30] := Chr(HexUpperCase[C^]); Inc(C);
  Result[31] := Chr(HexUpperCase[C^]); Inc(C);
  Result[32] := Chr(HexUpperCase[C^]); Inc(C);
  Result[33] := Chr(HexUpperCase[C^]); Inc(C);
  Result[34] := Chr(HexUpperCase[C^]); Inc(C);
  Result[35] := Chr(HexUpperCase[C^]); Inc(C);
  Result[36] := Chr(HexUpperCase[C^]); Inc(C);
  Result[37] := Chr(HexUpperCase[C^]);

  {$ENDIF}
end;

{$IFNDEF FPC}
function GUIDToString(const AGUID: TGUID): DACAString;
type
  TPointerInt = {$IFDEF DELPHI_16}NativeInt{$ELSE}Cardinal{$ENDIF};
var
  P: TPointerInt;
begin
  SetLength(Result, 38);
  P := TPointerInt(Result);
  PByte(P)^ := Ord('{'); Inc(P);
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F3]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F2]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F1]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F0]; Inc(P, SizeOf(Word));
  PByte(P)^ := Ord('-'); Inc(P);
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F5]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F4]; Inc(P, SizeOf(Word));
  PByte(P)^ := Ord('-'); Inc(P);
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F7]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F6]; Inc(P, SizeOf(Word));
  PByte(P)^ := Ord('-'); Inc(P);
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F8]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).F9]; Inc(P, SizeOf(Word));
  PByte(P)^ := Ord('-'); Inc(P);
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FA]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FB]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FC]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FD]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FE]; Inc(P, SizeOf(Word));
  PWord(P)^ := Int2HexHash[TFastGUID(AGUID).FF]; Inc(P, SizeOf(Word));
  PByte(P)^ := Ord('}');
end;
{$ENDIF}

function StringToGUID(const AStr: DACAString): TGUID;
begin
  if (AStr = '') or (Length(AStr) <> 38) then begin
    Result := GUID_NULL;
    Exit;
  end;
  TFastGUID(Result).F3 := Hex2IntHash[Byte(AStr[2])] shl 4 or Hex2IntHash[Byte(AStr[3])];
  TFastGUID(Result).F2 := Hex2IntHash[Byte(AStr[4])] shl 4 or Hex2IntHash[Byte(AStr[5])];
  TFastGUID(Result).F1 := Hex2IntHash[Byte(AStr[6])] shl 4 or Hex2IntHash[Byte(AStr[7])];
  TFastGUID(Result).F0 := Hex2IntHash[Byte(AStr[8])] shl 4 or Hex2IntHash[Byte(AStr[9])];
  //-
  TFastGUID(Result).F5 := Hex2IntHash[Byte(AStr[11])] shl 4 or Hex2IntHash[Byte(AStr[12])];
  TFastGUID(Result).F4 := Hex2IntHash[Byte(AStr[13])] shl 4 or Hex2IntHash[Byte(AStr[14])];
  //-
  TFastGUID(Result).F7 := Hex2IntHash[Byte(AStr[16])] shl 4 or Hex2IntHash[Byte(AStr[17])];
  TFastGUID(Result).F6 := Hex2IntHash[Byte(AStr[18])] shl 4 or Hex2IntHash[Byte(AStr[19])];
  //-
  TFastGUID(Result).F8 := Hex2IntHash[Byte(AStr[21])] shl 4 or Hex2IntHash[Byte(AStr[22])];
  TFastGUID(Result).F9 := Hex2IntHash[Byte(AStr[23])] shl 4 or Hex2IntHash[Byte(AStr[24])];
  //-
  TFastGUID(Result).FA := Hex2IntHash[Byte(AStr[26])] shl 4 or Hex2IntHash[Byte(AStr[27])];
  TFastGUID(Result).FB := Hex2IntHash[Byte(AStr[28])] shl 4 or Hex2IntHash[Byte(AStr[29])];
  TFastGUID(Result).FC := Hex2IntHash[Byte(AStr[30])] shl 4 or Hex2IntHash[Byte(AStr[31])];
  TFastGUID(Result).FD := Hex2IntHash[Byte(AStr[32])] shl 4 or Hex2IntHash[Byte(AStr[33])];
  TFastGUID(Result).FE := Hex2IntHash[Byte(AStr[34])] shl 4 or Hex2IntHash[Byte(AStr[35])];
  TFastGUID(Result).FF := Hex2IntHash[Byte(AStr[36])] shl 4 or Hex2IntHash[Byte(AStr[37])];
end;

function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@guid1);
  b := PIntegerArray(@guid2);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;

function TPSQLGuidField.GetAsGuid(): TGUID;
var
  S: String;
begin
  S := GetAsString();
  Result := StringToGUID(DACAString(S));
end;

procedure TPSQLGuidField.SetAsGuid(const Value: TGUID);
begin
  if IsEqualGUID(Value, GUID_NULL)
    then Clear()
    else SetAsString(string(GUIDToString(Value)))
end;

class procedure TPSQLGuidField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then
    inherited;
end;

{$IFDEF DELPHI_12}

{ TPSQLPointField }

function TPSQLPointField.GetAsPoint: TPSQLPoint;
begin
  if not GetValue(Result) then
    Result := OriginPoint;
end;

function TPSQLPointField.GetAsString: string;
var
  P: TPSQLPoint;
begin
  if GetValue(P) then
    Result := PointToSQLPoint(P, ';', True)
  else
    Result := '';
end;


function TPSQLPointField.GetAsTPoint: TPoint;
begin
  with GetAsPoint do
   begin
    Result.X := Longint(Round(X));
    Result.Y := Longint(Round(Y));
   end;
end;

function TPSQLPointField.GetDataSize: Integer;
begin
  Result := SizeOf(TPSQLPoint);
end;

function TPSQLPointField.GetDefaultWidth: Integer;
begin
  Result := 15;
end;

procedure TPSQLPointField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  P: TPSQLPoint;
begin
  if GetValue(P) then
  begin
    if DisplayText or (EditFormat = '') then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    if FmtStr = '' then
      Text := GetAsString()
    else
      Text := Format(FmtStr, [P.X, P.Y]);
  end else
    Text := '';
end;

{$IFDEF DELPHI_17}
function TPSQLPointField.GetValue(var Value: TPSQLPoint): Boolean;
var
  Data: TValueBuffer;
begin
  SetLength(Data, SizeOf(TPSQLPoint));
  Result := GetData(Data);
  Value := TPSQLBitConverter.ToPSQLPoint(Data);
end;
{$ELSE}
function TPSQLPointField.GetValue(var Value: TPSQLPoint): Boolean;
begin
  Result := GetData(@Value);
end;
{$ENDIF}

{$IFDEF DELPHI_17}
procedure TPSQLPointField.SetAsPoint(const Value: TPSQLPoint);
begin
  SetData(BytesOf(@Value, SizeOf(TPSQLPoint)));
end;
{$ELSE}
procedure TPSQLPointField.SetAsPoint(const Value: TPSQLPoint);
begin
  SetData(@Value);
end;
{$ENDIF}

procedure TPSQLPointField.SetAsString(const Value: string);
var P: TPSQLPoint;
begin
  if Value = '' then Clear else
  begin
    P := SQLPointToPoint(Value, ';', True);
    SetAsPoint(P);
  end;
end;

procedure TPSQLPointField.SetAsTPoint(const Value: TPoint);
var P: TPSQLPoint;
begin
  P.X := Value.X;
  P.Y := Value.Y;
  SetAsPoint(P);
end;

{ TPSQLCircleField }

function TPSQLCircleField.GetAsCircle: TPSQLCircle;
begin
  if not GetValue(Result) then
    Result := OriginCircle;
end;

function TPSQLCircleField.GetAsString: string;
var
  P: TPSQLCircle;
begin
  if GetValue(P) then
    Result := CircleToSQLCircle(P, ';', True)
  else
    Result := '';
end;

function TPSQLCircleField.GetDataSize: Integer;
begin
  Result := SizeOf(TPSQLCircle);
end;

function TPSQLCircleField.GetDefaultWidth: Integer;
begin
  Result := 22;
end;

procedure TPSQLCircleField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  P: TPSQLCircle;
begin
  if GetValue(P) then
  begin
    if DisplayText or (EditFormat = '') then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    if FmtStr = '' then
      Text := GetAsString()
    else
      Text := Format(FmtStr, [P.X, P.Y, P.R]);
  end else
    Text := '';
end;

{$IFDEF DELPHI_17}
function TPSQLCircleField.GetValue(var Value: TPSQLCircle): Boolean;
var
  Data: TValueBuffer;
begin
  SetLength(Data, SizeOf(TPSQLCircle));
  Result := GetData(Data);
  Value := TPSQLBitConverter.ToPSQLCircle(Data);
end;
{$ELSE}
function TPSQLCircleField.GetValue(var Value: TPSQLCircle): Boolean;
begin
  Result := GetData(@Value);
end;
{$ENDIF}

{$IFDEF DELPHI_17}
procedure TPSQLCircleField.SetAsCircle(const Value: TPSQLCircle);
begin
  SetData(BytesOf(@Value, SizeOf(TPSQLCircle)));
end;
{$ELSE}
procedure TPSQLCircleField.SetAsCircle(const Value: TPSQLCircle);
begin
  SetData(@Value);
end;
{$ENDIF}

procedure TPSQLCircleField.SetAsString(const Value: string);
var C: TPSQLCircle;
begin
  if Value = '' then Clear else
  begin
    C := SQLCircleToCircle(Value, ';', True);
    SetAsCircle(C);
  end;
end;

{ TPSQLBoxField }

function TPSQLBoxField.GetAsBox: TPSQLBox;
begin
  if not GetValue(Result) then
    Result := OriginBox;
end;

function TPSQLBoxField.GetAsString: string;
var
  B: TPSQLBox;
begin
  if GetValue(B) then
    Result := BoxToSQLBox(B, ';', True)
  else
    Result := '';
end;

function TPSQLBoxField.GetAsTRect: TRect;
begin
  with GetAsBox do
   begin
    Result.Left := Longint(Round(Left));
    Result.Top := Longint(Round(Top));
    Result.Right := Longint(Round(Right));
    Result.Bottom := Longint(Round(Bottom));
   end;
end;

function TPSQLBoxField.GetDataSize: Integer;
begin
  Result := SizeOf(TPSQLBox);
end;

function TPSQLBoxField.GetDefaultWidth: Integer;
begin
  Result := 32;
end;

procedure TPSQLBoxField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  B: TPSQLBox;
begin
  if GetValue(B) then
  begin
    if DisplayText or (EditFormat = '') then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    if FmtStr = '' then
      Text := GetAsString()
    else
      Text := Format(FmtStr, [B.Left, B.Top, B.Right, B.Bottom]);
  end else
    Text := '';
end;

{$IFDEF DELPHI_17}
function TPSQLBoxField.GetValue(var Value: TPSQLBox): Boolean;
var
  Data: TValueBuffer;
begin
  SetLength(Data, SizeOf(TPSQLBox));
  Result := GetData(Data);
  Value := TPSQLBitConverter.ToPSQLBox(Data);
end;
{$ELSE}
function TPSQLBoxField.GetValue(var Value: TPSQLBox): Boolean;
begin
  Result := GetData(@Value);
end;
{$ENDIF}

{$IFDEF DELPHI_17}
procedure TPSQLBoxField.SetAsBox(const Value: TPSQLBox);
begin
  SetData(BytesOf(@Value, SizeOf(TPSQLBox)));
end;
{$ELSE}
procedure TPSQLBoxField.SetAsBox(const Value: TPSQLBox);
begin
  SetData(@Value);
end;
{$ENDIF}

procedure TPSQLBoxField.SetAsString(const Value: string);
var B: TPSQLBox;
begin
  if Value = '' then Clear else
  begin
    B := SQLBoxToBox(Value, ';', True);
    SetAsBox(B);
  end;
end;

procedure TPSQLBoxField.SetAsTRect(const Value: TRect);
var B: TPSQLBox;
begin
  B.Right := Value.Right;
  B.Top := Value.Top;
  B.Left := Value.Left;
  B.Bottom := Value.Bottom;
  SetAsBox(B);
end;

{ TPSQLLSegField }

function TPSQLLSegField.GetAsLSeg: TPSQLLSeg;
begin
  if not GetValue(Result) then
    Result := OriginLSeg;
end;

function TPSQLLSegField.GetAsString: string;
var
  LS: TPSQLLSeg;
begin
  if GetValue(LS) then
    Result := LSegToSQLLSeg(LS, ';', True)
  else
    Result := '';
end;

function TPSQLLSegField.GetDataSize: Integer;
begin
  Result := SizeOf(TPSQLLSeg);
end;

function TPSQLLSegField.GetDefaultWidth: Integer;
begin
  Result := 32;
end;

procedure TPSQLLSegField.GetText(var Text: string; DisplayText: Boolean);
var
  FmtStr: string;
  LS: TPSQLLSeg;
begin
  if GetValue(LS) then
  begin
    if DisplayText or (EditFormat = '') then
      FmtStr := DisplayFormat
    else
      FmtStr := EditFormat;
    if FmtStr = '' then
      Text := GetAsString()
    else
      Text := Format(FmtStr, [LS.X1, LS.Y1, LS.X2, LS.Y2]);
  end else
    Text := '';
end;

{$IFDEF DELPHI_17}
function TPSQLLSegField.GetValue(var Value: TPSQLLSeg): Boolean;
var
  Data: TValueBuffer;
begin
  SetLength(Data, SizeOf(TPSQLLSeg));
  Result := GetData(Data);
  Value := TPSQLBitConverter.ToPSQLLSeg(Data);
end;
{$ELSE}
function TPSQLLSegField.GetValue(var Value: TPSQLLSeg): Boolean;
begin
  Result := GetData(@Value);
end;
{$ENDIF}

{$IFDEF DELPHI_17}
procedure TPSQLLSegField.SetAsLSeg(const Value: TPSQLLSeg);
begin
  SetData(BytesOf(@Value, SizeOf(TPSQLLSeg)));
end;
{$ELSE}
procedure TPSQLLSegField.SetAsLSeg(const Value: TPSQLLSeg);
begin
  SetData(@Value);
end;
{$ENDIF}

procedure TPSQLLSegField.SetAsString(const Value: string);
var LS: TPSQLLSeg;
begin
  if Value = '' then Clear else
  begin
    LS := SQLLSegToLSeg(Value, ';', True);
    SetAsLSeg(LS);
  end;
end;

{ TPSQLRangeField }

function TPSQLRangeField.GetValue(var Value: TPSQLRange): Boolean;
{$IFDEF DELPHI_17}
var
  Data: TValueBuffer;
{$ENDIF}
begin
{$IFDEF DELPHI_17}
  SetLength(Data, SizeOf(TPSQLRange));
  Result := GetData(Data);
  Value := TPSQLBitConverter.ToPSQLRange(Data);
{$ELSE}
  Result := GetData(@Value);
{$ENDIF}
end;

function TPSQLRangeField.GetNativeRangeType: cardinal;
begin
  Result := (DataSet.FieldDefs[FieldNo-1] as TPSQLFieldDef).NativeDataType;
end;

function TPSQLRangeField.GetAsRange: TPSQLRange;
begin
  if not GetValue(Result) then
    Result := Default(TPSQLRange);
end;

function TPSQLRangeField.GetAsString: string;
var R: TPSQLRange;
begin
  if GetValue(R) then
    Result := RangeToSQLRange(R, GetNativeRangeType(), ';', True)
  else
    Result := '';
end;

procedure TPSQLRangeField.SetAsString(const Value: string);
var R: TPSQLRange;
begin
  if Value = '' then Clear else
  begin
    R := SQLRangeToRange(Value, GetNativeRangeType(), ';', True);
    SetAsRange(R);
  end;
end;

function TPSQLRangeField.GetDefaultWidth: Integer;
begin
  case GetNativeRangeType() of
    FIELD_TYPE_INT4RANGE,
    FIELD_TYPE_INT8RANGE,
    FIELD_TYPE_NUMRANGE : Result := (inherited GetDefaultWidth) * 2 + 3;
    FIELD_TYPE_DATERANGE: Result := DATELEN * 2 + 3;
    FIELD_TYPE_TSRANGE,
    FIELD_TYPE_TSTZRANGE: Result := TIMESTAMPTZLEN * 2 + 3;
  else
    Result := 15;
  end;
end;

function TPSQLRangeField.IsDiscrete: boolean;
begin
  Result := False;
  case GetNativeRangeType() of
    FIELD_TYPE_INT4RANGE,
    FIELD_TYPE_INT8RANGE,
    FIELD_TYPE_DATERANGE: Result := True;
    FIELD_TYPE_NUMRANGE,
    FIELD_TYPE_TSRANGE,
    FIELD_TYPE_TSTZRANGE: Result := False;
  else
    DatabaseErrorFmt(SFieldNotRangeType, [DisplayName]);
  end;
end;

function TPSQLRangeField.IsEmpty: boolean;
var
  R: TPSQLRange;
begin
  if not GetValue(R) then
    Result := True
  else
    Result := R.Empty;
end;

procedure TPSQLRangeField.SetAsRange(const Value: TPSQLRange);
begin
{$IFDEF DELPHI_17}
  SetData(BytesOf(@Value, SizeOf(TPSQLRange)), True);
{$ELSE}
  SetData(@Value);
{$ENDIF}
end;

{$ENDIF DELPHI_12}

initialization

InitHexUpperCase();
InitHexLowerCase();

finalization

end.
