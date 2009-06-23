{$I psqldac.inc}
unit PSQLFields;

interface

uses DB;

type

  TPSQLGuidField = class(TGuidField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsGuid(): TGUID;
    procedure SetAsGuid(const Value: TGUID);
  public
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
  end;

procedure Register;

function BadGUIDToGUID(const AStr: AnsiString): AnsiString;

implementation

uses Classes;

procedure Register;
begin
  RegisterClasses([TPSQLGuidField]);
end;

//TPSQLGuidField
type
  TFastGUID = record
    F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, FA, FB, FC, FD, FE, FF: Byte;
  end;

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
  HexUpperCase, HexLowerCase: Array[AnsiChar] of AnsiChar;

procedure InitHexUpperCase();
begin
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
end;

procedure InitHexLowerCase();
begin
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
end;

function BadGUIDToGUID(const AStr: AnsiString): AnsiString;
var
  C: PAnsiChar;
begin
  Result := '{00000000-0000-0000-0000-000000000000}';
  if AStr = '' then Exit;
  C := PAnsiChar(AStr);
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
end;

function GUIDToBadGUID(const AStr: AnsiString): String;
var
  C: PAnsiChar;
begin
  Result := '00000000-0000-0000-0000-000000000000';
  if AStr = '' then Exit;

  C := PAnsiChar(AStr);
  Inc(C); // skip {

  Result[1] := HexLowerCase[C^]; Inc(C);
  Result[2] := HexLowerCase[C^]; Inc(C);
  Result[3] := HexLowerCase[C^]; Inc(C);
  Result[4] := HexLowerCase[C^]; Inc(C);
  Result[5] := HexLowerCase[C^]; Inc(C);
  Result[6] := HexLowerCase[C^]; Inc(C);
  Result[7] := HexLowerCase[C^]; Inc(C);
  Result[8] := HexLowerCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[10] := HexLowerCase[C^]; Inc(C);
  Result[11] := HexLowerCase[C^]; Inc(C);
  Result[12] := HexLowerCase[C^]; Inc(C);
  Result[13] := HexLowerCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[15] := HexLowerCase[C^]; Inc(C);
  Result[16] := HexLowerCase[C^]; Inc(C);
  Result[17] := HexLowerCase[C^]; Inc(C);
  Result[18] := HexLowerCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[20] := HexLowerCase[C^]; Inc(C);
  Result[21] := HexLowerCase[C^]; Inc(C);
  Result[22] := HexLowerCase[C^]; Inc(C);
  Result[23] := HexLowerCase[C^]; Inc(C);
  Inc(C); // skip -
  Result[25] := HexLowerCase[C^]; Inc(C);
  Result[26] := HexLowerCase[C^]; Inc(C);
  Result[27] := HexLowerCase[C^]; Inc(C);
  Result[28] := HexLowerCase[C^]; Inc(C);
  Result[29] := HexLowerCase[C^]; Inc(C);
  Result[30] := HexLowerCase[C^]; Inc(C);
  Result[31] := HexLowerCase[C^]; Inc(C);
  Result[32] := HexLowerCase[C^]; Inc(C);
  Result[33] := HexLowerCase[C^]; Inc(C);
  Result[34] := HexLowerCase[C^]; Inc(C);
  Result[35] := HexLowerCase[C^]; Inc(C);
  Result[36] := HexLowerCase[C^];
end;

function GUIDToString(const AGUID: TGUID): AnsiString;
var
  P: Cardinal;
begin
  SetLength(Result, 38);
  P := Cardinal(Result);
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

function StringToGUID(const AStr: AnsiString): TGUID;
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
  Result := StringToGUID(S);
end;

procedure TPSQLGuidField.SetAsGuid(const Value: TGUID);
begin
  if IsEqualGUID(Value, GUID_NULL)
    then Clear()
    else SetAsString(GUIDToString(Value))
end;

class procedure TPSQLGuidField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then
    inherited;
end;

initialization

InitHexUpperCase();
InitHexLowerCase();

finalization

end.
