unit PSQLExtMask;

{SVN revision: $Id$}

interface

uses SysUtils;

type
  EExtMaskException = class(Exception);

  TExtMask = class
  private
    FCaseSensitive: boolean;
    FMask: Pointer;
    FSize: Integer;
  public
    constructor Create(const MaskValue: string; const CaseSensitive: boolean = False;
                const MatchAnyChar: Ansichar = '%'; const MatchSingleChar: Ansichar = '?');
    destructor Destroy; override;
    function Matches(const AString: string): Boolean;
  end;


function MatchesMask(const AString, Mask: string; const CaseSensitive: boolean = False;
                     const MatchAnyChar: Ansichar = '%'; const MatchSingleChar: Ansichar = '?'): Boolean;

function EscapeMask(const AMask, EscapeChars: string): string;

implementation

const
  MaxCards = 30;

resourcestring
  SInvalidMask = '''%s'' is an invalid mask at (%d)';

type
  PMaskSet = ^TMaskSet;
  TMaskSet = set of AnsiChar;
  TMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);
  TMaskState = record
    SkipTo: Boolean;
    case State: TMaskStates of
      msLiteral: (Literal: AnsiChar);
      msAny: ();
      msSet: (
        Negate: Boolean;
        CharSet: PMaskSet);
      msMBCSLiteral: (LeadByte, TrailByte: AnsiChar);
  end;
  PMaskStateArray = ^TMaskStateArray;
  TMaskStateArray = array[0..128] of TMaskState;

function InitMaskStates(const Mask: string;
  var MaskStates: array of TMaskState;
  const MatchAnyChar: Ansichar = '*';
  const MatchSingleChar: Ansichar = '?'): Integer;
var
  I: Integer;
  SkipTo: Boolean;
  Literal: AnsiChar;
  LeadByte, TrailByte: AnsiChar;
  P: PAnsiChar;
  Negate: Boolean;
  CharSet: TMaskSet;
  Cards: Integer;

  procedure InvalidMask;
  begin
    raise EExtMaskException.CreateResFmt(@SInvalidMask, [Mask,
      P - PChar(Mask) + 1]);
  end;

  procedure Reset;
  begin
    SkipTo := False;
    Negate := False;
    CharSet := [];
  end;

  procedure WriteScan(MaskState: TMaskStates);
  begin
    if I <= High(MaskStates) then
    begin
      if SkipTo then
      begin
        Inc(Cards);
        if Cards > MaxCards then InvalidMask;
      end;
      MaskStates[I].SkipTo := SkipTo;
      MaskStates[I].State := MaskState;
      case MaskState of
        msLiteral: MaskStates[I].Literal := Literal;
        msSet:
          begin
            MaskStates[I].Negate := Negate;
            New(MaskStates[I].CharSet);
            MaskStates[I].CharSet^ := CharSet;
          end;
        msMBCSLiteral:
          begin
            MaskStates[I].LeadByte := LeadByte;
            MaskStates[I].TrailByte := TrailByte;
          end;
      end;
    end;
    Inc(I);
    Reset;
  end;

  procedure ScanSet;
  var
    LastChar: AnsiChar;
    C: AnsiChar;
  begin
    Inc(P);
    if P^ = '!' then
    begin
      Negate := True;
      Inc(P);
    end;
    LastChar := #0;
    while not (P^ in [#0, ']']) do
    begin
      // MBCS characters not supported in msSet!
      if P^ in LeadBytes then
         Inc(P)
      else
      case P^ of
        '-':
          if LastChar = #0 then InvalidMask
          else
          begin
            Inc(P);
            for C := LastChar to (P^) do Include(CharSet, C);
          end;
      else
        LastChar := (P^);
        Include(CharSet, LastChar);
      end;
      Inc(P);
    end;
    if (P^ <> ']') or (CharSet = []) then InvalidMask;
    WriteScan(msSet);
  end;

begin
  P := PAnsiChar(AnsiString(Mask));
  I := 0;
  Cards := 0;
  Reset;
  while P^ <> #0 do
  begin
    if P^ = MatchAnyChar then
      SkipTo := True
     else
      if P^ = MatchSingleChar then
        begin
          if not SkipTo then WriteScan(msAny);
        end
      else
        case P^ of
          '[':  ScanSet;
          '\':  begin
                 Inc(P);
                 If P^ <> #0 then
                  begin
                   Literal := P^;
                   WriteScan(msLiteral);
                  end;
                end;
        else
          if P^ in LeadBytes then
          begin
            LeadByte := P^;
            Inc(P);
            TrailByte := P^;
            WriteScan(msMBCSLiteral);
          end
          else
          begin
            Literal := P^;
            WriteScan(msLiteral);
          end;
        end;
    If P^ <> #0 then Inc(P);
  end;
  Literal := #0;
  WriteScan(msLiteral);
  Result := I;
end;

function MatchesMaskStates(const Filename: ansistring;
  const MaskStates: array of TMaskState): Boolean;
type
  TStackRec = record
    sP: PAnsiChar;
    sI: Integer;
  end;
var
  T: Integer;
  S: array[0..MaxCards - 1] of TStackRec;
  I: Integer;
  P: PAnsiChar;

  procedure Push(P: PAnsiChar; I: Integer);
  begin
    with S[T] do
    begin
      sP := P;
      sI := I;
    end;
    Inc(T);
  end;

  function Pop(var P: PAnsiChar; var I: Integer): Boolean;
  begin
    if T = 0 then
      Result := False
    else
    begin
      Dec(T);
      with S[T] do
      begin
        P := sP;
        I := sI;
      end;
      Result := True;
    end;
  end;

  function Matches(P: PAnsiChar; Start: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Start to High(MaskStates) do
      with MaskStates[I] do
      begin
        if SkipTo then
        begin
          case State of
            msLiteral:
              while (P^ <> #0) and (P^ <> Literal) do Inc(P);
            msSet:
              while (P^ <> #0) and not (Negate xor (P^ in CharSet^)) do Inc(P);
            msMBCSLiteral:
              while (P^ <> #0) do
              begin
                if (P^ <> LeadByte) then Inc(P, 2)
                else
                begin
                  Inc(P);
                  if (P^ = TrailByte) then Break;
                  Inc(P);
                end;
              end;
          end;
          if P^ <> #0 then Push(@P[1], I);
        end;
        case State of
          msLiteral: if P^ <> Literal then Exit;
          msSet: if not (Negate xor (P^ in CharSet^)) then Exit;
          msMBCSLiteral:
            begin
              if P^ <> LeadByte then Exit;
              Inc(P);
              if P^ <> TrailByte then Exit;
            end;
        end;
        Inc(P);
      end;
    Result := True;
  end;

begin
  Result := True;
  T := 0;
  P := PAnsiChar(Filename);
  I := Low(MaskStates);
  repeat
    if Matches(P, I) then Exit;
  until not Pop(P, I);
  Result := False;
end;

procedure DoneMaskStates(var MaskStates: array of TMaskState);
var
  I: Integer;
begin
  for I := Low(MaskStates) to High(MaskStates) do
    if MaskStates[I].State = msSet then Dispose(MaskStates[I].CharSet);
end;

{ TExtMask }

constructor TExtMask.Create(const MaskValue: string; const CaseSensitive: boolean = False;
        const MatchAnyChar: Ansichar = '%'; const MatchSingleChar: Ansichar = '?');
var
  A: array[0..0] of TMaskState;
  S: string;
begin
  FCaseSensitive := CaseSensitive;
  If not FCaseSensitive then S := UpperCase(MaskValue) else S := MaskValue;
  FSize := InitMaskStates(S, A, MatchAnyChar, MatchSingleChar);
  FMask := AllocMem(FSize * SizeOf(TMaskState));
  InitMaskStates(S, Slice(PMaskStateArray(FMask)^, FSize), MatchAnyChar, MatchSingleChar);
end;

destructor TExtMask.Destroy;
begin
  if FMask <> nil then
  begin
    DoneMaskStates(Slice(PMaskStateArray(FMask)^, FSize));
    FreeMem(FMask, FSize * SizeOf(TMaskState));
  end;
end;

function TExtMask.Matches(const AString: string): Boolean;
var S: string;
begin
  if not FCaseSensitive then S := UpperCase(AString) else S := AString;
  Result := MatchesMaskStates(AnsiString(S), Slice(PMaskStateArray(FMask)^, FSize));
end;

function MatchesMask(const AString, Mask: string; const CaseSensitive: boolean = False;
                  const MatchAnyChar: Ansichar = '%'; const MatchSingleChar: Ansichar = '?'): Boolean;
var
  CMask: TExtMask;
begin
  CMask := TExtMask.Create(Mask,CaseSensitive, MatchAnyChar, MatchSingleChar);
  try
    Result := CMask.Matches(AString);
  finally
    CMask.Free;
  end;
end;

function EscapeMask(const AMask, EscapeChars: string): string;
var
  I, J: Integer;
begin
  Result := AMask;
  for I := Length(Result) downto 1 do
   for J := Length(EscapeChars) downto 1 do
    if Result[I] = EscapeChars[J] then
     begin
       Insert('\', Result, I);
       Break;
     end;
end;

end.
