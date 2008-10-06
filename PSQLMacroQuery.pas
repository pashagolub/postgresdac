{$I PSQLdac.inc}
unit PSQLMacroQuery;

{$P+,W-,R-}

interface

uses Windows, Classes, SysUtils, DB, PSQLDBTables, PSQLTypes, PSQLAccess;

const
  DefaultMacroChar = '%';
  TrueExpr = '0=0';


type
  TCharSet = TSysCharSet;

{ TPSQLMacroQuery }
  TPSQLMacroQuery = class(TPSQLQuery)
  private
    FDisconnectExpected: Boolean;
    FSaveQueryChanged: TNotifyEvent;
    FMacroChar: Char;
    FMacros: TParams;
    FSQLPattern: TStrings;
    FStreamPatternChanged: Boolean;
    FPatternChanged: Boolean;
    function GetMacros: TParams;
    procedure SetMacros(Value: TParams);
    procedure SetSQL(Value: TStrings);
    procedure PatternChanged(Sender: TObject);
    procedure QueryChanged(Sender: TObject);
    procedure RecreateMacros;
    procedure CreateMacros(List: TParams; const Value: PChar);
    procedure Expand(Query: TStrings);
    function GetMacroCount: Word;
    procedure SetMacroChar(Value: Char);
  protected
    procedure Loaded; override;
    function CreateHandle: HDBICur; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure Disconnect; override;
  protected
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetTableName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExpandMacros;
    procedure ExecSQL;
    procedure Prepare;
    procedure ExecDirect;
    function MacroByName(const Value: string): TParam;
    function IsEmpty: Boolean;
    procedure Reopen;
    property MacroCount: Word read GetMacroCount;
  published
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property SQL: TStrings read FSQLPattern write SetSQL;
    property Macros: TParams read GetMacros write SetMacros;
  end;

{ TMacroQueryThread }
  TRunQueryMode = (rqOpen, rqExecute, rqExecDirect);

  TPSQLMacroQueryThread = class(TThread)
  private
    FData: TPSQLDataSet;
    FMode: TRunQueryMode;
    FPrepare: Boolean;
    FException: TObject;
    procedure DoHandleException;
  protected
    procedure ModeError; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure HandleException; virtual;
  public
    constructor Create(Data: TPSQLDataSet; RunMode: TRunQueryMode;
      Prepare, CreateSuspended: Boolean);
  end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
function IsDataSetEmpty(DataSet: TDataSet): Boolean;


implementation

uses {$IFDEF DELPHI_6}RTLConsts, {$ENDIF} Consts, Forms, BDEConst;

{ Parse SQL utility routines }
function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := CharInSet(C, [' ', ',', ';', ')', #13, #10] + Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := CharInSet(C, ['''', '"']);
end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
          if Macro then
            List.CreateParam(ftString, Name, ptInput).AsString := TrueExpr
          else List.CreateParam(ftUnknown, Name, ptUnknown);
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

function IsDataSetEmpty(DataSet: TDataSet): Boolean;
begin
  with DataSet do Result := (not Active) or (Eof and Bof);
end;


{ TPSQLMacroQuery }
constructor TPSQLMacroQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSaveQueryChanged := TStringList(inherited SQL).OnChange;
  TStringList(inherited SQL).OnChange := QueryChanged;
  FMacroChar := DefaultMacroChar;
  FSQLPattern := TStringList.Create;
  TStringList(SQL).OnChange := PatternChanged;
  FMacros := TParams.Create(Self);
end;

destructor TPSQLMacroQuery.Destroy;
begin
  Destroying;
  Disconnect;
  FMacros.Free;
  FSQLPattern.Free;
  inherited Destroy;
end;

procedure TPSQLMacroQuery.Loaded;
begin
  inherited Loaded;
  GetMacros;
end;

function TPSQLMacroQuery.CreateHandle: HDBICur;
begin
  Result := inherited CreateHandle;
end;

procedure TPSQLMacroQuery.OpenCursor;
begin
  ExpandMacros;
  inherited OpenCursor(InfoQuery);
end;

procedure TPSQLMacroQuery.ExecSQL;
begin
  ExpandMacros;
  inherited ExecSQL;
end;

procedure TPSQLMacroQuery.Prepare;
begin
  ExpandMacros;
  inherited Prepare;
end;


procedure TPSQLMacroQuery.ExecDirect;
var
   AffectedRows : LongInt;
begin
  CheckInactive;
  SetDBFlag(dbfExecSQL, True);
  try
    if SQL.Count > 0 then
    begin
      PSQLDBTables.Check(Engine,Engine.QExecDirect(DBHandle, PChar(inherited SQL.Text), nil, AffectedRows));
    end
    else DatabaseError(SEmptySQLStatement);
  finally
    SetDBFlag(dbfExecSQL, False);
  end;
end;

procedure TPSQLMacroQuery.Disconnect;
var
  Strings: TStrings;
  Event1, Event2: TNotifyEvent;
begin
  inherited Disconnect;
  if (csDestroying in ComponentState) then Exit;
  Strings := inherited SQL;
  Event1 := TStringList(Strings).OnChange;
  Event2 := QueryChanged;
  if @Event1 <> @Event2 then begin
    if not FDisconnectExpected then SQL := inherited SQL;
    TStringList(inherited SQL).OnChange := QueryChanged;
  end;
end;

procedure TPSQLMacroQuery.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TPSQLMacroQuery.GetMacros: TParams;
begin
  if FStreamPatternChanged then
  begin
    FStreamPatternChanged := False;
    PatternChanged(nil);
  end;
  Result := FMacros;
end;

procedure TPSQLMacroQuery.SetMacros(Value: TParams);
begin
  FMacros.AssignValues(Value);
end;

procedure TPSQLMacroQuery.SetSQL(Value: TStrings);
begin
  inherited Disconnect;
  TStringList(FSQLPattern).OnChange := nil;
  FSQLPattern.Assign(Value);
  TStringList(FSQLPattern).OnChange := PatternChanged;
  PatternChanged(nil);
end;

procedure TPSQLMacroQuery.PatternChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then
  begin
    FStreamPatternChanged := True;
    Exit;
  end;
  inherited Disconnect;
  RecreateMacros;
  FPatternChanged := True;
  try
    ExpandMacros;
  finally
    FPatternChanged := False;
  end;
end;

procedure TPSQLMacroQuery.QueryChanged(Sender: TObject);
begin
  FSaveQueryChanged(Sender);
  if not FDisconnectExpected then
  begin
    SQL := inherited SQL;
  end;
end;

procedure TPSQLMacroQuery.ExpandMacros;
var
  ExpandedSQL: TStringList;
begin
  if not FPatternChanged and not FStreamPatternChanged and
    (MacroCount = 0) then Exit;
  ExpandedSQL := TStringList.Create;
  try
    Expand(ExpandedSQL);
    FDisconnectExpected := True;
    try
      inherited SQL := ExpandedSQL;
    finally
      FDisconnectExpected := False;
    end;
  finally
    ExpandedSQL.Free;
  end;
end;

procedure TPSQLMacroQuery.RecreateMacros;
var
  List: TParams;
begin
    List := TParams.Create(Self);
    try
      CreateMacros(List, PChar(FSQLPattern.Text));
      List.AssignValues(FMacros);
      FMacros.Free;
      FMacros := List;
    except
      List.Free;
    end;
end;

procedure TPSQLMacroQuery.CreateMacros(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, True, MacroChar, ['.']);
end;

procedure TPSQLMacroQuery.Expand(Query: TStrings);

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TParam;
    Found: Boolean;
  begin
    Result := S;
    for I := Macros.Count - 1 downto 0 do begin
      Param := Macros[I];
      if Param.DataType = ftUnknown then Continue;
      repeat
        P := Pos(MacroChar + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1], ['.']));
        if Found then begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then begin
            Result := Copy(Result, 1, P - 1) + Param.Text + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to FSQLPattern.Count - 1 do
    Query.Add(ReplaceString(FSQLPattern[I]));
end;

function TPSQLMacroQuery.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

function TPSQLMacroQuery.MacroByName(const Value: string): TParam;
begin
  Result := FMacros.ParamByName(Value);
end;

function TPSQLMacroQuery.IsEmpty: Boolean;
begin
  Result := IsDataSetEmpty(Self);
end;

procedure TPSQLMacroQuery.Reopen;
begin
   DisableControls;
   try
     Close;
     Open;
   finally
     EnableControls;
   end;
end;

{ TPSQLMacroQuery.IProviderSupport }
function TPSQLMacroQuery.PSGetDefaultOrder: TIndexDef;
begin
  ExpandMacros;
  Result := inherited PSGetDefaultOrder;
end;

function TPSQLMacroQuery.PSGetTableName: string;
begin
  ExpandMacros;
  Result := inherited PSGetTableName;
end;

procedure TPSQLMacroQuery.PSExecute;
begin
  ExecSQL;
end;


{ TPSQLMacroQueryThread }
constructor TPSQLMacroQueryThread.Create(Data: TPSQLDataSet; RunMode: TRunQueryMode;
  Prepare, CreateSuspended: Boolean);
begin
  inherited Create(True);
  FData := Data;
  FMode := RunMode;
  FPrepare := Prepare;
  FreeOnTerminate := True;
  FData.DisableControls;
  if not CreateSuspended then Resume;
end;

procedure TPSQLMacroQueryThread.DoTerminate;
begin
  Synchronize(FData.EnableControls);
  inherited DoTerminate;
end;

procedure TPSQLMacroQueryThread.ModeError;
begin
  SysUtils.Abort;
end;

procedure TPSQLMacroQueryThread.DoHandleException;
begin
  if (FException is Exception) and not (FException is EAbort) then
  begin
    if Assigned(Application.OnException) then
       Application.OnException(FData, Exception(FException)) else
       Application.ShowException(Exception(FException));
  end;
end;

procedure TPSQLMacroQueryThread.HandleException;
begin
  FException := TObject(ExceptObject);
  Synchronize(DoHandleException);
end;

procedure TPSQLMacroQueryThread.Execute;
begin
  try
    if FPrepare and not (FMode in [rqExecDirect]) then
    begin
      if FData is TPSQLMacroQuery then
         TPSQLMacroQuery(FData).Prepare else
         if FData is TPSQLQuery then
            TPSQLQuery(FData).Prepare;
    end;
    case FMode of
      rqOpen:
        FData.Open;
      rqExecute:
        begin
          if FData is TPSQLMacroQuery then
             TPSQLMacroQuery(FData).ExecSQL else
             if FData is TPSQLQuery then
                TPSQLQuery(FData).ExecSQL else
                ModeError;
        end;
      rqExecDirect:
        begin
          if FData is TPSQLMacroQuery then TPSQLMacroQuery(FData).ExecDirect
          else ModeError;
        end;
    end;
  except
    HandleException;
  end;
end;

end.







