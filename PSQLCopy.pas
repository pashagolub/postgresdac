{$I pSQLDAC.inc}

unit PSQLCopy;

{SVN revision: $Id$}

interface

uses {$IFDEF FPC}LCLIntf,{$ENDIF} SysUtils, Classes, PSQLTypes,
        PSQLAccess, PSQLDbTables;

type

  TCopyDirection = (cdIn, cdOut); //in = put, out = get

  TCopyMode = (cmFile, cmSTDInOut, cmProgram); //copy to file, to active connection or to process launched by server

  TCopyFormat = (cfText, cfCSV, cfBinary);

  EPSQLCopyException = class(Exception);

  TCopyOption = (coUseOIDs, coBinary, coCSV, coHeader, coNULL, coDelimiter, coQuote, coEscape, coFreeze);
  TCopyOptions = set of TCopyOption;

  TAbstractCopyObject = class(TComponent)
   private
    FColumns: TStrings;
    FDelimiter: char;
    FNullValue: string;
    FFileName: string;
    FCommandLine: string;
    FTableName: TFileName;
    FDatabase: TPSQLDatabase;
    FOptions: TCopyOptions;
    FForcedColumns: TStrings;
    FSQL: TStrings;
    FEscape: char;
    FQuote: char;
    FRowsAffected: Integer;
    FEncoding: string;
    procedure SetColumns(const Value: TStrings);
    procedure SetDatabase(const Value: TPSQLDatabase);
    procedure SetOptions(const Value: TCopyOptions);
    procedure SetForcedColumns(const Value: TStrings);
    procedure SetSQL(const Value: TStrings);
    procedure SetEscape(const Value: char);
    procedure SetQuote(const Value: char);
    procedure SetDelimiter(const Value: char);
    procedure SetNullValue(const Value: string);
    function GetDataFormat: TCopyFormat;
    procedure SetDataFormat(const Value: TCopyFormat);
    procedure SetEncoding(const Value: string);
   protected
    function GetSQLStatement: string; virtual; abstract;
    procedure DoServerSideCopyGet; virtual; abstract;
    procedure DoClientSideCopyGet(Stream: TStream);  virtual; abstract;
    procedure DoServerSideCopyPut; virtual; abstract;
    procedure DoClientSideCopyPut(Stream: TStream); virtual; abstract;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Delimiter: char read FDelimiter write SetDelimiter;
    property Quote: char read FQuote write SetQuote;
    property Escape: char read FEscape write SetEscape;
    property NullValue: string read FNullValue write SetNullValue;
    property Tablename: TFileName read FTablename write FTablename;
    property Columns: TStrings read FColumns write SetColumns;
    property ForcedColumns: TStrings read FForcedColumns write SetForcedColumns;
    property Database: TPSQLDatabase read FDatabase write SetDatabase;
    property Options: TCopyOptions read FOptions write SetOptions default [];
    property SQL: TStrings read FSQL write SetSQL;
    property RowsAffected: Integer read FRowsAffected;
    property DataFormat: TCopyFormat read GetDataFormat write SetDataFormat;
    property Encoding: string read FEncoding write SetEncoding;
  end;

  TCustomPSQLCopy = class(TAbstractCopyObject)
   protected
    FCopyDirection: TCopyDirection;
    FCopyMode: TCopyMode;
    FAfterCopyGet: TNotifyEvent;
    FBeforeCopyGet: TNotifyEvent;
    FAfterCopyPut: TNotifyEvent;
    FBeforeCopyPut: TNotifyEvent;
    function GetCommaSeparatedText(const AStrings: TStrings):string;
    function GetSQLStatement: string; override;
    procedure DoServerSideCopyGet; override;
    procedure DoClientSideCopyGet(Stream: TStream); override;
    procedure DoServerSideCopyPut; override;
    procedure DoClientSideCopyPut(Stream: TStream); override;
   public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure LoadFromStrings(Strings: TStrings);
    procedure SaveToStrings(Strings: TStrings);

    procedure LoadFromClientSideFile(const FileName: string);
    procedure SaveToClientSideFile(const FileName: string);

    procedure LoadFromServerSideFile(const FileName: string);
    procedure SaveToServerSideFile(const FileName: string);

    procedure LoadFromProgram(const CommandLine: string);
    procedure SaveToProgram(const CommandLine: string);

    property BeforeCopyGet: TNotifyEvent read FBeforeCopyGet write FBeforeCopyGet;
    property AfterCopyGet: TNotifyEvent read FAfterCopyGet write FAfterCopyGet;
    property AfterCopyPut: TNotifyEvent read FAfterCopyPut write FAfterCopyPut;
    property BeforeCopyPut: TNotifyEvent read FBeforeCopyPut write FBeforeCopyPut;
  end;


  TPSQLCopy = class(TCustomPSQLCopy)
  published
    property Delimiter;
    property Quote;
    property Escape;
    property NullValue;
    property Tablename;
    property Columns;
    property ForcedColumns;
    property Database;
    property Options;
    property SQL;
    property DataFormat;
    property Encoding;
    property BeforeCopyGet;
    property AfterCopyGet;
    property BeforeCopyPut;
    property AfterCopyPut;
  end;

implementation

uses DB{$IFNDEF DELPHI_5}, StrUtils{$ENDIF};

{ TAbstractCopyObject }

constructor TAbstractCopyObject.Create(AOwner: TComponent);
var I: integer;
begin
 inherited;
 FColumns := TStringList.Create;
 FForcedColumns := TStringList.Create;
 FSQL := TStringList.Create;
 FDelimiter := #0;
 FNullValue := '';
 FTableName := '';
 if (csDesigning in ComponentState) and Assigned(AOwner) and (DBList.Count > 0) then
 begin
  for I := DBList.Count - 1 downto 0 do
    if TCustomConnection(DBList[I]).Owner = AOwner then
    begin
       Database := TPSQLDatabase(DBList[I]);
       Break;
    end;
    if not Assigned(Database) then
      Database := TPSQLDatabase(DBList[DBList.Count - 1]);
 end;
end;

destructor TAbstractCopyObject.Destroy;
begin
 FColumns.Free;
 FForcedColumns.Free;
 FSQL.Free;
 inherited Destroy;
end;

function TAbstractCopyObject.GetDataFormat: TCopyFormat;
begin
  if coCSV in FOptions then
    Result := cfCSV
  else
    if coBinary in FOptions then
      Result := cfBinary
    else
      Result := cfText;
end;

procedure TAbstractCopyObject.SetColumns(const Value: TStrings);
begin
  if Assigned(Value) then FColumns.Assign(Value);
end;

procedure TAbstractCopyObject.SetDatabase(const Value: TPSQLDatabase);
begin
  if (Value <> FDatabase) then FDatabase := Value;
end;

procedure TCustomPSQLCopy.DoClientSideCopyGet(Stream: TStream);
var Result: PPGresult;
    LineRes: integer;
    Buffer: PAnsiChar;
    S: AnsiString;
    AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyGet) then
    FBeforeCopyGet(Self);
  FRowsAffected := -1;
  if not FDatabase.Connected then DatabaseError(SDatabaseClosed);
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := _PQExecute(AConnect, GetSQLStatement);
  try
    Stream.Position := 0;
    Stream.Size := 0;
    try
      if PQresultStatus(Result) = PGRES_COPY_OUT then
        begin
         Repeat
          LineRes := PQgetCopyData(AConnect.Handle, @Buffer);
          if (LineRes > 0) and Assigned(Buffer) then
           begin
            S := Copy(Buffer,1,LineRes);
            Stream.Write(Pointer(S)^,length(S));
           end;
          if Buffer <> nil then
            PQfreemem (Buffer);
         Until LineRes < 0;
         if PQresultStatus(Result) <> PGRES_COMMAND_OK then
           AConnect.CheckResult;
         if Assigned(FAfterCopyGet) then
           FAfterCopyGet(Self);
         FRowsAffected :=  StrToIntDef(String(PQcmdTuples(Result)), -1);
       end
      else
        AConnect.CheckResult;
    except
     on E: EPSQLException do
        raise EPSQLDatabaseError.Create(FDatabase.Engine, FDatabase.Engine.CheckError);
     else
        raise;
    end;
  finally
   PQClear(Result);
  end;
end;

procedure TCustomPSQLCopy.DoClientSideCopyPut(Stream: TStream);
var Result, Result2: PPGresult;
    Count: cardinal;
    Buffer: array[Word] of AnsiChar;
    AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyPut) then
    FBeforeCopyPut(Self);
  FRowsAffected := -1;
  if not FDatabase.Connected then DatabaseError(SDatabaseClosed);
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := _PQExecute(AConnect, GetSQLStatement);
  try
    try
      Stream.Position := 0;
      if PQresultStatus(Result) = PGRES_COPY_IN then
        begin
         Count := Stream.Read(Buffer,Length(Buffer));
         while Count > 0 do
          if PQputCopyData(AConnect.Handle, Buffer, Count) <= 0 then
            AConnect.CheckResult(Result)
          else
            Count := Stream.Read(Buffer,Length(Buffer));
         if PQputCopyEnd(AConnect.Handle) <= 0 then
           AConnect.CheckResult(Result)
         else
          begin
           Result2 := PQgetResult(AConnect.Handle);
           try
            AConnect.CheckResult(Result2);
            FRowsAffected := StrToIntDef(String(PQcmdTuples(Result2)), -1);
           finally
            PQClear(Result2);
           end;
          end;
         if Assigned(FAfterCopyPut) then
           FAfterCopyPut(Self);
       end
      else
        AConnect.CheckResult(Result);
    finally
      PQClear(Result);
    end;
  except
   on E: EPSQLException do
      raise EPSQLDatabaseError.Create(FDatabase.Engine, FDatabase.Engine.CheckError);
   else
      raise;
  end;
end;

procedure TCustomPSQLCopy.DoServerSideCopyGet;
var
   Result: PPGResult;
   AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyGet) then
    FBeforeCopyGet(Self);
  FRowsAffected := -1;
  if not FDatabase.Connected then DatabaseError(SDatabaseClosed);
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := _PQExecute(AConnect, GetSQLStatement);
  try
    try
     AConnect.CheckResult(Result);
     FRowsAffected := StrToIntDef(String(PQcmdTuples(Result)), -1);
    finally
     PQClear(Result);
    end;
  except
   on E: EPSQLException do
      raise EPSQLDatabaseError.Create(FDatabase.Engine, FDatabase.Engine.CheckError);
   else
      raise;
  end;
  if Assigned(FAfterCopyGet) then
    FAfterCopyGet(Self);
end;

procedure TCustomPSQLCopy.DoServerSideCopyPut;
  var
   Result: PPGResult;
   AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyPut) then
    FBeforeCopyPut(Self);
  FRowsAffected := -1;
  if not FDatabase.Connected then DatabaseError(SDatabaseClosed);
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := _PQExecute(AConnect, GetSQLStatement);
  try
    try
      AConnect.CheckResult;
      FRowsAffected := StrToIntDef(String(PQcmdTuples(Result)), -1);
    finally
      PQClear(Result);
    end;
  except
   on E: EPSQLException do
      raise EPSQLDatabaseError.Create(FDatabase.Engine, FDatabase.Engine.CheckError);
   else
      raise;
  end;
  if Assigned(FAfterCopyPut) then
    FAfterCopyPut(Self);
end;

function TCustomPSQLCopy.GetCommaSeparatedText(
  const AStrings: TStrings): string;
var i: integer;
begin
  if AStrings.Count > 0 then
    Result := AStrings[0]
  else
   begin
    Result := '';
    Exit;
   end;
  for i:=1 to AStrings.Count-1 do
      Result := Result  + ', ' + AStrings[i];
end;

function TCustomPSQLCopy.GetSQLStatement: string;

    function GetOldWithStmt: string;
    const
      cForced: array[TCopyDirection] of string = (' FORCE NOT NULL %s ',' FORCE QUOTE %s ');
    begin
      if coUseOIDs in FOptions then Result := Result + 'OIDS ';
      if coBinary in FOptions then
        Result := Result + 'BINARY '
      else
       begin
        if (coDelimiter in FOptions) and (FDelimiter > #0) then
          Result := Result + Format(' DELIMITER %s ',[QuotedStr(FDelimiter)]);
        if (coNull in FOptions) and (FNullValue > '') then
          Result := Result + Format(' NULL %s ',[QuotedStr(FNullValue)]);
        if (coCSV in FOptions) then
         begin
          Result := Result + ' CSV ';
          if (coHeader in FOptions) then
            Result := Result + ' HEADER ';
          if (coQuote in FOptions) and (FQuote > #0) then
           Result := Result + Format(' QUOTE %s ',[FQuote]);
          if (coEscape in FOptions) and (FEscape > #0) then
           Result := Result + Format(' ESCAPE %s ',[FEscape]);
          if FForcedColumns.Count > 0 then
            Result := Result + Format(cForced[FCopyDirection],[GetCommaSeparatedText(FForcedColumns)]);
         end;
       end;
      if Result > '' then
        Result := ' WITH ' + Result;
    end;

    function GetNewWithStmt: string;
    const
      cFormat: array[TCopyFormat] of string = ('''text''', '''csv''', '''binary''');
      cForced: array[TCopyDirection] of string = (', FORCE NOT NULL %s', ', FORCE QUOTE %s');
    begin
      Result := 'FORMAT ' + cFormat[GetDataFormat];
      if coUseOIDs in FOptions then Result := Result + ', OIDS true';
      if coFreeze in FOptions then Result := Result + ', FREEZE true';

      if not (coBinary in FOptions) then
       begin
        if (coDelimiter in FOptions) and (FDelimiter > #0) then
          Result := Result + Format(', DELIMITER %s ',[QuotedStr(FDelimiter)]);
        if (coNull in FOptions) and (FNullValue > '') then
          Result := Result + Format(', NULL %s ',[QuotedStr(FNullValue)]);
        if (coCSV in FOptions) then
         begin
          if (coHeader in FOptions) then
            Result := Result + ', HEADER true';
          if (coQuote in FOptions) and (FQuote > #0) then
           Result := Result + Format(', QUOTE %s ',[QuotedStr(FQuote)]);
          if (coEscape in FOptions) and (FEscape > #0) then
           Result := Result + Format(', ESCAPE %s ',[QuotedStr(FEscape)]);
          if FForcedColumns.Count > 0 then
            Result := Result + Format(cForced[FCopyDirection],[GetCommaSeparatedText(FForcedColumns)]);
         end;
       end;
      if FEncoding > '' then
        Result := Result + ', ENCODING ' + QuotedStr(FEncoding);
      Result := ' WITH (' + Result + ')';
    end;

begin
  if (FCopyDirection = cdOUT) and (FSQL.Count > 0) then
    Result := Format('COPY (%s) ',[Trim(FSQL.Text)])
  else
  begin
    Result := Format('COPY %s ',[FTablename]);
    if FColumns.Count > 0 then
      Result := Result + '(' + GetCommaSeparatedText(FColumns) + ')';
  end;
  Result := Result + ifthen(FCopyDirection = cdOut, ' TO ', ' FROM ');
  case FCopyMode of
    cmFile:
      begin
        if FFileName = '' then
          if FCopyDirection = cdIn then
            raise EPSQLCopyException.Create('FileName is required for putting data.')
          else
            FFilename := DateTimeToStr(Date) + '.pge';
        Result := Result + QuotedStr(FFileName);
      end;
    cmSTDInOut:
      Result := Result + ifthen(FCopyDirection = cdOUT, 'STDOUT', 'STDIN');
    cmProgram:
      Result := Result + 'PROGRAM ' + QuotedStr(FCommandLine);
  end;

  if Assigned(FDatabase) then
      Result := Result + ifthen(FDatabase.ServerVersionAsInt < 090000, GetOldWithStmt(), GetNewWithStmt());
end;


procedure TCustomPSQLCopy.LoadFromClientSideFile(const FileName: string);
var FS: TFileStream;
begin
  FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
   LoadFromStream(FS);
  finally
   FS.Free;
  end;
end;

procedure TCustomPSQLCopy.LoadFromProgram(const CommandLine: string);
begin
  FCopyDirection := cdIN;
  FCopyMode := cmProgram;
  FCommandLine := CommandLine;
  DoServerSideCopyPut();
end;

procedure TCustomPSQLCopy.LoadFromServerSideFile(const FileName: string);
begin
  FCopyDirection := cdIN;
  FCopyMode := cmSTDINOUT;
  FFileName := FileName;
  DoServerSideCopyPut();
end;

procedure TCustomPSQLCopy.LoadFromStream(Stream: TStream);
begin
  FCopyDirection := cdIN;
  FCopyMode := cmSTDINOUT;
  if not Assigned(Stream) then Exit;
  DoClientSideCopyPut(Stream);
end;

procedure TCustomPSQLCopy.LoadFromStrings(Strings: TStrings);
var MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
   Strings.SaveToStream(MS);
   LoadFromStream(MS);
  finally
   MS.Free;
  end;
end;

procedure TCustomPSQLCopy.SaveToClientSideFile(const FileName: string);
var FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
 try
  SaveToStream(FS);
 finally
  FS.Free;
 end;
end;

procedure TCustomPSQLCopy.SaveToProgram(const CommandLine: string);
begin
 FCopyMode := cmProgram;
 FCopyDirection := cdOUT;
 FCommandLine := CommandLine;
 DoServerSideCopyGet();
end;

procedure TCustomPSQLCopy.SaveToServerSideFile(const FileName: string);
begin
 FCopyMode := cmFile;
 FCopyDirection := cdOut;
 FFileName := FileName;
 DoServerSideCopyGet();
end;

procedure TCustomPSQLCopy.SaveToStream(Stream: TStream);
begin
 FCopyDirection := cdOUT;
 FCopyMode := cmSTDINOUT;
 if not Assigned(Stream) then
   raise EPSQLCopyException.Create('Stream not assigned');
 DoClientSideCopyGet(Stream);
end;

procedure TCustomPSQLCopy.SaveToStrings(Strings: TStrings);
var MS: TMemoryStream;
begin
 if Assigned(Strings) then
  begin
    MS := TMemoryStream.Create;
   try
    SaveToStream(MS);
    MS.Position := 0;
    Strings.LoadFromStream(MS);
   finally
    MS.Free;
   end;
  end;
end;


procedure TAbstractCopyObject.SetDelimiter(const Value: char);
begin
  FDelimiter := Value;
  if Value > #0 then
    FOptions := FOptions + [coDelimiter];
end;

procedure TAbstractCopyObject.SetEncoding(const Value: string);
begin
  FEncoding := Trim(Value);
end;

procedure TAbstractCopyObject.SetEscape(const Value: char);
begin
  FEscape := Value;
  if Value > #0 then
    FOptions := FOptions + [coEscape];
end;

procedure TAbstractCopyObject.SetForcedColumns(const Value: TStrings);
begin
  if Assigned(Value) then FForcedColumns.Assign(Value);
end;

procedure TAbstractCopyObject.SetDataFormat(const Value: TCopyFormat);
begin
  if GetDataFormat <> Value then
    case Value of
      cfText: FOptions := FOptions - [coBinary, coCSV];
      cfCSV: FOptions := FOptions - [coBinary] + [coCSV];
      cfBinary: FOptions := FOptions - [coDelimiter, coNULL, coCSV] + [coBinary];
    end;
end;

procedure TAbstractCopyObject.SetNullValue(const Value: string);
begin
  FNullValue := Value;
end;

procedure TAbstractCopyObject.SetOptions(const Value: TCopyOptions);
begin
  if (coBinary in Value) and (Value * [coDelimiter, coNULL, coCSV]  <> []) then
    raise EPSQLCopyException.Create('You cannot specify the coDelimiter, coNULL or coCSV options in binary mode.')
  else
    FOptions := Value;
end;

procedure TAbstractCopyObject.SetQuote(const Value: char);
begin
  FQuote := Value;
  if Value > #0 then
    FOptions := FOptions + [coQuote];
end;

procedure TAbstractCopyObject.SetSQL(const Value: TStrings);
begin
  if Assigned(Value) then FSQL.Assign(Value);
end;

end.

