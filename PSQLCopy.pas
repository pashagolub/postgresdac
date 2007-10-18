{$I PSQLdac.inc}

unit PSQLCopy;

interface

Uses Windows, Messages, SysUtils, Classes, Dialogs, PSQLTypes,
        PSQLAccess, PSQLDbTables;

type

  TCopyDirection = (cdIn, cdOut); //in = put, out = get

  TCopyMode = (cmFile, cmSTDInOut); //copy to file or to active connection
                                // STDInOut = Standart In\Out

  EPSQLCopyException = class(Exception);

  TCopyOption = (coUseOIDs, coBinary, coCSV, coHeader, coNULL, coDelimiter, coQuote, coEscape);
  TCopyOptions = set of TCopyOption;

  TAbstractCopyObject = class(TComponent)
   private
    FColumns: TStrings;
    FDelimiter: char;
    FNullValue: string;
    FFileName: string;
    FTableName: TFileName;
    FDatabase: TPSQLDatabase;
    FOptions: TCopyOptions;
    FForcedColumns: TStrings;
    FSQL: TStrings;
    FEscape: char;
    FQuote: char;
    procedure SetColumns(const Value: TStrings);
    procedure SetDatabase(const Value: TPSQLDatabase);
    procedure SetOptions(const Value: TCopyOptions);
    procedure SetForcedColumns(const Value: TStrings);
    procedure SetSQL(const Value: TStrings);
    procedure SetEscape(const Value: char);
    procedure SetQuote(const Value: char);
    procedure SetDelimiter(const Value: char);
    procedure SetNullValue(const Value: string);
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
    property BeforeCopyGet;
    property AfterCopyGet;
    property BeforeCopyPut;
    property AfterCopyPut;

  end;

//////////////////////////////////////////////////////////////////
//            Plain API Function variables definition           //
//////////////////////////////////////////////////////////////////
{var
  PQgetCopyData: TPQgetCopyData;}

implementation

{ TAbstractCopyObject }

constructor TAbstractCopyObject.Create;
begin
 inherited;
 FColumns := TStringList.Create;
 FForcedColumns := TStringList.Create;
 FSQL := TStringList.Create;
 FDelimiter := #0;
 FNullValue := '';
 FTableName := '';
end;

destructor TAbstractCopyObject.Destroy;
begin
 FColumns.Free;
 FForcedColumns.Free;
 FSQL.Free;
 inherited Destroy;
end;

procedure TCustomPSQLCopy.DoClientSideCopyGet(Stream: TStream);
var Result: PPGresult;
    LineRes: integer;
    Buffer: PChar;
    S: string;
    AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyGet) then
    FBeforeCopyGet(Self);
  FDatabase.Connected := True;
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := PQexec(AConnect.Handle, PChar(GetSQLStatement));
  try
    Stream.Position := 0;
    Stream.Size := 0;
    if PQresultStatus(Result) = PGRES_COPY_OUT then
      begin
       Repeat
        LineRes := PQgetCopyData(AConnect.Handle, @Buffer);
        If (LineRes > 0) and Assigned(Buffer) then
         begin
          S := Copy(Buffer,1,LineRes);
          Stream.Write(Pointer(S)^,length(S));
         end;
        If Buffer <> nil then
          PQfreemem (Buffer);
       Until LineRes < 0;
       If PQresultStatus(Result) <> PGRES_COMMAND_OK then
         AConnect.CheckResult;
       if Assigned(FAfterCopyGet) then
         FAfterCopyGet(Self);
     end
    else
      AConnect.CheckResult;
  finally
   PQClear(Result);
  end;
end;

procedure TCustomPSQLCopy.DoClientSideCopyPut(Stream: TStream);
var Result: PPGresult;
    Count: cardinal;
    Buffer: array[Word] of char;
    AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyPut) then
    FBeforeCopyPut(Self);
  FDatabase.Connected := True;
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := PQexec(AConnect.Handle, PChar(GetSQLStatement));
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
       If PQputCopyEnd(AConnect.Handle) <= 0 then
         AConnect.CheckResult(Result);
       if Assigned(FAfterCopyPut) then
         FAfterCopyPut(Self);
     end
    else
      AConnect.CheckResult(Result);
  finally
    PQClear(Result);
  end;
end;

procedure TCustomPSQLCopy.DoServerSideCopyGet;
var
   Result: PPGResult;
   AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyGet) then
    FBeforeCopyGet(Self);
  FDatabase.Connected := True;
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := PQexec(AConnect.Handle, PChar(GetSQLStatement));
  try
   AConnect.CheckResult(Result);
  finally
   PQClear(Result);
  end;
  if Assigned(FAfterCopyGet) then
    FAfterCopyGet(Self);
end;

procedure TAbstractCopyObject.SetColumns(const Value: TStrings);
begin
  If Assigned(Value) then FColumns.Assign(Value);
end;

procedure TAbstractCopyObject.SetDatabase(const Value: TPSQLDatabase);
begin
  If (Value <> FDatabase) then FDatabase := Value;
end;


{       TCustomCopyTool        }

procedure TCustomPSQLCopy.DoServerSideCopyPut;
  var
   Result: PPGResult;
   AConnect: TNativeConnect;
begin
  if Assigned(FBeforeCopyPut) then
    FBeforeCopyPut(Self);
  FDatabase.Connected := True;
  AConnect := TNativeConnect(FDatabase.Handle);
  Result := PQexec(AConnect.Handle, PChar(GetSQLStatement));
  try
    AConnect.CheckResult;
  finally
    PQClear(Result);
  end;
  if Assigned(FAfterCopyPut) then
    FAfterCopyPut(Self);
end;

function TCustomPSQLCopy.GetCommaSeparatedText(
  const AStrings: TStrings): string;
var i: integer;
begin
  If AStrings.Count > 0 then
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
var 
    WithStat: string;
const
    cForced: array[TCopyDirection] of string = (' FORCE NOT NULL %s ',' FORCE QUOTE %s ');
begin

 If (FCopyDirection = cdOUT) and (FSQL.Count > 0) then
   Result := Format('COPY (%s) ',[FSQL.Text])
 else
  begin
   Result := Format('COPY %s ',[FTablename]);
   if FColumns.Count > 0 then
    Result := Result + '(' + GetCommaSeparatedText(FColumns) + ')';
  end;
  
 if FCopyDirection = cdOut then
   Result := Result + ' TO '
 else
   Result := Result + ' FROM ';

 if (FCopyMode = cmFile) then
    begin
     if FFileName = '' then
       if FCopyDirection = cdIn then
         raise EPSQLCopyException.Create('FileName is required for putting data.')
       else
         FFilename := DateTimeToStr(Date)+'.pge';
     Result := Result + QuotedStr(FFileName) + ' ';
    end
  else
    if FCopyDirection = cdOUT then
     Result := Result + 'STDOUT '
    else
     Result := Result + 'STDIN ';

  WithStat := 'WITH ';
  If coUseOIDs in FOptions then WithStat := WithStat + 'OIDS ';
  If coBinary in FOptions then
    WithStat := WithStat + 'BINARY '
  else
   begin
    If (coDelimiter in FOptions) and (FDelimiter > #0) then
      WithStat := WithStat + Format(' DELIMITER %s ',[QuotedStr(FDelimiter)]);
    If (coNull in FOptions) and (FNullValue > '') then
      WithStat := WithStat + Format(' NULL %s ',[QuotedStr(FNullValue)]);
    If (coCSV in FOptions) then
     begin
      WithStat := WithStat + ' CSV ';
      If (coHeader in FOptions) then
        WithStat := WithStat + ' HEADER ';
      If (coQuote in FOptions) and (FQuote > #0) then
       WithStat := WithStat + Format(' QUOTE %s ',[FQuote]);
      If (coEscape in FOptions) and (FEscape > #0) then
       WithStat := WithStat + Format(' ESCAPE %s ',[FEscape]);
      If FForcedColumns.Count > 0 then
        WithStat := WithStat + Format(cForced[FCopyDirection],[GetCommaSeparatedText(FForcedColumns)]);
     end;
   end;
   If length(WithStat) > 5 then
     Result := Result + WithStat;
   Result := TrimRight(Result);
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

procedure TCustomPSQLCopy.LoadFromServerSideFile(const FileName: string);
begin
  FCopyDirection := cdIN;
  FCopyMode := cmSTDINOUT;
  FFileName := FileName;
  DoServerSideCopyPut;
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

procedure TCustomPSQLCopy.SaveToServerSideFile(const FileName: string);
begin
 FCopyMode := cmFile;
 FCopyDirection := cdOut;
 FFileName := FileName;
 DoServerSideCopyGet;
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
  If Value > #0 then
    FOptions := FOptions + [coDelimiter];
end;

procedure TAbstractCopyObject.SetEscape(const Value: char);
begin
  FEscape := Value;
  If Value > #0 then
    FOptions := FOptions + [coEscape];
end;

procedure TAbstractCopyObject.SetForcedColumns(const Value: TStrings);
begin
  If Assigned(Value) then FForcedColumns.Assign(Value);
end;

procedure TAbstractCopyObject.SetNullValue(const Value: string);
begin
  FNullValue := Value;
end;

procedure TAbstractCopyObject.SetOptions(const Value: TCopyOptions);
begin
  If (coBinary in Value) and (Value * [coDelimiter,coNULL,coCSV]  <> []) then
    raise EPSQLCopyException.Create('You cannot specify the coDelimiter, coNULL or coCSV options in binary mode.')
  else
    FOptions := Value;
end;

procedure TAbstractCopyObject.SetQuote(const Value: char);
begin
  FQuote := Value;
  If Value > #0 then
    FOptions := FOptions + [coQuote];
end;

procedure TAbstractCopyObject.SetSQL(const Value: TStrings);
begin
  If Assigned(Value) then FSQL.Assign(Value);
end;

end.

