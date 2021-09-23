{$I PSQLDAC.inc}
unit PSQLDump;

{SVN revision: $Id$}

interface

Uses Classes, SysUtils, Db, PSQLTypes, Math, PSQLDbTables;

type
  Tv3_Dump = function (AppName: PAnsiDACChar; LogFileName: PAnsiDACChar; Params : PAnsiDACChar): longint; cdecl;
  Tv3_Restore = function (AppName: PAnsiDACChar; LogFileName : PAnsiDACChar; Params : PAnsiDACChar): longint; cdecl;
  Tpdmbvm_GetVersionAsInt = function():integer; cdecl;
  Tpdmbvm_SetErrorCallBackProc = procedure(ProcAddr : pointer); cdecl;
  Tpdmbvm_SetLogCallBackProc = procedure(ProcAddr : pointer); cdecl;

  TpdmvmParams = class
  private
    FParams : TStringList;
    FArr : PAnsiDACChar;

    procedure ClearMem();
  public
    constructor Create();
    destructor Destroy();override;

    procedure Add(aStr : string);
    procedure Clear();

    function GetPCharArray() : PAnsiDACChar;
  end;


  TLogEvent = procedure (Sender: TObject; const LogMessage: string) of object;

  TLibraryLoadEvent = procedure (Sender: TObject; var FileName: string) of object;

  EPSQLDumpException = class(Exception);

  TDumpRestoreSection = (drsPreData, drsData, drsPostData);

  TDumpRestoreSections = set of TDumpRestoreSection;

  TDumpOption = (doDataOnly, doIncludeBLOBs, doClean, doCreate, doInserts,
                doColumnInserts, doIgnoreVersion, doOIDs, doNoOwner,
                doSchemaOnly, doVerbose, doNoPrivileges, doDisableDollarQuoting,
                doDisableTriggers, doUseSetSessionAuthorization, doNoTablespaces,
                doQuoteAllIdentifiers, doNoSecurityLabels, doNoUnloggedTableData,
                doSerializableDeferrable, doNoSynchronizedSnapshots, doIfExists,
                doEnableRowSecurity, doStrictNames, doNoBlobs, doNoSync,
                doNoSubscriptions, doNoPublications, doLoadViaPartitionRoot,
                doNoComments);

  TDumpOptions = set of TDumpOption;

  TDumpStrOption = (dsoSchema, dsoSuperuser, dsoTable, dsoExcludeSchema,
                    dsoExcludeTable, dsoEncoding, dsoRole, dsoExcludeTableData,
                    dsoSnapshot);

  TDumpFormat = (dfPlain, dfTarArchive, dfCompressedArchive, dfDirectory);

  TCompressLevel = 0..9;

  TPSQLDump = class(TComponent)
  private
    FAbout      : TPSQLDACAbout;
    {$IFDEF M_DEBUG}
    FParamStr   : string;
    {$ENDIF}
    {$IFDEF NEXTGEN}[Weak]{$ENDIF} FDatabase: TPSQLDatabase;
    FCompressLevel: TCompressLevel;
    FDumpFormat : TDumpFormat;
    FDumpOptions : TDumpOptions;
    FDumpStrOptions : array[TDumpStrOption] of string;
    FBeforeDump : TNotifyEvent;
    FAfterDump  : TNotifyEvent;
    FRewriteFile: boolean;
    FmiParams: TpdmvmParams;
    FTableNames: TStrings;
    FSchemaNames: TStrings;
    FExcludeTables: TStrings;
    FExcludeSchemas: TStrings;
    FOnLog: TLogEvent;
    FLockWaitTimeout: cardinal;
    FOnLibraryLoad: TLibraryLoadEvent;
    FJobs: cardinal;
    FExcludeTablesData: TStrings;
    FSections: TDumpRestoreSections;
    procedure SetDatabase(const Value : TPSQLDatabase);
    procedure SetCompressLevel(const Value: TCompressLevel);
    function GetStrOptions(const Index: Integer): string;
    procedure SetStrOptions(const Index: Integer; const Value: string);
    procedure Dump(const TargetFile, LogFile: string);
    procedure SetTableNames(const Value: TStrings);
    procedure SetSchemaNames(const Value: TStrings);
    procedure SetExcludeSchemas(const Value: TStrings);
    procedure SetExcludeTables(const Value: TStrings);
    procedure DoLog(const Value: string);
    function GetVersionAsInt: integer;
    function GetVersionAsStr: string;
    procedure ReadTableName(Reader: TReader); //deal with old missing properties
    procedure ReadSchemaName(Reader: TReader); //deal with old missing properties
    procedure SetLockWaitTimeout(const Value: cardinal);
    procedure SetDumpOptions(const Value: TDumpOptions);
    procedure SetExcludeTablesData(const Value: TStrings);
  protected
    procedure CheckDependencies;
    procedure DefineProperties(Filer: TFiler); override;
    function GetParameters(OutputFileName: string): PAnsiDACChar;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure DumpToStream(Stream: TStream); overload;
    procedure DumpToStream(Stream: TStream; Log: TStrings); overload;
    procedure DumpToStream(Stream: TStream; LogFileName: string); overload;
    procedure DumpToFile(const FileName: string; Log: TStrings); overload;
    procedure DumpToFile(const FileName, LogFileName: string); overload;
    property VersionAsInt: integer read GetVersionAsInt;
    property VersionAsStr: string read GetVersionAsStr;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property CompressLevel: TCompressLevel read FCompressLevel write SetCompressLevel default 0;
    property Database   : TPSQLDatabase read FDatabase write SetDatabase;
    property DumpFormat : TDumpFormat read FDumpFormat write FDumpFormat default dfPlain;
    property Encoding: string index dsoEncoding read GetStrOptions write SetStrOptions;
    property ExcludeSchemas: TStrings read FExcludeSchemas write SetExcludeSchemas;
    property ExcludeTables: TStrings read FExcludeTables write SetExcludeTables;
    property ExcludeTablesData: TStrings read FExcludeTablesData write SetExcludeTablesData;
    property LockWaitTimeout: cardinal read FLockWaitTimeout write SetLockWaitTimeout default 0;
    property Options : TDumpOptions read FDumpOptions write SetDumpOptions default [];
    property RewriteFile: boolean read FRewriteFile write FRewriteFile default True;
    property Role: string index dsoRole read GetStrOptions write SetStrOptions;
    property SchemaNames: TStrings read FSchemaNames write SetSchemaNames;
    property SuperUserName: string  index dsoSuperUser read GetStrOptions write SetStrOptions;
    property TableNames: TStrings read FTableNames write SetTableNames;
    property Jobs: cardinal read FJobs write FJobs;
    property Sections: TDumpRestoreSections read FSections write FSections;
    property Snapshot: string index dsoSnapshot read GetStrOptions write SetStrOptions;
    property AfterDump : TNotifyEvent read FAfterDump write FAfterDump;
    property BeforeDump  : TNotifyEvent read FBeforeDump write FBeforeDump;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnLibraryLoad: TLibraryLoadEvent read FOnLibraryLoad write FOnLibraryLoad;
  end;


{TPSQLRestore stuff}
  EPSQLRestoreException = class(Exception);

  TRestoreFormat = (rfAuto, rfTarArchive, rfCompressedArchive, rfDirectory);

  TRestoreOption = (roDataOnly, roClean, roCreate, roExitOnError, roIgnoreVersion,
            roList, roNoOwner, roSchemaOnly, roVerbose, roNoPrivileges,
            roDisableTriggers, roUseSetSessionAuthorization, roSingleTransaction,
            roNoDataForFailedTables, roNoTablespaces, roNoSecurityLabels, roIfExists,
            roEnableRowSecurity, roStrictNames, roNoSubscriptions, roNoPublications);

  TRestoreOptions = set of TRestoreOption;

  TRestoreStrOption = (rsoTable, rsoSuperUser, rsoDBName,
                  rsoFileName, rsoIndex, rsoListFile, rsoFunction,
                  rsoTrigger, rsoRole, rsoSchemaName, rsoExcludeSchema);

  TPSQLRestore = class(TComponent)
  private
    FAbout: TPSQLDACAbout;
    {$IFDEF M_DEBUG}
    FParamStr: string;
    {$ENDIF}
    {$IFDEF NEXTGEN}[Weak]{$ENDIF} FDatabase: TPSQLDatabase;
    FRestoreFormat: TRestoreFormat;
    FRestoreOptions: TRestoreOptions;
    FRestoreStrOptions: array [TRestoreStrOption] of string;
    FBeforeRestore: TNotifyEvent;
    FAfterRestore: TNotifyEvent;
    FmiParams: TpdmvmParams;
    FOnLog: TLogEvent;
    FJobs: cardinal;
    FOnLibraryLoad: TLibraryLoadEvent;
    FTableNames: TStrings;
    FSections: TDumpRestoreSections;
    FSchemaNames: TStrings;
    FExcludeSchemas: TStrings;
    procedure SetDatabase(const Value : TPSQLDatabase);
    function GetStrOptions(const AnIndex: Integer): string;
    procedure SetStrOptions(const AnIndex: Integer; const Value: string);
    procedure Restore(const SourceFile, LogFile: string);
    procedure DoLog(const Value: string);
    function GetVersionAsInt: integer;
    procedure SetJobs(const Value: cardinal);
    procedure SetRestoreOptions(const Value: TRestoreOptions);
    function GetVersionAsStr: string;
    procedure SetTableNames(const Value: TStrings);
    procedure ReadTableName(Reader: TReader);
    procedure ReadSchemaName(Reader: TReader);
    procedure SetSchemaNames(const Value: TStrings);
    procedure SetExcludeSchemas(const Value: TStrings);
  protected
    procedure CheckDependencies;
    procedure DefineProperties(Filer: TFiler); override;
    function GetParameters(SourceFileName: string): PAnsiDACChar;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    procedure RestoreFromFile(const FileName: string; Log: TStrings); overload;
    procedure RestoreFromFile(const FileName, LogFileName: string); overload;
    property VersionAsInt: integer read GetVersionAsInt;
    property VersionAsStr: string read GetVersionAsStr;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property Database   : TPSQLDatabase read FDatabase write SetDatabase;
    property DBName: string  index rsoDBName read GetStrOptions write SetStrOptions;
    property FunctionDecl: string  index rsoFunction read GetStrOptions write SetStrOptions;
    property Index: string  index rsoIndex read GetStrOptions write SetStrOptions;
    property Jobs: cardinal read FJobs write SetJobs;
    property ListFile: string  index rsoListFile read GetStrOptions write SetStrOptions;
    property Options : TRestoreOptions read FRestoreOptions write SetRestoreOptions;
    property OutputFileName: string  index rsoFileName read GetStrOptions write SetStrOptions;
    property RestoreFormat : TRestoreFormat read FRestoreFormat write FRestoreFormat;
    property Role: string index dsoRole read GetStrOptions write SetStrOptions;
    property SuperUserName: string  index rsoSuperUser read GetStrOptions write SetStrOptions;
    property Trigger: string  index rsoTrigger read GetStrOptions write SetStrOptions;
    property TableNames: TStrings read FTableNames write SetTableNames;
    property SchemaNames: TStrings read FSchemaNames write SetSchemaNames;
    property ExcludeSchemas: TStrings read FExcludeSchemas write SetExcludeSchemas;
    property Sections: TDumpRestoreSections read FSections write FSections;
    property AfterRestore : TNotifyEvent read FAfterRestore write FAfterRestore;
    property BeforeRestore  : TNotifyEvent read FBeforeRestore write FBeforeRestore;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnLibraryLoad: TLibraryLoadEvent read FOnLibraryLoad write FOnLibraryLoad;
  end;

const
  DumpRestoreCommandLineSectionParameters: array[TDumpRestoreSection] of string = (
   '--section=pre-data', //drsPreData
   '--section=data',     //drsData
   '--section=post-data' //drsPostData
  );

  DumpCommandLineBoolParameters: array[TDumpOption] of string = (
   '--data-only',                      //doDataOnly
   '--blobs',                          //doIncludeBLOBs
   '--clean',                          //doClean
   '--create',                         //doCreate
   '--inserts',                        //doInserts
   '--column-inserts',                 //doColumnInserts, aka '--attribute-inserts'
   '--ignore-version',                 //doIgnoreVersion
   '--oids',                           //doOIDs
   '--no-owner',                       //doNoOwner
   '--schema-only',                    //doSchemaOnly
   '--verbose',                        //doVerbose,
   '--no-privileges',                  //doNoPrivileges, aka '--no-acl'
   '--disable-dollar-quoting',         //doDisableDollarQuoting,
   '--disable-triggers',               //doDisableTriggers,
   '--use-set-session-authorization',  //doUseSetSessionAuthorization,
   '--no-tablespaces',                 //doNoTablespaces
   '--quote-all-identifiers',          //doQuoteAllIdentifiers
   '--no-security-labels',             //doNoSecurityLabels
   '--no-unlogged-table-data',         //doNoUnloggedTableData
   '--serializable-deferrable',        //doSerializableDeferrable
   '--no-synchronized-snapshots',      //doNoSynchronizedSnapshots
   '--if-exists',                      //doIfExists
   '--enable-row-security',            //doEnableRowSecurity
   '--strict-names',                   //doStrictNames
   '--no-blobs',                       //doNoBlobs
   '--no-sync',                        //doNoSync
   '--no-subscriptions',               //doNoSubscriptions
   '--no-publications',                //doNoPublications
   '--load-via-partition-root',        //doLoadViaPartitionRoot
   '--no-comments'                     //doNoComments


  );

  RestoreCommandLineBoolParameters: array[TRestoreOption] of string = (
   //--no-reconnect   obsolete
   '--data-only',                //roDataOnly
   '--clean',                    //roClean
   '--create',                   //roCreate
   '--exit-on-error',            //roExitOnError
   '--ignore-version',           //roIgnoreVersion
   '--list',                     //roList, aka '--no-acl'
   '--no-owner',                 //roNoOwner
   '--schema-only',              //roSchemaOnly
   '--verbose',                  //roVerbose,
   '--no-privileges',            //roNoPrivileges,
   '--disable-triggers',         //roDisableTriggers,
   '--use-set-session-authorization',  //roUseSetSessionAuthorization
   '--single-transaction',             //roSingleTransaction
   '--no-data-for-failed-tables',      //roNoDataForFailedTables
   '--no-tablespaces',                 //roNoTablespaces
   '--no-security-labels',             //roNoSecurityLabels
   '--if-exists',                      //roIfExists
   '--enable-row-security',            //roEnableRowSecurity
   '--strict-names',                   //roStrictNames
   '--no-subscriptions',               //roNoSubscriptions
   '--no-publications'                 //roNoPublications
   );

  DumpCommandLineStrParameters: array[TDumpStrOption] of string =(
   '--schema=',             //dsoSchema
   '--superuser=',          //dsoSuperuser
   '--table=',              //dsoTable
   '--exclude-schema=',     //dsoExcludeSchema
   '--exclude-table=',      //dsoExcludeTable
   '--encoding=',           //dsoEncoding
   '--role=',               //dsoRole
   '--exclude-table-data=', //dsoExcludeTableData
   '--snapshot='            //dsoSnapshot
  );

  RestoreCommandLineStrParameters: array[TRestoreStrOption] of string =(
   '--table=',        //rsoTable
   '--superuser=',    //rsoSuperuser
   '--dbname=',       //rsoDBName,
   '--file=',         //rsoFileName,
   '--index=',        //rsoIndex,
   '--use-list=',     //rsoListFile,
   '--function=',     //rsoFunction,
   '--trigger=',      //rsoTrigger
   '--role=',         //dsoRole
   '--schema=',       //rsoSchemaName
   '--exclude-schema=' //rsoExcludeSchema
  );


  DumpCommandLineFormatValues: array[TDumpFormat] of string = (
  '--format=p',  //plain
  '--format=t',  //tar archive
  '--format=c',  //custom archive
  '--format=d'   //directory
  );

  RestoreCommandLineFormatValues: array[TRestoreFormat] of string = (
  '',             //auto
  '--format=t',  //tar archive
  '--format=c',  //custom archive
  '--format=d'   //directory
  );

function GetTempFileName: string;

implementation

uses PSQLAccess,
{$IFDEF MSWINDOWS}
  Windows
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysTypes, Posix.Stdio, Posix.Stdlib
{$ENDIF};

var ProccessOwner: TComponent = nil;

function IntVerToStr(VerInt: integer): string;
var Major, Minor, Revision: integer;
begin
  Major := VerInt div 10000;
  Minor := VerInt mod 10000 div 100;
  Revision := VerInt mod 100;
  Result := Format('%d.%d.%d', [Major, Minor, Revision]);
end;

function StringToAnsiChar(CodePage: Cardinal; Src: {$IFNDEF NEXTGEN}WideString{$ELSE}String{$ENDIF}): PAnsiDACChar;
var
  Len: integer;
begin
  Result := nil;
  if Src = EmptyStr then Exit;
  Len := {$IFDEF DELPHI_16}
            LocaleCharsFromUnicode
         {$ELSE}
            WideCharToMultiByte
         {$ENDIF}(codePage, 0, PWideChar(Src), Length(Src), nil, 0, nil, nil);
  if Len = 0 then Exit;
  GetMem(Result, Len + 1);
  if {$IFDEF DELPHI_16}
        LocaleCharsFromUnicode
     {$ELSE}
        WideCharToMultiByte
     {$ENDIF}(codePage, 0, PWideChar(Src), Length(Src), PAnsiDACChar(Result), Len, nil, nil) = 0 then
    ReallocMem(Result, 0)
  else
    Result[Len] := #0;
end;

function GetTempPath: string;
{$IFDEF MSWINDOWS}
var
  Len: Integer;
begin
  SetLastError(ERROR_SUCCESS);

  // get memory for the buffer retaining the temp path (plus null-termination)
  SetLength(Result, MAX_PATH);
  Len := Windows.GetTempPath(MAX_PATH, PChar(Result));
  if Len = 0 then
    Result := '';
end;
{$ENDIF}
{$IFDEF POSIX}
const
  CEnvVars: array[0..2] of DACAString = ('TMPDIR', 'TMP', 'TEMP'); // Do not localize
  CTmpDir = '/tmp'; // Do not localize

var
  LTempPathVar: PAnsiDACChar;
  I: Integer;
  {$IFDEF NEXTGEN}
  M: TMarshaller;
  {$ENDIF}
begin
  { Lookup env variables, in order: TMPDIR, TMP, TEMP }
  for I := Low(CEnvVars) to High(CEnvVars) do
  begin
    LTempPathVar := getenv({$IFNDEF NEXTGEN}
                            PAnsiChar(CEnvVars[I])
                          {$ELSE}
                            M.AsAnsi(CEnvVars[I]).ToPointer
                          {$ENDIF}
                          );

    if (LTempPathVar <> nil) and (LTempPathVar^ <> #0) then
    begin
      { We have found our temporary path }
      Break;
    end;
  end;

  { Get the UTF16 value out of the UTF8 one. The last resort is to fallback to /tmp }
  if LTempPathVar <> nil then
    Result := UTF8ToUnicodeString(LTempPathVar)
  else
    Result := CTmpDir;
end;
{$ENDIF}

function GetTempFileName: string;
{$IFDEF MSWINDOWS}
var
  TempPath: string;
  ErrCode: UINT;
begin
  TempPath := GetTempPath();
  SetLength(Result, MAX_PATH);

  SetLastError(ERROR_SUCCESS);
  ErrCode := Windows.GetTempFileName(PChar(TempPath), 'tmp', 0, PChar(Result)); // DO NOT LOCALIZE
  if ErrCode = 0 then
    raise EInOutError.Create(SysErrorMessage(GetLastError));

  SetLength(Result, StrLen(PChar(Result)));
end;
{$ENDIF}
{$IFDEF POSIX}
var
  LTempPath: PAnsiDACChar;
begin
  { Obtain a temporary file name }
  LTempPath := tmpnam(nil);

  { Convert to UTF16 or leave blank on possible error }
  if LTempPath <> nil then
    Result := UTF8ToUnicodeString(LTempPath)
  else
    Result := '';
end;
{$ENDIF}

{ TpdmvmParams }
procedure TpdmvmParams.Add(aStr: string);
begin
  FParams.Add(aStr);
end;

procedure TpdmvmParams.Clear;
begin
  FParams.Clear();
end;

procedure TpdmvmParams.ClearMem;
var
  s : PAnsiDACChar;
  p : PInteger; //32-bit pointer
begin
  if FArr <> nil then
  begin
    p := Pointer(FArr);
    repeat
      s := PAnsiDACChar(p^);
      if s <> nil then
        FreeMem(s);
      Inc(p);
    until s = nil;
    FreeMem(FArr);
    FArr := nil;
  end;
end;

constructor TpdmvmParams.Create;
begin
  FParams := TStringList.Create();
  FArr := nil;
end;

destructor TpdmvmParams.Destroy;
begin
  ClearMem();
  FParams.Free();

  inherited;
end;

function TpdmvmParams.GetPcharArray: PAnsiDACChar;
var
  s : PAnsiDACChar;
  p : PInteger; //32-bit pointer
  i : integer;
begin
  ClearMem();

  GetMem(FArr, SizeOf(PAnsiDACChar) * (FParams.Count + 1));
  p := Pointer(FArr);
  for i:=0 to FParams.Count - 1 do
  begin
    if FParams.Names[i] = '--file' then
      S := StringToAnsiChar(CP_ACP, FParams[i])
    else
      S := StringToAnsiChar(CP_UTF8, FParams[i]);
    p^ := Integer(s);
    Inc(p);
  end;
  p^ := 0;
  Result := FArr;
end;

{TPSQLDump}

Constructor TPSQLDump.Create(Owner : TComponent);
var I: integer;
begin
  inherited Create(Owner);
  FDumpOptions := [];
  {$IFNDEF NEXTGEN}
  ZeroMemory(@FDumpStrOptions,sizeof(FDumpStrOptions));
  {$ELSE}
  FillChar(FDumpStrOptions, sizeof(FDumpStrOptions), 0);
  {$ENDIF}
  FRewriteFile := True;
  FTableNames := TStringList.Create;
  FExcludeTables := TStringList.Create;
  FExcludeTablesData := TStringList.Create;
  FExcludeSchemas := TStringList.Create;
  FSchemaNames := TStringList.Create;
  FmiParams := TpdmvmParams.Create;
  if (csDesigning in ComponentState) and Assigned(Owner) then
    for I := Owner.ComponentCount - 1 downto 0 do
      if Owner.Components[I] is TPSQLDatabase then
      begin
        Database := Owner.Components[I] as TPSQLDatabase;
        Break;
      end;
end;

destructor TPSQLDump.Destroy;
begin
  FmiParams.Free;
  FTableNames.Free;
  FSchemaNames.Free;
  FExcludeTables.Free;
  FExcludeTablesData.Free;
  FExcludeSchemas.Free;
  inherited Destroy;
end;

Procedure TPSQLDump.Notification( AComponent: TComponent; Operation: TOperation );
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
    FDatabase := nil;
end;

procedure TPSQLDump.SetDatabase(const Value : TPSQLDatabase);
begin
  if Value <> FDatabase then
    FDatabase := Value;
end;

procedure TPSQLDump.SetCompressLevel(const Value: TCompressLevel);
begin
  FCompressLevel := Value;
end;

procedure TPSQLDump.DumpToFile(const FileName: string; Log: TStrings);
var
  tmpLogFile: string;
begin
  tmpLogFile := GetTempFileName();
  if tmpLogFile = '' then
    raise EPSQLDumpException.Create('Can''t create temporary log file');
  try
    DumpToFile(FileName, tmpLogFile);
  finally
    if Assigned(Log) then
      Log.LoadFromFile(tmpLogFile);
    SysUtils.DeleteFile(tmpLogFile);
  end;
end;

procedure TPSQLDump.DumpToStream(Stream: TStream);
begin
  DumpToStream(Stream,nil);
end;

procedure TPSQLDump.CheckDependencies;
begin
  if not Assigned(FDatabase) then
    raise EDatabaseError.Create('Property Database not set!');
  if not FDatabase.Connected then
    FDatabase.Connected := True;

  if (DumpFormat <> dfDirectory) and (FJobs > 1)  then
    raise EPSQLRestoreException.Create('Multiple jobs can be used only with directory output format');
{$IFDEF MSWINDOWS}
  if FJobs > MAXIMUM_WAIT_OBJECTS then
   raise EPSQLRestoreException.CreateFmt('Maximum number of parallel jobs is %d',[MAXIMUM_WAIT_OBJECTS]);
{$ENDIF}

  if [doDataOnly, doSchemaOnly] * FDumpOptions = [doDataOnly, doSchemaOnly] then
	  raise EPSQLDumpException.Create('Options "Schema only" and "Data only" cannot be used together');
  if [doDataOnly, doClean] * FDumpOptions = [doDataOnly, doClean] then
    raise EPSQLDumpException.Create('Options "Clean" and "Data only" cannot be used together');
  if (doIncludeBLOBs in FDumpOptions) and (FDumpStrOPtions[dsoTable] > '') then
    raise EPSQLDumpException.Create('Large-object output not supported for a single table.'#13#10+
		                 'Use a full dump instead');
  if (doIncludeBLOBs in FDumpOptions) and (FDumpStrOPtions[dsoSchema] > '') then
    raise EPSQLDumpException.Create('Large-object output not supported for a single schema.'#13#10+
		                 'Use a full dump instead');
  if [doInserts, doOids] * FDumpOPtions = [doInserts, doOids] then
    raise EPSQLDumpException.Create('"Insert" and "OID" options cannot be used together.'#13#10+
                                 'The INSERT command cannot set OIDs');
  if (doIncludeBLOBs in FDumpOptions) and (FDumpFormat = dfPlain) then
    raise EPSQLDumpException.Create('Large-object output is not supported for plain-text dump files.'#13#10+
		                 'Use a different output format.');
end;

function TPSQLDump.GetParameters(OutputFileName: string): PAnsiDACChar;
var I: TDumpOption;
    J: TDumpStrOption;
    DRS: TDumpRestoreSection;
    k: integer;
begin
  if not Assigned(FDatabase) then
    raise EPSQLDumpException.Create('Database property not assigned!');

  FmiParams.Clear;

  FmiParams.Add('--file=' + OutputFileName);

  for I := Low(TDumpOption) to High(TDumpOption) do
    if I in FDumpOptions then
      FmiParams.Add(DumpCommandLineBoolParameters[I]);

  for J := Low(TDumpStrOption) to High(TDumpStrOption) do
    if FDumpStrOptions[J] > '' then
      FmiParams.Add(DumpCommandLineStrParameters[J] + FDumpStrOptions[J]);

  for DRS := Low(TDumpRestoreSection) to High(TDumpRestoreSection) do
    if DRS in FSections then
      FmiParams.Add(DumpRestoreCommandLineSectionParameters[DRS]);

  for k := 0 to FSchemaNames.Count-1 do
    FmiParams.Add(DumpCommandLineStrParameters[dsoSchema] + FSchemaNames[k]);

  for k := 0 to FExcludeSchemas.Count-1 do
    FmiParams.Add(DumpCommandLineStrParameters[dsoExcludeSchema] + FExcludeSchemas[k]);

  for k := 0 to FTableNames.Count-1 do
    FmiParams.Add(DumpCommandLineStrParameters[dsoTable] + FTableNames[k]);

  for k := 0 to FExcludeTables.Count-1 do
    FmiParams.Add(DumpCommandLineStrParameters[dsoExcludeTable] + FExcludeTables[k]);

  for k := 0 to FExcludeTablesData.Count-1 do
    FmiParams.Add(DumpCommandLineStrParameters[dsoExcludeTableData] + FExcludeTablesData[k]);

  FmiParams.Add(DumpCommandLineFormatValues[FDumpFormat]);

  if FDumpFormat in [dfCompressedArchive, dfPlain] then
    FmiParams.Add(Format('--compress=%d', [FCompressLevel]));

  if FLockWaitTimeout > 0 then
    FmiParams.Add(Format('--lock-wait-timeout=%u', [FLockWaitTimeout]));

  if FJobs > 1 then
    FmiParams.Add(Format('--jobs=%u', [FJobs]));

  FmiParams.Add('--no-password'); //we will put it using environment variable

  with FDatabase do
  begin
   FmiParams.Add(Format('--username=%s',[UserName]));
   FmiParams.Add(Format('--port=%d',[Port]));
   FmiParams.Add(Format('--host=%s',[Host]));
   FmiParams.Add(DatabaseName);
  end;

  Result := FmiParams.GetPCharArray();

  {$IFDEF M_DEBUG}
  FParamStr := FmiParams.FParams.Commatext;
  {$ENDIF}
end;

function TPSQLDump.GetStrOptions(const Index: Integer): string;
begin
  Result := FDumpStrOptions[TDumpStrOption(Index)];
end;

procedure TPSQLDump.SetStrOptions(const Index: Integer;
  const Value: string);
begin
  FDumpStrOptions[TDumpStrOption(Index)] := Value;
end;

procedure TPSQLDump.DumpToFile(const FileName, LogFileName: string);
begin
  CheckDependencies;

  if Assigned(FBeforeDump) then
    FBeforeDump(Self);

  Dump(FileName, LogFileName);

  If Assigned(FAfterDump) then
    FAfterDump(Self);
end;

procedure TPSQLDump.DumpToStream(Stream: TStream; Log: TStrings);
var
   tmpLogFile: string;
begin
  tmpLogFile := GetTempFileName();
  if Assigned(Log) then
    if tmpLogFile = '' then
      raise EPSQLDumpException.Create('Can''t create temporary log file');
  try
    DumpToStream(Stream,tmpLogFile);
  finally
    if Assigned(Log) then
     begin
      Log.LoadFromFile(tmpLogFile);
      SysUtils.DeleteFile(tmpLogFile);
     end;
  end;
end;

procedure TPSQLDump.DumpToStream(Stream: TStream; LogFileName: string);
var
   tmpTargetFile: string;
   FS: TFileStream;
begin
  tmpTargetFile := GetTempFileName();
  if tmpTargetFile = '' then
    raise EPSQLDumpException.Create('Can''t create temporary target file');
  try
    DumpToFile(tmpTargetFile,LogFileName);
    if Assigned(Stream) then
     begin
      FS := TFileStream.Create(tmpTargetFile,fmOpenRead);
      try
       Stream.CopyFrom(FS,FS.Size);
      finally
       FS.Free;
      end;
     end;
  finally
    SysUtils.DeleteFile(tmpTargetFile);
  end;
end;

procedure ErrorCallBackProc(Code: integer);cdecl;
begin
  {This callback ALWAYS must raise an exception!
  In any way dump or restore process will be aborted}
  raise EPSQLDumpException.Create(Format('Error with code: %d', [Code]));
end;

procedure LogCallBackProc(S: PAnsiDACChar);cdecl;
begin
  if Assigned(ProccessOwner) then
     if (ProccessOwner is TPSQLDump) then
       (ProccessOwner as TPSQLDump).DoLog(TrimRight(UTF8ToString(S)))
     else
       if (ProccessOwner is TPSQLRestore) then
        (ProccessOwner as TPSQLRestore).DoLog(TrimRight(UTF8ToString(S)));
end;

{$IFDEF UNDER_DELPHI_6}
function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}

{$IFDEF DELPHI_5}
type UTF8String = AnsiString;
{$ENDIF}

procedure UpdateEnv(PWD: {$IFNDEF NEXTGEN}UTF8String{$ELSE}String{$ENDIF});
{$IFDEF MSWINDOWS}
type
  tputenv = function(NameValue: PAnsiDACChar): integer; cdecl;
var
  putenv: tputenv;
{$ENDIF}
{$IFDEF NEXTGEN}
var
  M: TMarshaller;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  @putenv := GetProcAddress(GetModuleHandle('msvcrt'), '_putenv');
  if not Assigned(putenv) then
    raise EPSQLDumpException.Create('Cannot obtain _putenv procedure entry point');
  if putenv(PAnsiChar('PGPASSWORD=' + PWD)) <> 0 then
    raise EPSQLDumpException.Create('Cannot populate environment settings');
  if not SetEnvironmentVariableA('PGPASSWORD', PAnsiChar(PWD)) then
    {$IFDEF DELPHI_5}
    RaiseLastWin32Error();
    {$ELSE}
    RaiseLastOSError();
    {$ENDIF}
{$ELSE POSIX}
  SetEnv('PGPASSWORD', {$IFNDEF NEXTGEN}PAnsiChar(PWD){$ELSE}M.AsAnsi(PWD).ToPointer{$ENDIF}, 1);
{$ENDIF}
end;

procedure TPSQLDump.Dump(const TargetFile, LogFile: string);
var
  h : Cardinal;
  Result: longint;
  S: string;

  PLog: PAnsiDACChar;
  Params: PAnsiDACChar;

  v3_dump: Tv3_dump;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  pdmbvm_SetErrorCallBackProc : Tpdmbvm_SetErrorCallBackProc;
  pdmbvm_SetLogCallBackProc : Tpdmbvm_SetLogCallBackProc;

  LibName: string;
begin
  if (FDumpFormat = dfDirectory) then
    begin
      if DirectoryExists(TargetFile) then
       raise EPSQLDumpException.Create('Cannot create dump. Target directory exists: ' + TargetFile);
    end
  else
    if FileExists(TargetFile) and not FRewriteFile then
      raise EPSQLDumpException.Create('Cannot create dump. Target file exists: ' + TargetFile);

  LibName := 'pg_dump.dll';
  if Assigned(FOnLibraryLoad) then FOnLibraryLoad(Self, LibName);
  h := SafeLoadLibrary(PChar(LibName));
  {$IFDEF M_DEBUG}
   LogDebugMessage('DUMPLIB', GetModuleName(h));
  {$ENDIF}
  try
    @v3_dump := GetProcAddress(h, PChar('v3_dump'));
    if not assigned(@v3_dump) then
      raise EPSQLDumpException.Create('Can''t load pg_dump.dll');

    @pdmbvm_GetVersionAsInt := GetProcAddress(h, PChar('pdmbvm_GetVersionAsInt'));

    {$IFDEF M_DEBUG}
    LogDebugMessage('DUMPVER', IntToStr(pdmbvm_GetVersionAsInt()));
    {$ENDIF}

    @pdmbvm_SetErrorCallBackProc := GetProcAddress(h, PChar('pdmbvm_SetErrorCallBackProc'));
    @pdmbvm_SetLogCallBackProc := GetProcAddress(h, PChar('pdmbvm_SetLogCallBackProc'));

    pdmbvm_SetErrorCallBackProc(@ErrorCallBackProc);
    if Assigned(FOnLog) then
     begin
      ProccessOwner := Self;
      pdmbvm_SetLogCallBackProc(@LogCallBackProc);
     end;
    PLog := StringToAnsiChar(CP_ACP, LogFile);
    UpdateEnv({$IFDEF NEXTGEN}String{$ENDIF}(UTF8Encode(FDatabase.UserPassword)));
    Params := GetParameters(TargetFile);
    {$IFDEF M_DEBUG}
    LogDebugMessage('PARAMSTR', FParamStr);
    {$ENDIF}
    Result := v3_dump(PAnsiDACChar(UTF8Encode(ParamStr(0))), PLog, Params);

    case Result of
        0: S := '';
        1: S := 'Common dump error';
        2: S := 'Connection error (wrong username, password, host etc.)';
        3: S := 'File IO error';
     else
        S := 'Uknown dump error';
    end;
    if S > '' then
      raise EPSQLDumpException.Create(S + #13#10 + Format('Error Code: %d', [Result]));

  finally
   ProccessOwner := nil;
   ReallocMem(PLog, 0);
   FreeLibrary(h);
  end;
end;

procedure TPSQLDump.SetTableNames(const Value: TStrings);
begin
  FTableNames.Assign(Value);
end;

procedure TPSQLDump.SetSchemaNames(const Value: TStrings);
begin
  FSchemaNames.Assign(Value);
end;

procedure TPSQLDump.SetExcludeSchemas(const Value: TStrings);
begin
  FExcludeSchemas.Assign(Value);
end;

procedure TPSQLDump.SetExcludeTables(const Value: TStrings);
begin
  FExcludeTables.Assign(Value);
end;

procedure TPSQLDump.SetExcludeTablesData(const Value: TStrings);
begin
  FExcludeTablesData.Assign(Value);
end;

procedure TPSQLDump.DoLog(const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TPSQLDump.GetVersionAsInt: integer;
var
  h : Cardinal;
  LibName: string;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
begin
  Result := 0;
  LibName := 'pg_dump.dll';
  if Assigned(FOnLibraryLoad) then FOnLibraryLoad(Self, LibName);
  h := LoadLibrary(PChar(LibName));
  try
   @pdmbvm_GetVersionAsInt := GetProcAddress(h, PChar('pdmbvm_GetVersionAsInt'));
   if Assigned(pdmbvm_GetVersionAsInt) then
     Result := pdmbvm_GetVersionAsInt();
  finally
   FreeLibrary(H);
  end;
end;

function TPSQLDump.GetVersionAsStr: string;
begin
  Result := IntVerToStr(GetVersionAsInt());
end;

procedure TPSQLDump.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TableName', ReadTableName, nil, False);
  Filer.DefineProperty('SchemaName', ReadSchemaName, nil, False);
end;

procedure TPSQLDump.ReadSchemaName(Reader: TReader);
var S: string;
begin
  S := Reader.ReadString;
  if S > '' then  FSchemaNames.Append(S);
end;

procedure TPSQLDump.ReadTableName(Reader: TReader);
var S: string;
begin
  S := Reader.ReadString;
  if S > '' then  FTableNames.Append(S);
end;

procedure TPSQLDump.SetLockWaitTimeout(const Value: cardinal);
begin
  FLockWaitTimeout := Value;
end;

procedure TPSQLDump.SetDumpOptions(const Value: TDumpOptions);
begin
  FDumpOptions := Value;
  //  since we allow use of custom lib this is not correct anymore
  //  Exclude(FDumpOptions, doIgnoreVersion); //deprecated and to be removed
end;

{TPSQLRestore}

constructor TPSQLRestore.Create(Owner: TComponent);
var I: integer;
begin
  inherited;
  FRestoreOptions := [];
  FmiParams := TpdmvmParams.Create;
  FRestoreFormat := rfAuto;
  FTableNames := TStringList.Create;
  FSchemaNames := TStringList.Create;
  FExcludeSchemas := TStringList.Create;
  {$IFNDEF NEXTGEN}
  ZeroMemory(@FRestoreStrOptions, SizeOf(FRestoreStrOptions));
  {$ELSE}
  FillChar(FRestoreStrOptions, SizeOf(FRestoreStrOptions), 0);
  {$ENDIF}
  if (csDesigning in ComponentState) and Assigned(Owner) then
    for I := Owner.ComponentCount - 1 downto 0 do
      if Owner.Components[I] is TPSQLDatabase then
      begin
         Database := Owner.Components[I] as TPSQLDatabase;
         Break;
      end;
end;

procedure TPSQLRestore.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TableName', ReadTableName, nil, False);
  Filer.DefineProperty('SchemaName', ReadSchemaName, nil, False);
end;

destructor TPSQLRestore.Destroy;
begin
  FmiParams.Free;
  FTableNames.Free;
  FSchemaNames.Free;
  FExcludeSchemas.Free;
  inherited;
end;

procedure TPSQLRestore.RestoreFromFile(const FileName: string; Log: TStrings);
var tmpLogFile: string;
begin
  tmpLogFile := GetTempFileName();
  if tmpLogFile = '' then
    raise EPSQLRestoreException.Create('Can''t create temporary log file');
  try
    RestoreFromFile(FileName,tmpLogFile);
  finally
    If Assigned(Log) then
      Log.LoadFromFile(tmpLogFile);
    SysUtils.DeleteFile(tmpLogFile);
  end;
end;

procedure TPSQLRestore.RestoreFromFile(const FileName, LogFileName: string);
begin
  CheckDependencies;

  if Assigned(FBeforeRestore) then
    FBeforeRestore(Self);

  Restore(FileName, LogFileName);

  if Assigned(FAfterRestore) then
    FAfterRestore(Self);
end;

function TPSQLRestore.GetParameters(SourceFileName: string): PAnsiDACChar;
var I: TRestoreOption;
    J: TRestoreStrOption;
    K: Integer;
  DRS: TDumpRestoreSection;
begin
  if not Assigned(FDatabase) then
    raise EPSQLRestoreException.Create('Database property not assigned!');

  FmiParams.Clear;

  for I := Low(TRestoreOption) to Pred(High(TRestoreOption)) do
    if I in FRestoreOptions then
      FmiParams.Add(RestoreCommandLineBoolParameters[I]);

  for J := Low(TRestoreStrOption) to High(TRestoreStrOption) do
    if (FRestoreStrOptions[J] > '') then
      FmiParams.Add(RestoreCommandLineStrParameters[J] + FRestoreStrOPtions[J]);

  for DRS := Low(TDumpRestoreSection) to High(TDumpRestoreSection) do
    if DRS in FSections then
      FmiParams.Add(DumpRestoreCommandLineSectionParameters[DRS]);

  if FRestoreFormat <> rfAuto then
    FmiParams.Add(RestoreCommandLineFormatValues[FRestoreFormat]);

  for k := 0 to FSchemaNames.Count-1 do
    FmiParams.Add(RestoreCommandLineStrParameters[rsoSchemaName] + FSchemaNames[k]);

  for k := 0 to FExcludeSchemas.Count-1 do
    FmiParams.Add(RestoreCommandLineStrParameters[rsoExcludeSchema] + FExcludeSchemas[k]);

  for k := 0 to FTableNames.Count-1 do
   FmiParams.Add(RestoreCommandLineStrParameters[rsoTable] + FTableNames[k]);

  if FJobs > 1 then
    FmiParams.Add(Format('--jobs=%u', [FJobs]));

  FmiParams.Add('--no-password'); //we will put it using environment variable

  with FDatabase do
    begin
     FmiParams.Add(Format('--username=%s',[UserName]));
     FmiParams.Add(Format('--port=%d',[Port]));
     FmiParams.Add(Format('--host=%s',[Host]));
    end;

  FmiParams.Add(SourceFileName);

  Result := FmiParams.GetPCharArray();

{$IFDEF M_DEBUG}
  FParamStr := FmiParams.FParams.Commatext;
{$ENDIF}
end;

function TPSQLRestore.GetStrOptions(const AnIndex: Integer): string;
begin
  Result := FRestoreStrOptions[TRestoreStrOption(AnIndex)];
end;

procedure TPSQLRestore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
    FDatabase := nil;
end;

procedure TPSQLRestore.SetDatabase(const Value : TPSQLDatabase);
begin
   if Value <> FDatabase then
      FDatabase := Value;
end;

procedure TPSQLRestore.SetExcludeSchemas(const Value: TStrings);
begin
  FExcludeSchemas.Assign(Value);
end;

procedure TPSQLRestore.SetSchemaNames(const Value: TStrings);
begin
  FSchemaNames.Assign(Value);
end;

procedure TPSQLRestore.SetStrOptions(const AnIndex: Integer;
  const Value: string);
begin
 FRestoreStrOptions[TRestoreStrOption(AnIndex)] := Value;
end;

procedure TPSQLRestore.SetTableNames(const Value: TStrings);
begin
  FTableNames.Assign(Value);
end;

procedure TPSQLRestore.CheckDependencies;
var CheckDB: TPSQLDatabase;
begin
 if not ((FRestoreStrOptions[rsoFileName] > '') xor (FRestoreStrOptions[rsoDBName] > ''))  then
   raise EPSQLRestoreException.Create('Database or file output should be specified, but not both');


 if (roSingleTransaction in FRestoreOptions) and (FJobs > 1)  then
   raise EPSQLRestoreException.Create('Multiple jobs cannot be used within the single transaction');

 {$IFDEF MSWINDOWS}
 if FJobs > MAXIMUM_WAIT_OBJECTS then
   raise EPSQLRestoreException.CreateFmt('Maximum number of parallel jobs is %d',[MAXIMUM_WAIT_OBJECTS]);
 {$ENDIF}

 if (FRestoreStrOptions[rsoFileName] = '')
    and (FRestoreStrOptions[rsoDBName] = '')
  then
   Raise EPSQLRestoreException.Create('At least database or file output must be specified');

 if FRestoreStrOptions[rsoDBName] > '' then
   begin
    CheckDB := TPSQLDatabase.Create(nil);
    try
      CheckDB.DatabaseName := FRestoreStrOptions[rsoDBName];
      CheckDB.Host := FDatabase.Host;
      CheckDB.Port := FDatabase.Port;
      CheckDB.UserName := FDatabase.UserName;
      CheckDB.UserPassword := FDatabase.UserPassword;
      CheckDB.Connected := True;
    finally
      CheckDB.Connected := False;
      CheckDB.Free;
    end;
   end;
end;

procedure TPSQLRestore.ReadSchemaName(Reader: TReader);
var S: string;
begin
  S := Reader.ReadString;
  if S > '' then FSchemaNames.Append(S);
end;

procedure TPSQLRestore.ReadTableName(Reader: TReader);
var S: string;
begin
  S := Reader.ReadString;
  if S > '' then FTableNames.Append(S);
end;

procedure TPSQLRestore.Restore(const SourceFile, LogFile: string);
var
  h : Cardinal;
  Result: longint;
  S: string;

  v3_restore: Tv3_Restore;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  pdmbvm_SetErrorCallBackProc : Tpdmbvm_SetErrorCallBackProc;
  pdmbvm_SetLogCallBackProc : Tpdmbvm_SetLogCallBackProc;

  PLog: PAnsiDACChar;
  LibName: string;
  Params: PAnsiDACChar;
  {$IFDEF NEXTGEN}
  M: TMarshaller;
  {$ENDIF}
begin
  S := '';
  LibName := 'pg_restore.dll';
  if Assigned(FOnLibraryLoad) then FOnLibraryLoad(Self, LibName);
  h := SafeLoadLibrary(PChar(LibName));
  {$IFDEF M_DEBUG}
  LogDebugMessage('RESTLIB', GetModuleName(h));
  {$ENDIF}
  try
    @v3_restore := GetProcAddress(h, PChar('v3_restore'));
    if not assigned(@v3_restore) then
      raise EPSQLRestoreException.Create('Can''t load pg_restore.dll');

    @pdmbvm_GetVersionAsInt := GetProcAddress(h, PChar('pdmbvm_GetVersionAsInt'));

    {$IFDEF M_DEBUG}
    LogDebugMessage('RESTVER', IntToStr(pdmbvm_GetVersionAsInt()));
    {$ENDIF}

    @pdmbvm_SetErrorCallBackProc := GetProcAddress(h, PChar('pdmbvm_SetErrorCallBackProc'));
    @pdmbvm_SetLogCallBackProc := GetProcAddress(h, PChar('pdmbvm_SetLogCallBackProc'));

    pdmbvm_SetErrorCallBackProc(@ErrorCallBackProc);
    if Assigned(FOnLog) then
     begin
      ProccessOwner := Self;
      pdmbvm_SetLogCallBackProc(@LogCallBackProc);
     end;
    if LogFile > '' then
      {$IFNDEF NEXTGEN}
      PLog := PAnsiDACChar(UTF8Encode(LogFile))
      {$ELSE}
      PLog := PAnsiDACChar(M.AsAnsi(LogFile))
      {$ENDIF}
    else
      PLog := nil;

    UpdateEnv({$IFDEF NEXTGEN}string{$ENDIF}(UTF8Encode(FDatabase.UserPassword)));
    Params := GetParameters(SourceFile);

  {$IFDEF M_DEBUG}
    LogDebugMessage('PARAMSTR', FParamStr);
  {$ENDIF}

    Result := v3_restore(PAnsiDACChar(UTF8Encode(ParamStr(0))), PLog, Params);

    case Result of
      0: ;// - OK
      1: if roExitOnError in Options then S := 'Common pg_restore error';
      3: S := 'Output file error.'; //stdout operation
      4: S := 'Error output file error.'; //stderr
    else
      S := 'Unknown restore error';
    end;

    if S > '' then
      raise EPSQLRestoreException.Create(S + #13#10 + Format('Error Code: %d', [Result]));

  finally
    FreeLibrary(h);
  end;
end;

procedure TPSQLRestore.DoLog(const Value: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TPSQLRestore.GetVersionAsInt: integer;
var
  h : Cardinal;
  LibName: string;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
begin
  Result := 0;
  LibName := 'pg_restore.dll';
  if Assigned(FOnLibraryLoad) then FOnLibraryLoad(Self, LibName);
  h := LoadLibrary(PChar(LibName));
  try
   @pdmbvm_GetVersionAsInt := GetProcAddress(h, PChar('pdmbvm_GetVersionAsInt'));
   if Assigned(pdmbvm_GetVersionAsInt) then
     Result := pdmbvm_GetVersionAsInt();
  finally
   FreeLibrary(H);
  end;
end;
function TPSQLRestore.GetVersionAsStr: string;
begin
  Result := IntVerToStr(GetVersionAsInt());
end;

procedure TPSQLRestore.SetJobs(const Value: cardinal);
begin
  FJobs := Value;
end;

procedure TPSQLRestore.SetRestoreOptions(const Value: TRestoreOptions);
begin
  FRestoreOptions := Value;
  //since we allow use of custom lib this is not correct anymore
  //Exclude(FRestoreOptions, roIgnoreVersion);
end;

end.
