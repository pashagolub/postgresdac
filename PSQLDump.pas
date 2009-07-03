{$I PSQLdac.inc}
unit PSQLDump;

{SVN revision: $Id$}

interface

Uses Classes, SysUtils, Windows, BDE, Db,DbTables,PSQLTypes,Math,
     {$IFDEF DELPHI_6}Variants,{$ENDIF}
     PSQLDbTables, PSQLAboutFrm;

type
  Tpdmvm_dump = function ( app_exe : PAnsiChar; database : PAnsiChar; pwd : PAnsiChar; err_str : PAnsiChar; out_file : PWideChar; err_file : PWideChar; params : PAnsiChar):longint; cdecl;
  Tpdmvm_restore = function ( app_exe : PAnsiChar; filename : PWideChar; pwd : PAnsiChar; out_file : PWideChar; err_file : PWideChar; params : PAnsiChar):longint; cdecl;

  Tpdmbvm_GetLastError = procedure(out_buffer : PAnsiChar);cdecl;//mi:2006-10-12
  Tpdmbvm_GetVersionAsInt = function ():integer;cdecl;//mi:2007-01-15
  Tpdmbvm_SetErrorCallBackProc = procedure(ProcAddr : pointer);cdecl;//mi:2007-01-15
  Tpdmbvm_SetLogCallBackProc = procedure(ProcAddr : pointer);cdecl;//pg:2007-03-13

  TpdmvmParams = class
  private
    FParams : TStringList;
    FArr : PAnsiChar;

    procedure ClearMem();
  public
    constructor Create();
    destructor Destroy();override;

    procedure Add(aStr : string);
    procedure Clear();

    function GetPCharArray() : PAnsiChar;
  end;


    TLogEvent = procedure (Sender: TObject; const LogMessage: string) of object;

    EPSQLDumpException = class(Exception);

    TPSQLDump = class;

    TDumpOption = (doDataOnly, doIncludeBLOBs, doClean, doCreate, doInserts,
                  doColumnInserts, doIgnoreVersion, doOIDs, doNoOwner,
                  doSchemaOnly, doVerbose, doNoPrivileges, doDisableDollarQuoting,
                  doDisableTriggers, doUseSetSessionAuthorization, doNoTablespaces);

    TDumpOptions = set of TDumpOption;

    TDumpStrOption = (dsoSchema, dsoSuperuser, dsoTable, dsoExcludeSchema,
                      dsoExcludeTable, dsoEncoding, dsoRole);

    TDumpFormat = (dfPlain, dfTarArchive, dfCompressedArchive);

    TCompressLevel = 0..9;


    TPSQLDump = class(TComponent)
     private
        FAbout      : TPSQLDACAbout;
        FDatabase   : TPSQLDatabase;
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
      protected
        procedure CheckDependencies;
        procedure DefineProperties(Filer: TFiler); override;
        function GetParameters: PAnsiChar;
        Procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
      public
        Constructor Create(Owner : TComponent); override;
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
        property LockWaitTimeout: cardinal read FLockWaitTimeout write SetLockWaitTimeout default 0;
        property Options : TDumpOptions read FDumpOptions write SetDumpOptions default [];
        property RewriteFile: boolean read FRewriteFile write FRewriteFile default True;
        property Role: string index dsoRole read GetStrOptions write SetStrOptions;
        property SchemaNames: TStrings read FSchemaNames write SetSchemaNames;
        property SuperUserName: string  index dsoSuperUser read GetStrOptions write SetStrOptions;
        property TableNames: TStrings read FTableNames write SetTableNames;
        property AfterDump : TNotifyEvent read FAfterDump write FAfterDump;
        property BeforeDump  : TNotifyEvent read FBeforeDump write FBeforeDump;
        property OnLog: TLogEvent read FOnLog write FOnLog;
      end;


{TPSQLRestore stuff}
    TPSQLRestore = class;

    EPSQLRestoreException = class(Exception);

    TRestoreFormat = (rfAuto, rfTarArchive, rfCompressedArchive);

    TRestoreOption = (roDataOnly, roClean, roCreate, roExitOnError,
              roIgnoreVersion, roList, roNoOwner,
              roSchemaOnly, roVerbose, roNoPrivileges,
              roDisableTriggers, roUseSetSessionAuthorization,
              roSingleTransaction, roNoDataForFailedTables,
              roNoTablespaces);

    TRestoreOptions = set of TRestoreOption;

    TRestoreStrOption = (rsoTable, rsoSuperUser, rsoDBName,
                    rsoFileName, rsoIndex, rsoListFile, rsoFunction,
                    rsoTrigger, rsoRole);

    TPSQLRestore = class(TComponent)
     private
        FAbout      : TPSQLDACAbout;
        FDatabase   : TPSQLDatabase;
        FRestoreFormat : TRestoreFormat;
        FRestoreOptions : TRestoreOptions;
        FRestoreStrOptions : array[TRestoreStrOption] of string;
        FBeforeRestore : TNotifyEvent;
        FAfterRestore  : TNotifyEvent;
        FmiParams: TpdmvmParams;
        FOnLog: TLogEvent;
        FJobs: cardinal;
        procedure SetDatabase(const Value : TPSQLDatabase);
        function GetStrOptions(const Index: Integer): string;
        procedure SetStrOptions(const Index: Integer; const Value: string);
        procedure Restore(const SourceFile, OutFile, LogFile: string);
        procedure DoLog(const Value: string);
        function GetVersionAsInt: integer;
        procedure SetJobs(const Value: cardinal);
        procedure SetRestoreOptions(const Value: TRestoreOptions);
      protected
        procedure CheckDependencies;
        function GetParameters: PAnsiChar;
        Procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
      public
        Constructor Create(Owner : TComponent); override;
        destructor Destroy; override;
        procedure RestoreFromFile(const FileName: string; Log: TStrings); overload;
      	procedure RestoreFromFile(const FileName, LogFileName: string); overload;
        property VersionAsInt: integer read GetVersionAsInt;
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
        property TableName: string  index rsoTable read GetStrOptions write SetStrOptions;
        property Trigger: string  index rsoTrigger read GetStrOptions write SetStrOptions;
        property AfterRestore : TNotifyEvent read FAfterRestore write FAfterRestore;
        property BeforeRestore  : TNotifyEvent read FBeforeRestore write FBeforeRestore;
        property OnLog: TLogEvent read FOnLog write FOnLog;
      end;

const
    DumpCommandLineBoolParameters: array[TDumpOption] of string = (
     '--data-only',                      //doDataOnly
     '--blobs',                          //doIncludeBLOBs
     '--clean',                          //doClean
     '--create',                         //doCreate
     '--inserts',                        //doInserts
     '--column-inserts',                 //doColumnInserts
     '--ignore-version',                 //doIgnoreVersion
     '--oids',                           //doOIDs
     '--no-owner',                       //doNoOwner
     '--schema-only',                    //doSchemaOnly
     '--verbose',                        //doVerbose,
     '--no-privileges',                  //doNoPrivileges,
     '--disable-dollar-quoting',         //doDisableDollarQuoting,
     '--disable-triggers',               //doDisableTriggers,
     '--use-set-session-authorization',  //doUseSetSessionAuthorization,
     '--no-tablespaces'                  //doNoTablespaces
    );

    RestoreCommandLineBoolParameters: array[TRestoreOption] of string = (
     '--data-only',                //roDataOnly
     '--clean',                    //roClean
     '--create',                   //roCreate
     '--exit-on-error',            //roExitOnError
     '--ignore-version',           //roIgnoreVersion
     '--list',                     //roList
     '--no-owner',                 //roNoOwner
     '--schema-only',              //roSchemaOnly
     '--verbose',                  //roVerbose,
     '--no-privileges',            //roNoPrivileges,
     '--disable-triggers',         //roDisableTriggers,
     '--use-set-session-authorization',  //roUseSetSessionAuthorization
     '--single-transaction',             //roSingleTransaction
     '--no-data-for-failed-tables',      //roNoDataForFailedTables
     '--no-tablespaces'
     );

    DumpCommandLineStrParameters: array[TDumpStrOption] of string =(
     '--schema=',         //dsoSchema
     '--superuser=',      //dsoSuperuser
     '--table=',          //dsoTable
     '--exclude-schema',  //dsoExcludeSchema
     '--exclude-table=',  //dsoExcludeTable
     '--encoding=',       //dsoEncoding
     '--role='            //dsoRole
    );

    RestoreCommandLineStrParameters: array[TRestoreStrOption] of string =(
     '--table=',      //rsoTable
     '--superuser=',  //rsoSuperuser
     '--dbname=',     //rsoDBName,
     '--file=',       //rsoFileName,
     '--index=',      //rsoIndex,
     '--use-list=',   //rsoListFile,
     '--function=',   //rsoFunction,
     '--trigger=',    //rsoTrigger
     '--role='        //dsoRole
    );


    DumpCommandLineFormatValues: array[TDumpFormat] of string = (
    '--format=p',  //plain
    '--format=t',  //tar archive
    '--format=c'   //custom archive
    );

    RestoreCommandLineFormatValues: array[TRestoreFormat] of string = (
    '',             //auto
    '--format=t',  //tar archive
    '--format=c'   //custom archive
    );

implementation

uses PSQLAccess;

var ProccessOwner: TComponent = nil;

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
  s : PAnsiChar;
  p : PInteger; //32-bit pointer
begin
  if FArr <> nil then
  begin
    p := Pointer(FArr);

    repeat
      s := PAnsiChar(p^);
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

function TpdmvmParams.GetPcharArray: PAnsiChar;
var
  s, s1 : PAnsiChar;
  p : PInteger; //32-bit pointer
  i : integer;
begin
  ClearMem();

  GetMem(FArr, SizeOf(PAnsiChar) * (FParams.Count + 1));
  p := Pointer(FArr);
  for i:=0 to FParams.Count - 1 do
  begin
    s1 := PAnsiChar(UTF8Encode(FParams[i]));
    GetMem(s, Length(s1) + 1);
    StrCopy(s, s1);
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
  ZeroMemory(@FDumpStrOptions,sizeof(FDumpStrOptions));
  FRewriteFile := True;
  FTableNames := TStringList.Create;
  FExcludeTables := TStringList.Create;
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
   tmpPath, tmpLogFile: array[0..MAX_PATH] of char;
begin
  If (GetTempPath(MAX_PATH, tmpPath) = 0) or
     (GetTempFileName(tmpPath,'pgd',0,tmpLogFile) = 0) then
    raise EPSQLDumpException.Create('Can''t create temporary log file');
  try
    DumpToFile(FileName,tmpLogFile);
  finally
    If Assigned(Log) then
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

   if [doDataOnly, doSchemaOnly] * FDumpOptions = [doDataOnly, doSchemaOnly]
    then
	      Raise EPSQLDumpException.Create('Options "Schema only" and "Data only"'+
                                 ' cannot be used together');
   if [doDataOnly, doClean] * FDumpOptions = [doDataOnly, doClean]
    then
        Raise EPSQLDumpException.Create('Options "Clean" and "Data only"'+
                                 ' cannot be used together');
   if (doIncludeBLOBs in FDumpOptions)
        and (FDumpStrOPtions[dsoTable] > '')
    then
        Raise EPSQLDumpException.Create('Large-object output not supported for a single table.'#13#10+
		                 'Use a full dump instead');
   if (doIncludeBLOBs in FDumpOptions)
        and (FDumpStrOPtions[dsoSchema] > '')
    then
        Raise EPSQLDumpException.Create('Large-object output not supported for a single schema.'#13#10+
		                 'Use a full dump instead');
   if [doInserts, doOids] * FDumpOPtions = [doInserts, doOids]
    then
        Raise EPSQLDumpException.Create('"Insert" and "OID" options cannot be used together.'#13#10+
                                 'The INSERT command cannot set OIDs');
   if (doIncludeBLOBs in FDumpOptions)
      and (FDumpFormat = dfPlain)
    then
        Raise EPSQLDumpException.Create('Large-object output is not supported for plain-text dump files.'#13#10+
		                 'Use a different output format.');
end;

function TPSQLDump.GetParameters: PAnsiChar;
var I: TDumpOption;
    J: TDumpStrOption;
    k: integer;
begin
 If not Assigned(FDatabase) then
   raise EPSQLDumpException.Create('Database property not assigned!');

 FmiParams.Clear;

 for I := Low(TDumpOption) to High(TDumpOption) do
   if I in FDumpOptions then
     FmiParams.Add(DumpCommandLineBoolParameters[I]);

 for J := Low(TDumpStrOption) to High(TDumpStrOption) do
   if  FDumpStrOptions[J] > '' then
     FmiParams.Add(DumpCommandLineStrParameters[J] + FDumpStrOptions[J]);

 for k := 0 to FSchemaNames.Count-1 do
   FmiParams.Add(DumpCommandLineStrParameters[dsoSchema] + FSchemaNames[k]);

 for k := 0 to FExcludeSchemas.Count-1 do
   FmiParams.Add(DumpCommandLineStrParameters[dsoExcludeSchema] + FExcludeSchemas[k]);

 for k := 0 to FTableNames.Count-1 do
   FmiParams.Add(DumpCommandLineStrParameters[dsoTable] + FTableNames[k]);

 for k := 0 to FExcludeTables.Count-1 do
   FmiParams.Add(DumpCommandLineStrParameters[dsoExcludeTable] + FExcludeTables[k]);

 FmiParams.Add(DumpCommandLineFormatValues[FDumpFormat]);

 if FDumpFormat in [dfCompressedArchive, dfPlain] then
   FmiParams.Add(Format('--compress=%d', [FCompressLevel]));

 if FLockWaitTimeout > 0 then
   FmiParams.Add(Format('--lock-wait-timeout=%u', [FLockWaitTimeout]));

 with FDatabase do
  begin
   FmiParams.Add(Format('--username=%s',[UserName]));
   FmiParams.Add(Format('--port=%d',[Port]));
   FmiParams.Add(Format('--host=%s',[Host]));
  end;
 Result := FmiParams.GetPCharArray();
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

  If Assigned(FBeforeDump) then
    FBeforeDump(Self);

  Dump(FileName, LogFileName);

  If Assigned(FAfterDump) then
    FAfterDump(Self);
end;

procedure TPSQLDump.DumpToStream(Stream: TStream; Log: TStrings);
var
   tmpPath, tmpLogFile: array[0..MAX_PATH] of char;
begin
  tmpLogFile := '';
  if Assigned(Log) then
    if (GetTempPath(MAX_PATH, tmpPath) = 0) or
       (GetTempFileName(tmpPath,'pgd',0,tmpLogFile) = 0) then
      raise EPSQLDumpException.Create('Can''t create temporary log file');
  try
    DumpToStream(Stream,tmpLogFile);
  finally
    If Assigned(Log) then
     begin
      Log.LoadFromFile(tmpLogFile);
      SysUtils.DeleteFile(tmpLogFile);
     end;                                     
  end;
end;

procedure TPSQLDump.DumpToStream(Stream: TStream; LogFileName: string);
var
   tmpPath, tmpTargetFile: array[0..MAX_PATH] of char;
   FS: TFileStream;
begin
  if (GetTempPath(MAX_PATH, tmpPath) = 0) or
     (GetTempFileName(tmpPath,'pgd',0,tmpTargetFile) = 0) then
    raise EPSQLDumpException.Create('Can''t create temporary target file');
  try
    DumpToFile(tmpTargetFile,LogFileName);
    If Assigned(Stream) then
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

procedure ErrorCallBackProc(ModuleName: PAnsiChar; S: PAnsiChar);cdecl;
begin
  {This callback ALWAYS must raise an exception!
  In any way dump or restore process will be aborted}
  raise EPSQLDumpException.Create(Format('Error in module %s: %s', [UTF8ToString(ModuleName), UTF8ToString(S)]));
end;

procedure LogCallBackProc(S: PAnsiChar);cdecl;
begin
  If Assigned(ProccessOwner) then
     if (ProccessOwner is TPSQLDump) then
       (ProccessOwner as TPSQLDump).DoLog(TrimRight(UTF8ToString(S)))
     else
       if (ProccessOwner is TPSQLRestore) then
        (ProccessOwner as TPSQLRestore).DoLog(TrimRight(UTF8ToString(S)));
end;

procedure TPSQLDump.Dump(const TargetFile, LogFile: string);
var
  h : Cardinal;
  ErrBuff : array[0..1023] of AnsiChar;//error buffer
  Result: longint;
  S: string;

  PLog: PWideChar;
  PWD: PAnsiChar;

  pdmvm_dump: Tpdmvm_dump;
  pdmvm_GetLastError: Tpdmbvm_GetLastError;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  pdmbvm_SetErrorCallBackProc : Tpdmbvm_SetErrorCallBackProc;
  pdmbvm_SetLogCallBackProc : Tpdmbvm_SetLogCallBackProc;

begin
  if FileExists(TargetFile) and not FRewriteFile then
    raise EPSQLDumpException.Create('Cannot rewrite existing file '+ TargetFile);

  h := LoadLibrary('pg_dump.dll');
  {$IFDEF M_DEBUG}
   LogDebugMessage('DUMPLIB', GetModuleName(h));
  {$ENDIF}
  try
    @pdmvm_dump := GetProcAddress(h, PAnsiChar('pdmvm_dump'));
    if not assigned(@pdmvm_dump) then
      raise EPSQLDumpException.Create('Can''t load pg_dump.dll');

    @pdmvm_GetLastError := GetProcAddress(h, PAnsiChar('pdmbvm_GetLastError'));
    @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));//mi:2007-01-15

    {$IFDEF M_DEBUG}
    LogDebugMessage('DUMPVER', IntToStr(pdmbvm_GetVersionAsInt()));
    {$ENDIF}

    @pdmbvm_SetErrorCallBackProc := GetProcAddress(h, PAnsiChar('pdmbvm_SetErrorCallBackProc'));//mi:2007-01-15
    @pdmbvm_SetLogCallBackProc := GetProcAddress(h, PAnsiChar('pdmbvm_SetLogCallBackProc'));//pg:2007-03-13




    pdmbvm_SetErrorCallBackProc(@ErrorCallBackProc);//mi:2007-01-15
    if Assigned(FOnLog) then
     begin
      ProccessOwner := Self;
      pdmbvm_SetLogCallBackProc(@LogCallBackProc);//pg:2007-03-13
     end;

    if LogFile > '' then
     PLog := PWideChar(WideString(LogFile))
    else
     PLog := nil;
    PWD := PAnsiChar(UTF8Encode(FDatabase.UserPassword));
    Result := pdmvm_dump(PAnsiChar(UTF8Encode(ParamStr(0))),
                         PAnsiChar(UTF8Encode(FDatabase.DatabaseName)),
                         PWD,
                         ErrBuff,
                         PWideChar(WideString(TargetFile)),
                         PLog,
                         GetParameters());
    case Result of
        0: S := '';
        1: S := 'Common dump error';
        2: S := 'Connection error (wrong username, password, host etc.)';
        3: S := 'File IO error';
     else
        S := 'Uknown dump error';
    end;
    if S > '' then
      raise EPSQLDumpException.Create(S + #13#10 + Utf8ToString(ErrBuff));

  finally
   ProccessOwner := nil;
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

procedure TPSQLDump.DoLog(const Value: string);
begin
  If Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TPSQLDump.GetVersionAsInt: integer;
var
  h : Cardinal;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
begin
  Result := 0;
  h := LoadLibrary('pg_dump.dll');
  try
   @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));
   if Assigned(pdmbvm_GetVersionAsInt) then
     Result := pdmbvm_GetVersionAsInt();
  finally
   FreeLibrary(H);
  end;
end;

function TPSQLDump.GetVersionAsStr: string;
var VerInt, Major, Minor, Revision: integer;
begin
  VerInt := GetVersionAsInt;
  Major := VerInt div 10000;
  Minor := VerInt mod 10000 div 100;
  Revision := VerInt mod 100;
  Result := Format('%d.%d.%d', [Major, Minor, Revision]);
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
  Exclude(FDumpOptions, doIgnoreVersion); //deprecated and to be removed
end;

{TPSQLRestore}

constructor TPSQLRestore.Create(Owner: TComponent);
var I: integer;
begin
  inherited;
  FRestoreOptions := [];
  FmiParams := TpdmvmParams.Create;
  FRestoreFormat := rfAuto;
  ZeroMemory(@FRestoreStrOptions,sizeof(FRestoreStrOptions));
  if (csDesigning in ComponentState) and Assigned(Owner) then
    for I := Owner.ComponentCount - 1 downto 0 do
      if Owner.Components[I] is TPSQLDatabase then
      begin
         Database := Owner.Components[I] as TPSQLDatabase;
         Break;
      end;
end;

destructor TPSQLRestore.Destroy;
begin
  FmiParams.Free;
  inherited;
end;

procedure TPSQLRestore.RestoreFromFile(const FileName: string; Log: TStrings);
var tmpLogFile: array[0..MAX_PATH] of char;
    tmpPath: array[0..MAX_PATH] of char;
begin
  If (GetTempPath(MAX_PATH, tmpPath) = 0) or
     (GetTempFileName(tmpPath,'pgr',0,tmpLogFile) = 0) then
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
var tmpOutFile: array[0..MAX_PATH] of char;
    tmpPath: array[0..MAX_PATH] of char;
begin
  CheckDependencies;

  If Assigned(FBeforeRestore) then
    FBeforeRestore(Self);

  If FRestoreStrOptions[rsoDBName] = '' then
   Restore(FileName,FRestoreStrOptions[rsoFileName],LogFileName)
  else
     begin

       If (GetTempPath(MAX_PATH, tmpPath) = 0) or
          (GetTempFileName(tmpPath,'pgr',0,tmpOutFile) = 0) then
         raise EPSQLRestoreException.Create('Can''t create temporary out file');
       try
         Restore(FileName,tmpOutFile,logFileName);
       finally
         SysUtils.DeleteFile(tmpOutFile);
       end;
     end;
  If Assigned(FAfterRestore) then
    FAfterRestore(Self);
end;

function TPSQLRestore.GetParameters: PAnsiChar;
var I: TRestoreOption;
    J: TRestoreStrOption;
begin
   If not Assigned(FDatabase) then
     raise EPSQLRestoreException.Create('Database property not assigned!');

   FmiParams.Clear;

   if roNoDataForFailedTables in FRestoreOptions then
    begin
     FmiParams.Add('-X');
     FmiParams.Add('no-data-for-failed-tables');
    end;

   for I := Low(TRestoreOption) to Pred(High(TRestoreOption)) do
     if I in FRestoreOptions then
       FmiParams.Add(RestoreCommandLineBoolParameters[I]);
   for J := Low(TRestoreStrOption) to High(TRestoreStrOption) do
     if (J <> rsoFileName) and (FRestoreStrOptions[J] > '') then
       FmiParams.Add(RestoreCommandLineStrParameters[J] + FRestoreStrOPtions[J]);

   if FRestoreFormat <> rfAuto then
     FmiParams.Add(RestoreCommandLineFormatValues[FRestoreFormat]);

   if FJobs > 1 then
     FmiParams.Add(Format('--jobs=%u', [FJobs]));

   With FDatabase do
    begin
     FmiParams.Add(Format('--username=%s',[UserName]));
     FmiParams.Add(Format('--port=%d',[Port]));
     FmiParams.Add(Format('--host=%s',[Host]));
    end;
   Result := FmiParams.GetPCharArray();
end;

function TPSQLRestore.GetStrOptions(const Index: Integer): string;
begin
  Result := FRestoreStrOptions[TRestoreStrOption(Index)];
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

procedure TPSQLRestore.SetStrOptions(const Index: Integer;
  const Value: string);
begin
 FRestoreStrOptions[TRestoreStrOption(Index)] := Value;
end;

procedure TPSQLRestore.CheckDependencies;
var CheckDB: TPSQLDatabase;
begin
 if (FRestoreStrOptions[rsoFileName] > '')
    and (FRestoreStrOptions[rsoDBName] > '')
  then
   Raise EPSQLRestoreException.Create('Cannot specify both database and file output');


 if (roSingleTransaction in FRestoreOptions) and (FJobs > 1)
  then
   Raise EPSQLRestoreException.Create('Multiple jobs cannot be used within the single transaction');


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
     CheckDB.Connected := False;
    finally
     CheckDB.Free;
    end;
   end;
end;

procedure TPSQLRestore.Restore(const SourceFile, OutFile, LogFile: string);
var
  h : Cardinal;
  Result: longint;
  S: string;
  pdmvm_restore: Tpdmvm_restore;
  pdmvm_GetLastError: Tpdmbvm_GetLastError;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  pdmbvm_SetErrorCallBackProc : Tpdmbvm_SetErrorCallBackProc;
  pdmbvm_SetLogCallBackProc : Tpdmbvm_SetLogCallBackProc;

  PLog: PWideChar;
begin
  S := '';
  h := LoadLibrary('pg_restore.dll');
  {$IFDEF M_DEBUG}
  LogDebugMessage('RESTLIB', GetModuleName(h));
  {$ENDIF}
  try
    @pdmvm_restore := GetProcAddress(h, PAnsiChar('pdmvm_restore'));
    if not assigned(@pdmvm_restore) then
     raise EPSQLRestoreException.Create('Can''t load pg_restore.dll');

    @pdmvm_GetLastError := GetProcAddress(h, PAnsiChar('pdmbvm_GetLastError'));
    @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));//mi:2007-01-15

    {$IFDEF M_DEBUG}
    LogDebugMessage('RESTVER', IntToStr(pdmbvm_GetVersionAsInt()));
    {$ENDIF}

    @pdmbvm_SetErrorCallBackProc := GetProcAddress(h, PAnsiChar('pdmbvm_SetErrorCallBackProc'));//mi:2007-01-15
    @pdmbvm_SetLogCallBackProc := GetProcAddress(h, PAnsiChar('pdmbvm_SetLogCallBackProc'));//pg:2007-03-13

    pdmbvm_SetErrorCallBackProc(@ErrorCallBackProc);//mi:2007-01-15
    If Assigned(FOnLog) then
     begin
      ProccessOwner := Self;
      pdmbvm_SetLogCallBackProc(@LogCallBackProc);//pg:2007-03-13
     end;

    if LogFile > '' then
     PLog := PWideChar(WideString(LogFile))
    else
     PLog := nil;
    Result := pdmvm_restore(PAnsiChar(UTF8Encode(ParamStr(0))),
                          PWideChar(WideString(SourceFile)),//in file
                          PAnsiChar(UTF8Encode(FDatabase.UserPassword)),
                          PWideChar(WideString(OutFile)),//out file
                          PLog,//out file
                          GetParameters());

    case Result of
      0: ;// - OK
      1: if roExitOnError in Options then S := 'Common pg_restore error';
      3: S := 'File IO error'; //any file operation
      1000: S := Format('Could not open input file %s',[SourceFile]);
      1001: S := Format('Could not read input file %s',[SourceFile]);
      1002: S := Format('Input file %s is too short',[SourceFile]);
      1003: S := Format('Input file %s does not appear to be a valid archive (too short?)',[SourceFile]);
      1004: S := Format('Input file %s does not appear to be a valid archive',[SourceFile]);
    else
      S := 'Unknown restore error';
    end;

    if S > '' then
      raise EPSQLRestoreException.Create(S);

  finally
    FreeLibrary(h);
  end;
end;

procedure TPSQLRestore.DoLog(const Value: string);
begin
  If Assigned(FOnLog) then
    FOnLog(Self, Value);
end;

function TPSQLRestore.GetVersionAsInt: integer;
var
  h : Cardinal;
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
begin
  Result := 0;
  h := LoadLibrary('pg_restore.dll');
  try
   @pdmbvm_GetVersionAsInt := GetProcAddress(h, PAnsiChar('pdmbvm_GetVersionAsInt'));
   if Assigned(pdmbvm_GetVersionAsInt) then
     Result := pdmbvm_GetVersionAsInt();
  finally
   FreeLibrary(H);
  end;
end;
procedure TPSQLRestore.SetJobs(const Value: cardinal);
begin
  FJobs := Value;
end;

procedure TPSQLRestore.SetRestoreOptions(const Value: TRestoreOptions);
begin
  FRestoreOptions := Value;
  Exclude(FRestoreOptions, roIgnoreVersion); //deprecated and to be removed
end;

end.


