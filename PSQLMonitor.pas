{$I pSQLDAC.inc}
unit PSQLMonitor;

{SVN revision: $Id$}

interface

uses
  SysUtils, {$IFDEF FPC}LCLIntf,{$ENDIF}{$IFDEF MSWINDOWS} Windows, Messages,{$ENDIF}
  Classes, PSQLAccess, DB, PSQLDbTables, PSQLTypes
  {$IFDEF DELPHI_16}, System.SyncObjs{$ENDIF}
  {$IFDEF DELPHI_17}, System.Types{$ENDIF}
  {$IFDEF NEXTGEN}, Generics.Collections{$ENDIF};

const
  {$IFNDEF MSWINDOWS}
  WM_USER = $400;
  {$ENDIF}

  CM_BASE                   = $B000;
  CM_RELEASE                = CM_BASE + 33;

  WM_MIN_MONITOR = WM_USER;
  WM_MAX_MONITOR = WM_USER + 512;
  WM_SQL_EVENT = WM_MIN_MONITOR + 1;

  CRLF = #13#10;

type
  TPSQLCustomMonitor = class;

  EPSQLMonitorError = class(EPSQLDatabaseError);//mi:2007-04-27 EPSQLMonitorError inherited from EPSQLDACException


  TPSQLTraceFlag = (tfQPrepare, tfQExecute, tfQFetch,tfConnect, tfTransact,tfMisc);
  TPSQLTraceFlags = set of TPSQLTraceFlag;

  TSQLEvent = procedure(const Application, Database, Msg, SQL, ErrorMsg: string;
      DataType: TPSQLTraceFlag; const ExecutedOK: boolean; EventTime: TDateTime) of object;

  TPSQLCustomMonitor = class(TComponent)
  private
    FHWnd: THandle;
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TPSQLTraceFlags;
    FActive: Boolean;  protected
{$IFDEF MSWINDOWS}
    procedure MonitorWndProc(var Message : TMessage);
{$ENDIF}
    procedure SetActive(const Value: Boolean);
    procedure SetTraceFlags(const Value: TPSQLTraceFlags);
  protected
    property OnSQL      : TSQLEvent read FOnSQLEvent write FOnSQLEvent;
    property TraceFlags : TPSQLTraceFlags read FTraceFlags write SetTraceFlags;
    property Active     : Boolean read FActive write SetActive default true;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  Release;
    property   Handle : THandle read FHwnd;
  end;

  TPSQLMonitor = class(TPSQLCustomMonitor)
  private
    FAbout : TPSQLDACAbout;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property OnSQL;
    property TraceFlags;
    property Active;
  end;

  TPSQLMonitorHook = class(TObject)
  private
    FActive: Boolean;
    vEventsCreated : Boolean;
    procedure CreateEvents;
  protected
    procedure WriteSQLData(const ADatabase, AMsg, ASQL: string; DataType: TPSQLTraceFlag;
      AExecOK: boolean; const AErrorMsg: string = '');
  public
    constructor Create;
    destructor Destroy; override;
    procedure TerminateWriteThread;
    function  SQLString(k:integer):Byte;
    procedure RegisterMonitor(SQLMonitor : TPSQLCustomMonitor);
    procedure UnregisterMonitor(SQLMonitor : TPSQLCustomMonitor);
    procedure ReleaseMonitor(Arg : TPSQLCustomMonitor);
    procedure SQLPrepare(qry: TNativeDataset); virtual;
    procedure SQLExecute(qry: TNativeDataset; const AExecOK: boolean); overload; virtual;
    procedure SQLExecute(db: TNativeConnect; const Sql: string; const AExecOK: boolean); overload; virtual;
    procedure SQLFetch(qry: TNativeDataset); virtual;
    procedure DBConnect(db: TNativeConnect; const AExecOK: boolean); virtual;
    procedure DBDisconnect(db: TNativeConnect); virtual;
    procedure TRStart(db: TNativeConnect; const AExecOK: boolean); virtual;
    procedure TRCommit(db: TNativeConnect; const AExecOK: boolean); virtual;
    procedure TRRollback(db: TNativeConnect; const AExecOK: boolean); virtual;
    procedure SendMisc(Msg : String);
    function  GetEnabled: Boolean;
    function  GetMonitorCount : Integer;
    procedure SetEnabled(const Value: Boolean);
    property Enabled : Boolean read GetEnabled write SetEnabled default true;
  end;

function MonitorHook: TPSQLMonitorHook;
procedure EnableMonitoring;
procedure DisableMonitoring;
function MonitoringEnabled: Boolean;


implementation

uses
   Math{$IFNDEF FMX_AVAILABLE}, Forms{$ENDIF};

procedure MonError(ErrMess: String; const Args: array of const);
begin
  raise EPSQLMonitorError.CreateFmt(ErrMess, Args);
end;

function IsBlank(const Str: string) : boolean;
var
  L: Integer;
begin
  L := Length(Str);
  while (L > 0) and (Str[L] <= ' ') do Dec(L);
  result := L = 0;
end;


type
  TPSQLTraceObject = Class(TObject)
  private
    FDataType : TPSQLTraceFlag;
    FMsg : String;
    FTimeStamp : TDateTime;
    FDatabase: string;
    FExecutedOK: boolean;
    FSQL: string;
    FApplication: string;
    FErrorMsg: string;
  public
    constructor Create(const AAppName, ADatabase, AMsg, ASQL: string;
      ADataType: TPSQLTraceFlag; const AExecOK: boolean; const AErrorMsg: string = '');

    property DataType: TPSQLTraceFlag read FDataType;
    property Application: string read FApplication;
    property Msg: string read FMsg;
    property SQL: string read FSQL;
    property TimeStamp: TDateTime read FTimeStamp;
    property Database: string read FDatabase;
    property ExecutedOK: boolean read FExecutedOK;
    property ErrorMsg: string read FErrorMsg;
  end;

  TReleaseObject = Class(TObject)
  private
    FHandle : THandle;
  public
    constructor Create(Handle : THandle);
  end;

  TMonitorWriterThread = class(TThread)
  private
    StopExec: boolean;
    FMonitorMsgs : TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  protected
    procedure Lock;
    Procedure Unlock;
    procedure BeginWrite;
    procedure EndWrite;
    procedure Execute; override;
    procedure WriteToBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteSQLData(const AAppName, ADatabase, AMsg, ASQL: String;
      ADataType: TPSQLTraceFlag; AExecOK: boolean; const AErrorMsg: string = '');
    procedure ReleaseMonitor(HWnd : THandle);
  end;

  TMonitorReaderThread = class(TThread)
  private
    st : TPSQLTraceObject;
    FMonitors : TList{$IFDEF NEXTGEN}<TPSQLCustomMonitor>{$ENDIF};
  protected
    procedure BeginRead;
    procedure EndRead;
    procedure ReadSQLData;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure  AddMonitor(Arg : TPSQLCustomMonitor);
    procedure  RemoveMonitor(Arg : TPSQLCustomMonitor);
  end;

const
  MonitorHookNames: array[0..5] of String = (
    'PSQL.SQL.MONITOR.Mutex',
    'PSQL.SQL.MONITOR.SharedMem',
    'PSQL.SQL.MONITOR.WriteEvent',
    'PSQL.SQL.MONITOR.WriteFinishedEvent',
    'PSQL.SQL.MONITOR.ReadEvent',
    'PSQL.SQL.MONITOR.ReadFinishedEvent');

  cMonitorHookSize = 2048;
  cMaxBufferSize = cMonitorHookSize - (9 * SizeOf(Integer)) - SizeOf(TDateTime) - 2*SizeOf(Byte);
  cDefaultTimeout = 1000; // 1 second

var
  {$IFNDEF NEXTGEN}
  FAppSharedBuf,
  FDBSharedBuf,
  FMsgSharedBuf,
  FSQLSharedBuf,
  FErrSharedBuf,
  FWriteLock,
  FWriteEvent,
  FWriteFinishedEvent,
  FReadEvent,
  FReadFinishedEvent : THandle;

  {$ENDIF}

  FAppBuffer,
  FDBBuffer,
  FMsgBuffer,
  FSQLBuffer,
  FErrBuffer: PAnsiDACChar;

  FMonitorCount,
  {$IFNDEF NEXTGEN}
  FReaderCount,
  {$ENDIF}
  FTraceDataType,
  FQPrepareReaderCount,
  FQExecuteReaderCount,
  FQFetchReaderCount,
  FConnectReaderCount,
  FTransactReaderCount,
  FAppBufSize,
  FDBBufSize,
  FMsgBufSize,
  FSQLBufSize,
  FErrBufSize,
  FExecOK: PInteger;
//  FBufferSize : PInteger;
  FTimeStamp  : PDateTime;
  {$IFNDEF NEXTGEN}
  FReserved   : PByte;
  FReserved1  : PByte;
  {$ENDIF}

  FPSQLWriterThread : TMonitorWriterThread;
  FPSQLReaderThread : TMonitorReaderThread;

  _MonitorHook: TPSQLMonitorHook;

  bDone: Boolean;
{$IFDEF DELPHI_16}
  CS: TCriticalSection;
{$ELSE}
  CS: TRTLCriticalSection;
{$ENDIF}

  bEnabledMonitoring:boolean;

constructor TPSQLCustomMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := true;
  if not (csDesigning in ComponentState) then
  begin
  {$IFDEF MSWINDOWS}
     FHWnd := {$IFDEF DELPHI_6}Classes.{$ENDIF}AllocateHWnd(MonitorWndProc);
  {$ENDIF}
     MonitorHook.RegisterMonitor(self);
  end;
  TraceFlags := [tfqPrepare .. tfTransact];
end;
//----------------------------------------------------------------------------------------------------------------------
destructor TPSQLCustomMonitor.Destroy();
begin
  if not (csDesigning in ComponentState) then
  begin
   if (tfQPrepare in TraceFlags) then
     {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQPrepareReaderCount^);
   if (tfQExecute in TraceFlags) then
     {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQExecuteReaderCount^);
   if (tfQFetch in TraceFlags) then
     {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQFetchReaderCount^);
   if (tfConnect in TraceFlags) then
     {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FConnectReaderCount^);
   if (tfTransact in TraceFlags) then
     {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FTransactReaderCount^);
   if FActive then
      MonitorHook.UnregisterMonitor(self);
   {$IFDEF MSWINDOWS}
     {$IFDEF DELPHI_6}Classes.{$ENDIF}DeallocateHwnd(FHWnd);
   {$ENDIF MSWINDOWS}
  end;

  inherited Destroy;
end;

{$IFDEF MSWINDOWS}
procedure TPSQLCustomMonitor.MonitorWndProc(var Message: TMessage);
var
  st : TPSQLTraceObject;
begin
   case Message.Msg of
     WM_SQL_EVENT: begin
                      st := TPSQLTraceObject(Message.LParam);
                      if (Assigned(FOnSQLEvent)) and
                         (st.FDataType in FTraceFlags) then
                         FOnSQLEvent(st.Application, st.Database, st.Msg,
                          st.SQL, st.ErrorMsg, st.DataType, st.ExecutedOK, st.TimeStamp);
                      st.Free;
                   end;

     CM_RELEASE :  Free;
   else
     DefWindowProc(FHWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;
{$ENDIF}

procedure TPSQLCustomMonitor.Release;
begin
  MonitorHook.ReleaseMonitor(self);
end;

procedure TPSQLCustomMonitor.SetActive(const Value: Boolean);
begin
   if Value <> FActive then
   begin
      FActive := Value;
      if not (csDesigning in ComponentState) then
         if FActive then
            Monitorhook.RegisterMonitor(self) else
            MonitorHook.UnregisterMonitor(self);
  end;
end;

procedure TPSQLCustomMonitor.SetTraceFlags(const Value: TPSQLTraceFlags);
begin
   if not (csDesigning in ComponentState) then
   begin
      if (tfQPrepare in TraceFlags) and not (tfQPrepare in Value) then
        {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQPrepareReaderCount^) else
         if (not (tfQPrepare in TraceFlags)) and (tfQPrepare in Value) then
           {$IFDEF DELPHI_16}TInterlocked.Increment{$ELSE}InterlockedIncrement{$ENDIF}(FQPrepareReaderCount^);
      if (tfQExecute in TraceFlags) and not (tfQExecute in Value) then
        {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQExecuteReaderCount^) else
         if (not (tfQExecute in TraceFlags)) and (tfQExecute in Value) then
           {$IFDEF DELPHI_16}TInterlocked.Increment{$ELSE}InterlockedIncrement{$ENDIF}(FQExecuteReaderCount^);
      if (tfQFetch in TraceFlags) and not (tfQFetch in Value) then
        {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FQFetchReaderCount^) else
         if (not (tfQFetch in TraceFlags)) and (tfQFetch in Value) then
           {$IFDEF DELPHI_16}TInterlocked.Increment{$ELSE}InterlockedIncrement{$ENDIF}(FQFetchReaderCount^);
      if (tfConnect in TraceFlags) and not (tfConnect in Value) then
        {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FConnectReaderCount^) else
         if (not (tfConnect in TraceFlags)) and (tfConnect in Value) then
           {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FConnectReaderCount^);
      if (tfTransact in TraceFlags) and not (tfTransact in Value) then
        {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FTransactReaderCount^) else
         if (not (tfTransact in TraceFlags)) and (tfTransact in Value) then
           {$IFDEF DELPHI_16}TInterlocked.Increment{$ELSE}InterlockedIncrement{$ENDIF}(FTransactReaderCount^);
   end;
   FTraceFlags:=Value
end;


constructor TPSQLMonitorHook.Create;
begin
  inherited Create;
  vEventsCreated := false;
  FActive := true;
  if not vEventsCreated then
  try
    CreateEvents;
  except
    Enabled := false;
    Exit;
  end;
end;


procedure TPSQLMonitorHook.CreateEvents;
{$IFDEF MSWINDOWS}
var
  Sa : TSecurityAttributes;
  Sd : TSecurityDescriptor;
  MapError: Integer;

  function CreateLocalEvent(Idx: Integer; InitialState: Boolean): THandle;
  begin
    Result := CreateEvent(@sa, true, InitialState, PChar(MonitorHookNames[Idx]));
    if Result = 0 then
       MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;


  function OpenLocalEvent(Idx: Integer): THandle;
  begin
    Result := OpenEvent(EVENT_ALL_ACCESS, true, PChar(MonitorHookNames[Idx]));
    if Result = 0 then
       MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

begin
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;

  FAppSharedBuf := CreateFileMapping(INVALID_HANDLE_VALUE, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1] + '01'));

  MapError:=GetLastError;
  if  MapError= ERROR_ALREADY_EXISTS then
  begin
     FAppSharedBuf := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1] + '01'));
     if (FAppSharedBuf = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end else
  begin
     FWriteLock := CreateMutex(@sa, False, PChar(MonitorHookNames[0]));
     FWriteEvent := CreateLocalEvent(2, False);
     FWriteFinishedEvent := CreateLocalEvent(3, True);
     FReadEvent := CreateLocalEvent(4, False);
     FReadFinishedEvent := CreateLocalEvent(5, False);
  end;

  FDBSharedBuf := CreateFileMapping(INVALID_HANDLE_VALUE, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1] + '02'));

  MapError := GetLastError();
  if MapError = ERROR_ALREADY_EXISTS then
  begin
     FDBSharedBuf := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1] + '02'));
     if (FDBSharedBuf = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

  FMsgSharedBuf := CreateFileMapping(INVALID_HANDLE_VALUE, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1] + '03'));

  MapError := GetLastError;
  if MapError = ERROR_ALREADY_EXISTS then
  begin
     FMsgSharedBuf := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1] + '03'));
     if (FMsgSharedBuf = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

  FSQLSharedBuf := CreateFileMapping(INVALID_HANDLE_VALUE, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1] + '04'));

  MapError := GetLastError;
  if MapError = ERROR_ALREADY_EXISTS then
  begin
     FSQLSharedBuf := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1] + '04'));
     if (FSQLSharedBuf = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

  FErrSharedBuf := CreateFileMapping(INVALID_HANDLE_VALUE, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1] + '05'));

  MapError := GetLastError;
  if MapError = ERROR_ALREADY_EXISTS then
  begin
     FErrSharedBuf := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1] + '05'));
     if (FErrSharedBuf = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

//  FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  FAppBuffer := MapViewOfFile(FAppSharedBuf, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  FDBBuffer := MapViewOfFile(FDBSharedBuf, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  FMsgBuffer := MapViewOfFile(FMsgSharedBuf, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  FSQLBuffer := MapViewOfFile(FSQLSharedBuf, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  FErrBuffer := MapViewOfFile(FErrSharedBuf, FILE_MAP_ALL_ACCESS, 0, 0, 0);

  if FAppBuffer = nil then
     MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
//  FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
  FMonitorCount := PInteger(FAppBuffer + cMonitorHookSize - SizeOf(Integer));
  FReaderCount  := PInteger(PAnsiChar(FMonitorCount)      -   SizeOf(Integer));
  FTraceDataType:= PInteger(PAnsiChar(FMonitorCount)      - 2*SizeOf(Integer));
  FExecOK := PInteger(PAnsiChar(FMonitorCount)      - 3*SizeOf(Integer));
//  FBufferSize   := PInteger(PChar(FMonitorCount)      - 3*SizeOf(Integer));
  FAppBufSize   := PInteger(PAnsiChar(FMonitorCount)      - 4*SizeOf(Integer));
  FDBBufSize   := PInteger(PAnsiChar(FMonitorCount)      - 5*SizeOf(Integer));
  FMsgBufSize   := PInteger(PAnsiChar(FMonitorCount)      - 6*SizeOf(Integer));
  FSQLBufSize   := PInteger(PAnsiChar(FMonitorCount)      - 7*SizeOf(Integer));
  FErrBufSize   := PInteger(PAnsiChar(FMonitorCount)      - 8*SizeOf(Integer));
  FQPrepareReaderCount:=PInteger(PAnsiChar(FMonitorCount) - 9*SizeOf(Integer));
  FQExecuteReaderCount:=PInteger(PAnsiChar(FMonitorCount) - 10*SizeOf(Integer));
  FQFetchReaderCount  :=PInteger(PAnsiChar(FMonitorCount) - 11*SizeOf(Integer));
  FConnectReaderCount :=PInteger(PAnsiChar(FMonitorCount) - 12*SizeOf(Integer));
  FTransactReaderCount:=PInteger(PAnsiChar(FMonitorCount) - 13*SizeOf(Integer));
  FTimeStamp    := PDateTime(PAnsiChar(FTransactReaderCount)- SizeOf(TDateTime));
  FReserved     := PByte(PAnsiChar(FTimeStamp)- SizeOf(Byte));
  FReserved1    := PByte(PAnsiChar(FReserved )- SizeOf(Byte));
  if  MapError= ERROR_ALREADY_EXISTS then
  begin
     FWriteLock  := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MonitorHookNames[0]));
     FWriteEvent := OpenLocalEvent(2);
     FWriteFinishedEvent := OpenLocalEvent(3);
     FReadEvent  := OpenLocalEvent(4);
     FReadFinishedEvent  := OpenLocalEvent(5);
  end else
  begin
     FMonitorCount^       :=0;
     FReaderCount^        :=0;
//     FBufferSize^         :=0;
     FMsgBufSize^         :=0;
     FQPrepareReaderCount^:=0;
     FQExecuteReaderCount^:=0;
     FQFetchReaderCount^  :=0;
     FConnectReaderCount^ :=0;
     FTransactReaderCount^:=0;
  end;
  if FMonitorCount^ < 0 then
     FMonitorCount^ := 0;
  if FReaderCount^ < 0 then
     FReaderCount^ := 0;
  vEventsCreated := true;
end;
{$ELSE}
begin
//no support for non-Windows environment yet
end;
{$ENDIF}

function  TPSQLMonitorHook.SQLString(k:integer):Byte;
begin
  Result := 127 + k - k;
end;

procedure TPSQLMonitorHook.DBConnect(db: TNativeConnect; const AExecOK: boolean);
{var
  st : String;}
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FConnectReaderCount^>0) then
   begin
//      st := db.DBOptions.DatabaseName + ': [Connect]'; {do not localize}
      WriteSQLData(db.DBOptions.DatabaseName, 'Connect', '', tfConnect, AExecOK, db.GetErrorText);
   end;
end;

procedure TPSQLMonitorHook.DBDisconnect(db: TNativeConnect);
{var
  st: String;}
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FConnectReaderCount^>0) then
   begin
//      st := db.DBOptions.DatabaseName + ': [Disconnect]'; {do not localize}
      WriteSQLData(db.DBOptions.DatabaseName, 'Disconnect', '', tfConnect, True);
   end;
end;

destructor TPSQLMonitorHook.Destroy;
begin
{$IFDEF MSWINDOWS}
   if vEventsCreated then
   begin
//      UnmapViewOfFile(FBuffer);
      UnmapViewOfFile(FAppBuffer);
      UnmapViewOfFile(FDBBuffer);
      UnmapViewOfFile(FMsgBuffer);
      UnmapViewOfFile(FSQLBuffer);
      UnmapViewOfFile(FErrBuffer);
      CloseHandle(FAppSharedBuf);
      CloseHandle(FDBSharedBuf);
      CloseHandle(FMsgSharedBuf);
      CloseHandle(FSQLSharedBuf);
      CloseHandle(FErrSharedBuf);

//      CloseHandle(FSharedBuffer);
      CloseHandle(FWriteEvent);
      CloseHandle(FWriteFinishedEvent);
      CloseHandle(FReadEvent);
      CloseHandle(FReadFinishedEvent);
      CloseHandle(FWriteLock);
   end;
{$ENDIF}
   inherited Destroy;
end;

function TPSQLMonitorHook.GetEnabled: Boolean;
begin
   Result := FActive;
end;

function TPSQLMonitorHook.GetMonitorCount: Integer;
begin
  if FMonitorCount = nil then
    Result := 0
  else
    Result := FMonitorCount^;
end;

procedure TPSQLMonitorHook.RegisterMonitor(SQLMonitor: TPSQLCustomMonitor);
begin
   if not vEventsCreated then
   try
     CreateEvents;
   except
     SQLMonitor.Active := false;
   end;
   if not Assigned(FPSQLReaderThread) then
      FPSQLReaderThread := TMonitorReaderThread.Create;
   FPSQLReaderThread.AddMonitor(SQLMonitor);
end;

procedure TPSQLMonitorHook.ReleaseMonitor(Arg: TPSQLCustomMonitor);
begin
   FPSQLWriterThread.ReleaseMonitor(Arg.FHWnd);
end;

procedure TPSQLMonitorHook.SendMisc(Msg: String);
begin
   if FActive then
      WriteSQLData('', Msg, '', tfMisc, False);
end;

procedure TPSQLMonitorHook.SetEnabled(const Value: Boolean);
begin
   if FActive <> Value then
      FActive := Value;
   if (not FActive) and (Assigned(FPSQLWriterThread)) then
   begin
      FPSQLWriterThread.Terminate;
      FPSQLWriterThread.WaitFor;
      FPSQLWriterThread.Free;
      FPSQLWriterThread := nil;
   end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLMonitorHook.SQLExecute(qry: TNativeDataset; const AExecOK: boolean);
var
  st: string;
begin
  if FActive and  bEnabledMonitoring  and (GetMonitorCount>0)
    and (FQExecuteReaderCount^ > 0)
  then
  begin
    if qry.SQLQuery <> '' then
      st := qry.SQLQuery
    else
      st := string(qry.TableName);

    WriteSQLData(qry.Connect.DBOptions.DatabaseName, 'Execute', st,
                 tfQExecute, AExecOK, qry.Connect.GetErrorText);
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLMonitorHook.SQLExecute(db: TNativeConnect; const Sql: string; const AExecOK: boolean);
begin
  if FActive and  bEnabledMonitoring  and (GetMonitorCount > 0) and
     (FQExecuteReaderCount^ > 0)
  then
  begin
    WriteSQLData(db.DBOptions.DatabaseName, 'Execute', Sql,
        tfQExecute, AExecOK, db.GetErrorText);
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLMonitorHook.SQLFetch(qry: TNativeDataset);
var
  st: String;
begin
  if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
     and (FQFetchReaderCount^>0)
  then
  begin
    if qry.SQLQuery <> '' then
      st := 'Query'
    else
      st := string(qry.TableName);

    st := st + ': Row # '+ IntToStr(qry.RecordNumber) + CRLF;
    WriteSQLData(qry.Connect.DBOptions.DatabaseName, 'Fetch', st, tfQFetch, True);
  end;
end;

procedure TPSQLMonitorHook.SQLPrepare(qry: TNativeDataset);
var
  st: String;
begin
  if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
     and (FQPrepareReaderCount^>0)
  then
  begin
    if qry.SQLQuery <> '' then
      st := qry.SQLQuery
    else
      st := string(qry.TableName);

    WriteSQLData(qry.Connect.DBOptions.DatabaseName, 'Prepare', st, tfQPrepare, True);
  end;
end;

procedure TPSQLMonitorHook.TRCommit(db: TNativeConnect; const AExecOK: boolean);
{var
  st: String;}
begin
   if FActive and  bEnabledMonitoring  and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
//       st := db.DBOptions.DatabaseName + ': [Commit (Hard commit)]';
       WriteSQLData(db.DBOptions.DatabaseName, 'Commit (Hard commit)', '',
        tfTransact, AExecOK, db.GetErrorText);
   end;
end;

procedure TPSQLMonitorHook.TRRollback(db: TNativeConnect; const AExecOK: boolean);
{var
  st: String;}
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
//      st := db.DBOptions.DatabaseName + ': [Rollback]';
      WriteSQLData(db.DBOptions.DatabaseName, 'Rollback', '', tfTransact,
        AExecOK, db.GetErrorText);
   end;
end;

procedure TPSQLMonitorHook.TRStart(db: TNativeConnect; const AExecOK: boolean);
{var
  st: String;}
begin
   if FActive and  bEnabledMonitoring and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
//      st := db.DBOptions.DatabaseName + ': [Start transaction]';
//      WriteSQLData(st, tfTransact);
      WriteSQLData(db.DBOptions.DatabaseName, 'Start transaction', '', tfTransact,
        AExecOK, db.GetErrorText);
   end;
end;

procedure TPSQLMonitorHook.UnregisterMonitor(SQLMonitor: TPSQLCustomMonitor);
begin
   FPSQLReaderThread.RemoveMonitor(SQLMonitor);
   if FPSQLReaderThread.FMonitors.Count = 0 then
   begin
      FPSQLReaderThread.Terminate;
      if not Assigned(FPSQLWriterThread) then
      begin
         FPSQLWriterThread := TMonitorWriterThread.Create;
      end;
      FPSQLWriterThread.WriteSQLData('', '', '', '', tfMisc, True);
      FPSQLReaderThread.WaitFor;
      FPSQLReaderThread.Free;
      FPSQLReaderThread:=nil;
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLMonitorHook.WriteSQLData(const ADatabase, AMsg, ASQL: string;
  DataType: TPSQLTraceFlag; AExecOK: boolean; const AErrorMsg: string);
var
  AppName: string;
begin
  if not vEventsCreated then
  begin
    try
      CreateEvents;
    except
      Enabled := false;
      Exit;
    end;
  end;

  {$IFDEF FMX_AVAILABLE}
  AppName := ''; //cannot use TApplication.Title 'cause have no idea if it's FMX or VCL
  {$ELSE}
  AppName := Application.Title;
  {$ENDIF}
  if not Assigned(FPSQLWriterThread) then
    FPSQLWriterThread := TMonitorWriterThread.Create;

  FPSQLWriterThread.WriteSQLData(AppName, ADatabase, AMsg, ASQL, DataType, AExecOK, AErrorMsg);
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLMonitorHook.TerminateWriteThread;
begin
   if Assigned(FPSQLWriterThread) then
   begin
     FPSQLWriterThread.Free;
     FPSQLWriterThread:=nil
   end;
end;
//----------------------------------------------------------------------------------------------------------------------
constructor TMonitorWriterThread.Create;
begin
  StopExec := False;
  FMonitorMsgs := TList{$IFDEF NEXTGEN}<TObject>{$ENDIF}.Create;
  inherited Create(False);
  if FMonitorCount^ = 0 then
 {$WARNINGS OFF}
    Suspend;
 {$WARNINGS ON}
end;
//----------------------------------------------------------------------------------------------------------------------
destructor TMonitorWriterThread.Destroy;
var
  Msg:TObject;
begin
 {$WARNINGS OFF}
   Resume;
 {$WARNINGS ON}

   if FMonitorMsgs.Count>0 then
   begin
      Msg:=FMonitorMsgs[0];
      FMonitorMsgs.Delete(0);
      Msg.Free;
   end;

   FMonitorMsgs.Free;
   FMonitorMsgs := nil;//mi:2006-09-15
   inherited Destroy;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.Execute;
begin
{$IFDEF MSWINDOWS}
  while (Assigned(FMonitorMsgs)) and not StopExec do
  begin
    if (Terminated or bDone) and (FMonitorMsgs.Count = 0) then//mi:2006-09-15 removed from while condition to ensure FMonitorMsgs<>nil
      break;

    if (FMonitorCount^ = 0) then
      begin
        while FMonitorMsgs.Count <> 0 do
          FMonitorMsgs.Remove(FMonitorMsgs[0]);
 {$WARNINGS OFF}
        Suspend;
 {$WARNINGS ON}
      end

    else

      if FMonitorMsgs.Count <> 0 then
        begin
          if (TObject(FMonitorMsgs.Items[0]) is TReleaseObject) then
            PostMessage(TReleaseObject(FMonitorMsgs.Items[0]).FHandle, CM_RELEASE, 0, 0)
          else
            begin
              if bEnabledMonitoring  then
                WriteToBuffer()
              else
                begin
                  BeginWrite();
                  TPSQLTraceObject(FMonitorMsgs[0]).Free;
                  FMonitorMsgs.Delete(0);
                  EndWrite();
                end;
            end;
        end
      else
 {$WARNINGS OFF}
        Suspend;
 {$WARNINGS ON}
  end;
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.Lock;
begin
{$IFDEF MSWINDOWS}
   WaitForSingleObject(FWriteLock, INFINITE);
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.Unlock;
begin
{$IFDEF MSWINDOWS}
   ReleaseMutex(FWriteLock);
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.WriteSQLData(const AAppName, ADatabase, AMsg, ASQL: String;
  ADataType: TPSQLTraceFlag; AExecOK: boolean; const AErrorMsg: string);
var
  mto : TPSQLTraceObject;
begin
  if (FMonitorCount^ <> 0) then
  begin
    mto := TPSQLTraceObject.Create(AAppName, ADatabase, AMsg, ASQL, ADataType, AExecOK, AErrorMsg);
    FMonitorMsgs.Add(mto);

 {$WARNINGS OFF}
    Resume;
 {$WARNINGS ON}
  end
  else
  begin
    FreeAndNil(FPSQLWriterThread)
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.BeginWrite;
begin
   Lock();
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.EndWrite;
begin
  {
   * 1. Wait to end the write until all registered readers have
   *    started to wait for a write event
   * 2. Block all of those waiting for the write to finish.
   * 3. Block all of those waiting for all readers to finish.
   * 4. Unblock all readers waiting for a write event.
   * 5. Wait until all readers have finished reading.
   * 6. Now, block all those waiting for a write event.
   * 7. Unblock all readers waiting for a write to be finished.
   * 8. Unlock the mutex.
   }
{$IFDEF MSWINDOWS}
  while WaitForSingleObject(FReadEvent, cDefaultTimeout) = WAIT_TIMEOUT do
  begin
    if FMonitorCount^ > 0 then
      InterlockedDecrement(FMonitorCount^);

    if (FReaderCount^ = FMonitorCount^ - 1) or (FMonitorCount^ = 0) then
      SetEvent(FReadEvent);
  end;

  ResetEvent(FWriteFinishedEvent);
  ResetEvent(FReadFinishedEvent);
  SetEvent(FWriteEvent); { Let all readers pass through. }

  while WaitForSingleObject(FReadFinishedEvent, cDefaultTimeout) = WAIT_TIMEOUT do
  begin
    if (FReaderCount^ = 0) or (InterlockedDecrement(FReaderCount^) = 0) then
      SetEvent(FReadFinishedEvent);
  end;

  ResetEvent(FWriteEvent);
  SetEvent(FWriteFinishedEvent);
  Unlock();
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.WriteToBuffer();
{$IFDEF MSWINDOWS}
//local procedures
  procedure _WriteStrToBuf(const S: string; Buf: PAnsiChar; BufSize: PInteger);
  var
    i, len: Integer;
    Text: String;
    ptr : {$IFDEF DELPHI_12}PByte{$ELSE}PChar{$ENDIF};
  begin
    Text  := EmptyStr;

    for i := 1 to length(S) do
    begin
      if ((S[i] >= #0) and (S[i] <= #9))
        or (S[i] = #$B)
        or (S[i] = #$C)
        or ((S[i] >= #$E) and (S[i] <= #31))
      then
        Text := Text + '#$' + IntToHex(ord(S[i]), 2)
      else
        Text := Text + S[i];
    end;

    len := Length(Text) * sizeof(Char);//mi:2008-11-12 unicode compatibility
    ptr := {$IFDEF DELPHI_12}PByte{$ELSE}PChar{$ENDIF}(Text);//mi:2008-11-12

    BufSize^ := 0;
//   Move(#0, Buf[0], BufSize^);
    while (len > 0) do
    begin
      BufSize^ := Min(len, cMaxBufferSize);
//      Move(ptr, Buf, BufSize^);
      CopyMemory(pointer(Buf), ptr, BufSize^);
      Inc(ptr, cMaxBufferSize);
      Dec(len, cMaxBufferSize);
    end;
  end;

begin
  Lock();

  try
    if FMonitorCount^ = 0 then
    begin
        FMonitorMsgs.Remove(FMonitorMsgs[0]);
    end
    else
    begin
      BeginWrite();

      try
        _WriteStrToBuf(TPSQLTraceObject(FMonitorMsgs[0]).Application, FAppBuffer, FAppBufSize);
        _WriteStrToBuf(TPSQLTraceObject(FMonitorMsgs[0]).Database, FDBBuffer, FDBBufSize);
        _WriteStrToBuf(TPSQLTraceObject(FMonitorMsgs[0]).Msg, FMsgBuffer, FMsgBufSize);
        _WriteStrToBuf(TPSQLTraceObject(FMonitorMsgs[0]).SQL, FSQLBuffer, FSQLBufSize);
        _WriteStrToBuf(TPSQLTraceObject(FMonitorMsgs[0]).ErrorMsg, FErrBuffer, FErrBufSize);

        FTraceDataType^ := Integer(TPSQLTraceObject(FMonitorMsgs[0]).DataType);
        FTimeStamp^ := TPSQLTraceObject(FMonitorMsgs[0]).TimeStamp;
        FExecOK^ := Ord(TPSQLTraceObject(FMonitorMsgs[0]).ExecutedOK);
      finally
        EndWrite();
      end;
    end;

    if FMonitorMsgs.Count > 0 then
    begin
      TPSQLTraceObject(FMonitorMsgs[0]).Free();
      FMonitorMsgs.Delete(0);
    end;
  finally
    Unlock();
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorWriterThread.ReleaseMonitor(HWnd: THandle);
begin
  FMonitorMsgs.Add(TReleaseObject.Create(HWnd));
end;

{ TPSQLTraceObject }

constructor TPSQLTraceObject.Create(const AAppName, ADatabase, AMsg, ASQL: string;
  ADataType: TPSQLTraceFlag; const AExecOK: boolean; const AErrorMsg: string);
begin
  FApplication := AAppName;
  FDatabase := ADatabase;
  FMsg := AMsg;
  FSQL := ASQL;
  FDataType := ADataType;
  FExecutedOK := AExecOK;
  FTimeStamp := Now;
  FErrorMsg := AErrorMsg;
end;

{TReleaseObject}

constructor TReleaseObject.Create(Handle: THandle);
begin
   FHandle := Handle;
end;

{ReaderThread}

procedure TMonitorReaderThread.AddMonitor(Arg: TPSQLCustomMonitor);
begin
{$IFDEF DELPHI_16}
  CS.Enter;
{$ELSE}
  EnterCriticalSection(CS);
{$ENDIF}
  try
    if FMonitors.IndexOf(Arg) < 0 then
      FMonitors.Add(Arg);
  finally
 {$IFDEF DELPHI_16}
   CS.Leave;
 {$ELSE}
   LeaveCriticalSection(CS);
 {$ENDIF}
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorReaderThread.BeginRead();
begin
{$IFDEF MSWINDOWS}
  {
   * 1. Wait for the "previous" write event to complete.
   * 2. Increment the number of readers.
   * 3. if the reader count is the number of interested readers, then
   *    inform the system that all readers are ready.
   * 4. Finally, wait for the FWriteEvent to signal.
   }
  WaitForSingleObject(FWriteFinishedEvent, INFINITE);
  InterlockedIncrement(FReaderCount^);
  if FReaderCount^ = FMonitorCount^ then
     SetEvent(FReadEvent);
  WaitForSingleObject(FWriteEvent, INFINITE);
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
constructor TMonitorReaderThread.Create;
begin
   inherited Create(true);
{$IFDEF MSWINDOWS}
   st := TPSQLTraceObject.Create('', '', '', '', tfMisc, True);
   FMonitors := TList{$IFDEF NEXTGEN}<TPSQLCustomMonitor>{$ENDIF}.Create;
   InterlockedIncrement(FMonitorCount^);
   if Suspended then
 {$WARNINGS OFF}
    Resume;
 {$WARNINGS ON}
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
destructor TMonitorReaderThread.Destroy;
begin
{$IFDEF MSWINDOWS}
   if FMonitorCount^ > 0 then
      {$IFDEF DELPHI_16}TInterlocked.Decrement{$ELSE}InterlockedDecrement{$ENDIF}(FMonitorCount^);
   FreeAndNil(FMonitors);
   FreeAndNil(st);
{$ENDIF}
   inherited Destroy;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorReaderThread.EndRead;
begin
{$IFDEF MSWINDOWS}
   if InterlockedDecrement(FReaderCount^) = 0 then
   begin
      ResetEvent(FReadEvent);
      SetEvent(FReadFinishedEvent);
   end;
{$ENDIF}
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorReaderThread.Execute;
var
  i : Integer;
  FTemp : TPSQLTraceObject;
begin
  while (not Terminated) and (not bDone) do
  begin
    ReadSQLData();

    if not IsBlank(st.FMsg) then
    for i := 0 to FMonitors.Count - 1 do
    begin
      FTemp := TPSQLTraceObject.Create(st.Application, st.Database,
                                        st.Msg, st.SQL, st.FDataType, st.ExecutedOK, st.ErrorMsg);
{$IFDEF MSWINDOWS}
      PostMessage(TPSQLCustomMonitor(FMonitors[i]).Handle,
                  WM_SQL_EVENT,
                  0,
                  LPARAM(FTemp));
{$ENDIF}
    end;
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorReaderThread.ReadSQLData();
  function _ReadStr(Buffer : PAnsiDACChar; Len : Cardinal) : string;
  begin
    {$IFDEF DELPHI_12}
    SetString(Result, PChar(Buffer), Len div sizeof(char));
    {$ELSE}
    SetString(Result, Buffer, Len);
    {$ENDIF}
  end;
begin
  st.FMsg := '';
  st.FApplication := '';
  st.FDatabase := '';
  st.FSQL := '';
  st.FErrorMsg := '';

  BeginRead();
  if not bDone then
  try
    st.FApplication := _ReadStr(FAppBuffer, FAppBufSize^);
    st.FDatabase := _ReadStr(FDBBuffer, FDBBufSize^);
    st.FMsg := _ReadStr(FMsgBuffer, FMsgBufSize^);
    st.FSQL := _ReadStr(FSQLBuffer, FSQLBufSize^);
    st.FErrorMsg := _ReadStr(FErrBuffer, FErrBufSize^);

    st.FDataType := TPSQLTraceFlag(FTraceDataType^);
    st.FTimeStamp := TDateTime(FTimeStamp^);
    st.FExecutedOK := Boolean(FExecOK^);
  finally
    EndRead();
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TMonitorReaderThread.RemoveMonitor(Arg: TPSQLCustomMonitor);
begin
{$IFDEF DELPHI_16}
  CS.Enter;
{$ELSE}
  EnterCriticalSection(CS);
{$ENDIF}
  try
   FMonitors.Remove(Arg);
  finally
 {$IFDEF DELPHI_16}
   CS.Leave;
 {$ELSE}
   LeaveCriticalSection(CS);
 {$ENDIF}
  end;
end;

function MonitorHook: TPSQLMonitorHook;
begin
  if (_MonitorHook = nil) and (not bDone) then
   begin
    {$IFDEF DELPHI_16}
    CS.Enter;
    {$ELSE}
    EnterCriticalSection(CS);
    {$ENDIF}

    if (_MonitorHook = nil) and (not bDone) then
    begin
       _MonitorHook := TPSQLMonitorHook.Create;
    end;

    {$IFDEF DELPHI_16}
    CS.Leave;
    {$ELSE}
    LeaveCriticalSection(CS);
    {$ENDIF}
   end;
  Result := _MonitorHook
end;

procedure EnableMonitoring;
begin
  bEnabledMonitoring := true;
end;

procedure DisableMonitoring;
begin
  bEnabledMonitoring :=false;
end;

function MonitoringEnabled: Boolean;
begin
  Result := bEnabledMonitoring;
end;


initialization
{$IFDEF DELPHI_16}
  CS := TCriticalSection.Create;
{$ELSE}
  InitializeCriticalSection(CS);
{$ENDIF}
  _MonitorHook := nil;
  FPSQLWriterThread := nil;
  FPSQLReaderThread := nil;
  bDone := False;
  bEnabledMonitoring := True;

finalization
  try
     bDone := True;

     if Assigned(FPSQLWriterThread) then
     begin
      {$WARNINGS OFF}
        FPSQLWriterThread.Resume;
      {$WARNINGS ON}
        FPSQLWriterThread.Terminate;
        FPSQLWriterThread.StopExec := True;
     {$IFDEF DELPHI_6}
        FPSQLWriterThread.WaitFor;
     {$ENDIF}
     end;

     if Assigned(FPSQLReaderThread) then
      begin
      {$WARNINGS OFF}
        FPSQLReaderThread.Resume;
      {$WARNINGS ON}
        FPSQLReaderThread.Terminate;
     {$IFDEF DELPHI_6}
        FPSQLReaderThread.WaitFor;
     {$ENDIF}
      end;

     FreeAndNil(_MonitorHook);

  finally
  {$IFDEF DELPHI_16}
    CS.Free;
  {$ELSE}
    DeleteCriticalSection(CS);
  {$ENDIF}
  end;
end.

