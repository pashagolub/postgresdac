{$I psqldac.inc}
unit PSQLMonitor;

interface

uses
  SysUtils, Windows, Messages, Classes, PSQLAccess,
  Dialogs, Forms, Controls,DB,PSQLDBTables;

const
  WM_MIN_MONITOR = WM_USER;
  WM_MAX_MONITOR = WM_USER + 512;
  WM_SQL_EVENT = WM_MIN_MONITOR + 1;

  CRLF = #13#10;

type
  TPSQLCustomMonitor = class;

  EPSQLMonitorError = class(EPSQLDatabaseError);


  TPSQLTraceFlag = (tfQPrepare, tfQExecute, tfQFetch, tfConnect, tfTransact, tfMisc);
  TPSQLTraceFlags = set of TPSQLTraceFlag;

  TSQLEvent = procedure(EventText: String; EventTime : TDateTime) of object;

  TPSQLCustomMonitor = class(TComponent)
  private
    FHWnd: HWND;
    FOnSQLEvent: TSQLEvent;
    FTraceFlags: TPSQLTraceFlags;
    FActive: Boolean;  protected
    procedure MonitorWndProc(var Message : TMessage);
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
    property   Handle : HWND read FHwnd;
  end;

  TPSQLMonitor = class(TPSQLCustomMonitor)
  published
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
    procedure WriteSQLData(const Text: String; DataType: TPSQLTraceFlag);
  public
    constructor Create;
    destructor Destroy; override;
    procedure TerminateWriteThread;
    function  SQLString(k:integer):Byte;
    procedure RegisterMonitor(SQLMonitor : TPSQLCustomMonitor);
    procedure UnregisterMonitor(SQLMonitor : TPSQLCustomMonitor);
    procedure ReleaseMonitor(Arg : TPSQLCustomMonitor);
    procedure SQLPrepare(qry: TNativeDataset); virtual;
    procedure SQLExecute(qry: TNativeDataset); virtual;
    procedure SQLFetch(qry: TNativeDataset); virtual;
    procedure DBConnect(db: TNativeConnect); virtual;
    procedure DBDisconnect(db: TNativeConnect); virtual;
    procedure TRStart(db: TNativeConnect); virtual;
    procedure TRCommit(db: TNativeConnect); virtual;
    procedure TRRollback(db: TNativeConnect); virtual;
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
   Math;

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
  public
    constructor Create(Msg : String; DataType : TPSQLTraceFlag);
  end;

  TReleaseObject = Class(TObject)
  private
    FHandle : THandle;
  public
    constructor Create(Handle : THandle);
  end;

  TMonitorWriterThread = class(TThread)
  private
    StopExec:boolean;
    FMonitorMsgs : TList;
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
    procedure WriteSQLData(Msg : String; DataType : TPSQLTraceFlag);
    procedure ReleaseMonitor(HWnd : THandle);
  end;

  TMonitorReaderThread = class(TThread)
  private
    st : TPSQLTraceObject;
    FMonitors : TList;
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
  cDefaultTimeout = 1000; // 1 seconds

var
  FSharedBuffer,
  FWriteLock,
  FWriteEvent,
  FWriteFinishedEvent,
  FReadEvent,
  FReadFinishedEvent : THandle;
  FBuffer : PChar;
  FMonitorCount,
  FReaderCount,
  FTraceDataType,
  FQPrepareReaderCount,
  FQExecuteReaderCount,
  FQFetchReaderCount,
  FConnectReaderCount,
  FTransactReaderCount,
  FBufferSize : PInteger;
  FTimeStamp  : PDateTime;
  FReserved   : PByte;
  FReserved1  : PByte;

  FPSQLWriterThread : TMonitorWriterThread;
  FPSQLReaderThread : TMonitorReaderThread;

  _MonitorHook: TPSQLMonitorHook;

  bDone: Boolean;
  CS : TRTLCriticalSection;
  bEnabledMonitoring:boolean;

constructor TPSQLCustomMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := true;
  if not (csDesigning in ComponentState) then
  begin
     FHWnd := {$IFDEF DELPHI_6}Classes.{$ENDIF}AllocateHWnd(MonitorWndProc);
     MonitorHook.RegisterMonitor(self);
  end;
  TraceFlags := [tfqPrepare .. tfTransact];
end;

destructor TPSQLCustomMonitor.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
     if (tfQPrepare in TraceFlags) then
        InterlockedDecrement(FQPrepareReaderCount^);
     if (tfQExecute in TraceFlags) then
        InterlockedDecrement(FQExecuteReaderCount^);
     if (tfQFetch in TraceFlags) then
        InterlockedDecrement(FQFetchReaderCount^);
     if (tfConnect in TraceFlags) then
        InterlockedDecrement(FConnectReaderCount^);
     if (tfTransact in TraceFlags) then
        InterlockedDecrement(FTransactReaderCount^);
     if FActive then
        MonitorHook.UnregisterMonitor(self);
     {$IFDEF DELPHI_6}Classes.{$ENDIF}DeallocateHwnd(FHWnd);
  end;
  inherited Destroy;
end;

procedure TPSQLCustomMonitor.MonitorWndProc(var Message: TMessage);
var
  st : TPSQLTraceObject;
begin
   case Message.Msg of
     WM_SQL_EVENT: begin
                      st := TPSQLTraceObject(Message.LParam);
                      if (Assigned(FOnSQLEvent)) and
                         (st.FDataType in FTraceFlags) then
                         FOnSQLEvent(st.FMsg, st.FTimeStamp);
                      st.Free;
                   end;
     CM_RELEASE :  Free;
   else
     DefWindowProc(FHWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

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
         InterlockedDecrement(FQPrepareReaderCount^) else
         if (not (tfQPrepare in TraceFlags)) and (tfQPrepare in Value) then
            InterlockedIncrement(FQPrepareReaderCount^);
      if (tfQExecute in TraceFlags) and not (tfQExecute in Value) then
         InterlockedDecrement(FQExecuteReaderCount^) else
         if (not (tfQExecute in TraceFlags)) and (tfQExecute in Value) then
            InterlockedIncrement(FQExecuteReaderCount^);
      if (tfQFetch in TraceFlags) and not (tfQFetch in Value) then
         InterlockedDecrement(FQFetchReaderCount^) else
         if (not (tfQFetch in TraceFlags)) and (tfQFetch in Value) then
            InterlockedIncrement(FQFetchReaderCount^);
      if (tfConnect in TraceFlags) and not (tfConnect in Value) then
         InterlockedDecrement(FConnectReaderCount^) else
         if (not (tfConnect in TraceFlags)) and (tfConnect in Value) then
            InterlockedIncrement(FConnectReaderCount^);
      if (tfTransact in TraceFlags) and not (tfTransact in Value) then
         InterlockedDecrement(FTransactReaderCount^) else
         if (not (tfTransact in TraceFlags)) and (tfTransact in Value) then
            InterlockedIncrement(FTransactReaderCount^);
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
var
  Sa : TSecurityAttributes;
  Sd : TSecurityDescriptor;
  MapError: Integer;

{$IFDEF VER100}
const
  SECURITY_DESCRIPTOR_REVISION = 1;
{$ENDIF}

  function OpenLocalEvent(Idx: Integer): THandle;
  begin
    Result := OpenEvent(EVENT_ALL_ACCESS, true, PChar(MonitorHookNames[Idx]));
    if Result = 0 then
       MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

  function CreateLocalEvent(Idx: Integer; InitialState: Boolean): THandle;
  begin
    Result := CreateEvent(@sa, true, InitialState, PChar(MonitorHookNames[Idx]));
    if Result = 0 then
       MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end;

begin
  InitializeSecurityDescriptor(@Sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@Sd,true,nil,false);
  Sa.nLength := SizeOf(Sa);
  Sa.lpSecurityDescriptor := @Sd;
  Sa.bInheritHandle := true;

  FSharedBuffer := CreateFileMapping($FFFFFFFF, @sa, PAGE_READWRITE,
                       0, cMonitorHookSize, PChar(MonitorHookNames[1]));

  MapError:=GetLastError;
  if  MapError= ERROR_ALREADY_EXISTS then
  begin
     FSharedBuffer := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PChar(MonitorHookNames[1]));
     if (FSharedBuffer = 0) then
        MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  end else
  begin
     FWriteLock := CreateMutex(@sa, False, PChar(MonitorHookNames[0]));
     FWriteEvent := CreateLocalEvent(2, False);
     FWriteFinishedEvent := CreateLocalEvent(3, True);
     FReadEvent := CreateLocalEvent(4, False);
     FReadFinishedEvent := CreateLocalEvent(5, False);
  end;
  FBuffer := MapViewOfFile(FSharedBuffer, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if FBuffer = nil then
     MonError('Cannot create shared resource. (Windows error %d)',[GetLastError]);
  FMonitorCount := PInteger(FBuffer + cMonitorHookSize - SizeOf(Integer));
  FReaderCount  := PInteger(PChar(FMonitorCount)      -   SizeOf(Integer));
  FTraceDataType:= PInteger(PChar(FMonitorCount)      - 2*SizeOf(Integer));
  FBufferSize   := PInteger(PChar(FMonitorCount)      - 3*SizeOf(Integer));
  FQPrepareReaderCount:=PInteger(PChar(FMonitorCount) - 4*SizeOf(Integer));
  FQExecuteReaderCount:=PInteger(PChar(FMonitorCount) - 5*SizeOf(Integer));
  FQFetchReaderCount  :=PInteger(PChar(FMonitorCount) - 6*SizeOf(Integer));
  FConnectReaderCount :=PInteger(PChar(FMonitorCount) - 7*SizeOf(Integer));
  FTransactReaderCount:=PInteger(PChar(FMonitorCount) - 8*SizeOf(Integer));
  FTimeStamp    := PDateTime(PChar(FTransactReaderCount)- SizeOf(TDateTime));
  FReserved     := PByte(PChar(FTimeStamp)- SizeOf(Byte));
  FReserved1    := PByte(PChar(FReserved )- SizeOf(Byte));

//  {$IFDEF TRIAL}
//     if Application.FindComponent('AppBuilder')<>nil then
//     begin
//      FReserved1^ := FReserved^;
//     end;
//  {$ENDIF}

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
     FBufferSize^         :=0;
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

function  TPSQLMonitorHook.SQLString(k:integer):Byte;
begin
// {$IFDEF TRIAL}
//  if (k mod 5)>0 then
//   Result:=FReserved^ else
//   Result:=FReserved1^;
// {$ELSE}
  Result:=127
// {$ENDIF}
end;

procedure TPSQLMonitorHook.DBConnect(db: TNativeConnect);
var
  st : String;
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FConnectReaderCount^>0) then
   begin
      st := string(db.DBOptions.DatabaseName) + ': [Connect]'; {do not localize}
      st := st + Format(' [Time elapsed (ms): %d]', [db.LastOperationTime]);
      WriteSQLData(st, tfConnect);
   end;
end;

procedure TPSQLMonitorHook.DBDisconnect(db: TNativeConnect);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FConnectReaderCount^>0) then
   begin
      st := string(db.DBOptions.DatabaseName) + ': [Disconnect]'; {do not localize}
      st := st + Format(' [Time elapsed (ms): %d]', [db.LastOperationTime]);
      WriteSQLData(st, tfConnect);
   end;
end;

destructor TPSQLMonitorHook.Destroy;
begin
   if vEventsCreated then
   begin
      UnmapViewOfFile(FBuffer);
      CloseHandle(FSharedBuffer);
      CloseHandle(FWriteEvent);
      CloseHandle(FWriteFinishedEvent);
      CloseHandle(FReadEvent);
      CloseHandle(FReadFinishedEvent);
      CloseHandle(FWriteLock);
   end;
   inherited Destroy;
end;

function TPSQLMonitorHook.GetEnabled: Boolean;
begin
   Result := FActive;
end;

function TPSQLMonitorHook.GetMonitorCount: Integer;
begin
   if FMonitorCount=nil then
      Result:=0 else
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
      WriteSQLData(Msg, tfMisc);
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
      FPSQLWriterThread:=nil;
   end;
end;

procedure TPSQLMonitorHook.SQLExecute(qry: TNativeDataset);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring  and (GetMonitorCount>0)
      and (FQExecuteReaderCount^>0) then
   begin
      if qry.SQLQuery <> '' then
      begin
         st := 'Query';
         st := st + ': [Execute] ' + qry.SQLQuery;
      end else
      begin
         st := qry.TableName;
         st := st + ': [Execute] ';
      end;
      st := st + Format(' [Time elapsed (ms): %d]'+CRLF, [qry.LastOperationTime]);
      WriteSQLData(st, tfQExecute);
   end;
end;

procedure TPSQLMonitorHook.SQLFetch(qry: TNativeDataset);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FQFetchReaderCount^>0) then
   begin
      if qry.SQLQuery <> '' then
      begin
         st := 'Query';
         st := st + ': [Fetch] Row # '+ IntToStr(qry.RecordNumber) + CRLF;
      end else
      begin
         st := qry.TableName;
         st := st + ': [Fetch] Row # '+ IntToStr(qry.RecordNumber) + CRLF;
      end;
      WriteSQLData(st, tfQFetch);
   end;
end;

procedure TPSQLMonitorHook.SQLPrepare(qry: TNativeDataset);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FQPrepareReaderCount^>0) then
   begin
      if qry.SQLQuery <> '' then
      begin
         st := 'Query';
         st := st + ': [Prepare] ' + qry.SQLQuery;
      end else
      begin
         st := qry.TableName;
         st := st + ': [Prepare] ';
      end;
      st := st + Format(' [Time elapsed (ms): %d]'+CRLF, [qry.LastOperationTime]);
      WriteSQLData(st, tfQPrepare);
   end;
end;

procedure TPSQLMonitorHook.TRCommit(db: TNativeConnect);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring  and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
       st := string(db.DBOptions.DatabaseName) + ': [Commit (Hard commit)]';
       WriteSQLData(st, tfTransact);
   end;
end;

procedure TPSQLMonitorHook.TRRollback(db: TNativeConnect);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
      st := string(db.DBOptions.DatabaseName) + ': [Rollback]';
      WriteSQLData(st, tfTransact);
   end;
end;

procedure TPSQLMonitorHook.TRStart(db: TNativeConnect);
var
  st: String;
begin
   if FActive and  bEnabledMonitoring and  bEnabledMonitoring and (GetMonitorCount>0)
      and (FTransactReaderCount^>0) then
   begin
      st := string(db.DBOptions.DatabaseName) + ': [Start transaction]';
      WriteSQLData(st, tfTransact);
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
      FPSQLWriterThread.WriteSQLData(' ', tfMisc);
      FPSQLReaderThread.Terminate; //added by pasha_golub 01.08.05
      FPSQLReaderThread.WaitFor;
      FPSQLReaderThread.Free;
      FPSQLReaderThread:=nil;
  end;
end;

procedure TPSQLMonitorHook.WriteSQLData(const Text: String; DataType: TPSQLTraceFlag);
var
  vText: string;
begin
   if not vEventsCreated then
   try
     CreateEvents;
   except
     Enabled := false;
     Exit;
   end;
   vText := CRLF + '[Application: ' + Application.Title + ']' + CRLF + Text; {do not localize}
   if not Assigned(FPSQLWriterThread) then
      FPSQLWriterThread := TMonitorWriterThread.Create;
   FPSQLWriterThread.WriteSQLData(vText, DataType);
end;


procedure TPSQLMonitorHook.TerminateWriteThread;
begin
   if Assigned(FPSQLWriterThread) then
   begin
     FPSQLWriterThread.Free;
     FPSQLWriterThread:=nil
   end;
end;



constructor TMonitorWriterThread.Create;
begin
   StopExec:=False;
   FMonitorMsgs := TList.Create;
   inherited Create(False);
   {$IFNDEF DELPHI_6}
   if FMonitorCount^ = 0 then
      Suspend;
   {$ENDIF}
end;

destructor TMonitorWriterThread.Destroy;
var
  Msg:TObject;
begin
   {$IFNDEF DELPHI_6}
   Resume;
   {$ENDIF}
   if FMonitorMsgs.Count>0 then
   begin
      Msg:=FMonitorMsgs[0];
      FMonitorMsgs.Delete(0);
      Msg.Free;
   end;
   FMonitorMsgs.Free;
   FMonitorMsgs := nil; //mi:2006-09-15
   inherited Destroy;
end;

procedure TMonitorWriterThread.Execute;
begin
  while (Assigned(FMonitorMsgs))
        and not StopExec do
  begin
     if (Terminated or bDone) and (FMonitorMsgs.Count = 0) then//mi:2006-09-15 removed from while condition to ensure FMonitorMsgs<>nil
        break;

     if (FMonitorCount^ = 0) then
     begin
        while FMonitorMsgs.Count <> 0 do
           FMonitorMsgs.Remove(FMonitorMsgs[0]);
        {$IFNDEF DELPHI_6}
        Suspend;
       {$ELSE}
        Sleep(50);
       {$ENDIF}
     end else
     if FMonitorMsgs.Count <> 0 then
     begin
        if (TObject(FMonitorMsgs.Items[0]) is TReleaseObject) then
           PostMessage(TReleaseObject(FMonitorMsgs.Items[0]).FHandle, CM_RELEASE, 0, 0) else
           begin
              if bEnabledMonitoring  then
                 WriteToBuffer else
                 begin
                    BeginWrite;
                    TPSQLTraceObject(FMonitorMsgs[0]).Free;
                    FMonitorMsgs.Delete(0);
                    EndWrite;
                 end;
           end;
     end else
     {$IFNDEF DELPHI_6}
     Suspend;
     {$ELSE}
     Sleep(50);
     {$ENDIF}
  end;
end;

procedure TMonitorWriterThread.Lock;
begin
   WaitForSingleObject(FWriteLock, INFINITE);
end;

procedure TMonitorWriterThread.Unlock;
begin
   ReleaseMutex(FWriteLock);
end;

procedure TMonitorWriterThread.WriteSQLData(Msg : String; DataType: TPSQLTraceFlag);
begin
   if (FMonitorCount^ <> 0)   then
   begin
      FMonitorMsgs.Add(TPSQLTraceObject.Create(Msg, DataType));
      {$IFNDEF DELPHI_6}
      Resume;
     {$ENDIF}
   end else
   begin
      FreeAndNil(FPSQLWriterThread)
   end;
end;

procedure TMonitorWriterThread.BeginWrite;
begin
   Lock;
end;

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
     if (FReaderCount^ = 0) or (InterlockedDecrement(FReaderCount^) = 0) then
        SetEvent(FReadFinishedEvent);
  ResetEvent(FWriteEvent);
  SetEvent(FWriteFinishedEvent);
  Unlock;
end;

procedure TMonitorWriterThread.WriteToBuffer;
var
  i, len: Integer;
  Text : String;
  ps   :PString;
begin
   Lock;
   try
     if FMonitorCount^ = 0 then
        FMonitorMsgs.Remove(FMonitorMsgs[0]) else
        begin
           ps    :=@TPSQLTraceObject(FMonitorMsgs[0]).FMsg;
           Text  := '';
           for i := 1 to length(ps^) do
           begin
              if ord(ps^[i]) in [0..9,$B,$C,$E..31] then
                 Text := Text + '#$'+IntToHex(ord(ps^[i]),2) else
                 Text := Text + ps^[i];
           end;
           i := 1;
           len := Length(Text);
           while (len > 0) do
           begin
              BeginWrite;
              try
                FTraceDataType^ := Integer(TPSQLTraceObject(FMonitorMsgs[0]).FDataType);
                FTimeStamp^ := TPSQLTraceObject(FMonitorMsgs[0]).FTimeStamp;
                FBufferSize^ := Min(len, cMaxBufferSize);
                Move(Text[i], FBuffer[0], FBufferSize^);
                Inc(i, cMaxBufferSize);
                Dec(len, cMaxBufferSize);
              finally
                EndWrite;
              end;
           end;
        end;
     if FMonitorMsgs.Count>0 then
     begin
        TPSQLTraceObject(FMonitorMsgs[0]).Free;
        FMonitorMsgs.Delete(0);
     end;
   finally
     Unlock;
   end;
end;


procedure TMonitorWriterThread.ReleaseMonitor(HWnd: THandle);
begin
  FMonitorMsgs.Add(TReleaseObject.Create(HWnd));
end;

{ TPSQLTraceObject }

constructor TPSQLTraceObject.Create(Msg : String; DataType: TPSQLTraceFlag);
begin
   FMsg := Msg;
   FDataType := DataType;
   FTimeStamp := Now;
end;

{TReleaseObject}

constructor TReleaseObject.Create(Handle: THandle);
begin
   FHandle := Handle;
end;

{ReaderThread}

procedure TMonitorReaderThread.AddMonitor(Arg: TPSQLCustomMonitor);
begin
   EnterCriticalSection(CS);
   if FMonitors.IndexOf(Arg) < 0 then
      FMonitors.Add(Arg);
   LeaveCriticalSection(CS);
end;

procedure TMonitorReaderThread.BeginRead;
begin
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
end;

constructor TMonitorReaderThread.Create;
begin
   inherited Create(true);
   st := TPSQLTraceObject.Create('', tfMisc);
   FMonitors := TList.Create;
   InterlockedIncrement(FMonitorCount^);
   Resume;
end;

destructor TMonitorReaderThread.Destroy;
begin
   if FMonitorCount^ > 0 then
      InterlockedDecrement(FMonitorCount^);
   FMonitors.Free;
   st.Free;
   inherited Destroy;
end;

procedure TMonitorReaderThread.EndRead;
begin
   if InterlockedDecrement(FReaderCount^) = 0 then
   begin
      ResetEvent(FReadEvent);
      SetEvent(FReadFinishedEvent);
   end;
end;

procedure TMonitorReaderThread.Execute;
var
  i : Integer;
  FTemp : TPSQLTraceObject;
begin
   while (not Terminated) and (not bDone) do
   begin
      ReadSQLData;
      if not IsBlank(st.FMsg) then
         for i := 0 to FMonitors.Count - 1 do
         begin
            FTemp := TPSQLTraceObject.Create(st.FMsg,  st.FDataType);
            PostMessage(TPSQLCustomMonitor(FMonitors[i]).Handle,
                        WM_SQL_EVENT,
                        0,
                        LPARAM(FTemp));
         end;
   end;
end;

procedure TMonitorReaderThread.ReadSQLData;
begin
   st.FMsg := '';
   BeginRead;
   if not bDone then
   try
     SetString(st.FMsg, FBuffer, FBufferSize^);
     st.FDataType := TPSQLTraceFlag(FTraceDataType^);
     st.FTimeStamp := TDateTime(FTimeStamp^);
   finally
     EndRead;
   end;
end;

procedure TMonitorReaderThread.RemoveMonitor(Arg: TPSQLCustomMonitor);
begin
   EnterCriticalSection(CS);
   FMonitors.Remove(Arg);
   LeaveCriticalSection(CS);
end;

function MonitorHook: TPSQLMonitorHook;
begin
   if (_MonitorHook = nil) and (not bDone) then
   begin
      EnterCriticalSection(CS);
      if (_MonitorHook = nil) and (not bDone) then
      begin
         _MonitorHook := TPSQLMonitorHook.Create;
      end;
      LeaveCriticalSection(CS);
  end;
  Result := _MonitorHook
end;

procedure EnableMonitoring;
begin
  bEnabledMonitoring:=true;
end;

procedure DisableMonitoring;
begin
  bEnabledMonitoring  :=false;
end;

function MonitoringEnabled: Boolean;
begin
  Result := bEnabledMonitoring;
end;


initialization
  InitializeCriticalSection(CS);
  _MonitorHook := nil;
  FPSQLWriterThread := nil;
  FPSQLReaderThread := nil;
  bDone := False;
  bEnabledMonitoring:=true;
finalization
  try
     bDone := True;
     {$IFDEF DELPHI_6}
     if Assigned(FPSQLWriterThread) then
     begin
        FPSQLWriterThread.StopExec:=True;
        FPSQLWriterThread.Terminate;
        FPSQLWriterThread.WaitFor;
     end;
     {$ENDIF}
     if FPSQLReaderThread <> nil then
        FreeAndNil(FPSQLReaderThread);
     {$IFNDEF DELPHI_6}
     if Assigned(FPSQLWriterThread) and not FPSQLWriterThread.Suspended then
        FPSQLWriterThread.Suspend;
     {$ENDIF}
     if FPSQLWriterThread <> nil then
        FreeAndNil(FPSQLWriterThread);
     if Assigned(_MonitorHook) then _MonitorHook.Free;
  finally
    _MonitorHook := nil;
    DeleteCriticalSection(CS);
  end;
end.














