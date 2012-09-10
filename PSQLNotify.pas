{$I PSQLDAC.inc}
unit PSQLNotify;

{SVN revision: $Id$}

interface

{$DEFINE USE_THREAD_POLLING} //coment this directive to use TTimer instead of TThread


uses {$IFNDEF USE_THREAD_POLLING}{$IFDEF FMX}FMX.Types{$ELSE}ExtCtrls{$ENDIF},{$ENDIF}
     {$IFDEF MSWINDOWS}Windows,{$ENDIF}
     Classes, PSQLTypes, PSQLAccess, PSQLDbTables;

type
  TPSQLNotify = class;

  TNotifyThread = class(TThread)
  private
    FOwner : TPSQLNotify;
    FInterval : cardinal;
    FActive : boolean;
  public
    constructor Create(Owner : TPSQLNotify);
    procedure Execute; override;
  end;

  TPSQLNotifyEvent = procedure (Sender: TObject; Event: string; ProcessID : Integer) of object;

  TPSQLNotifyEventEx = procedure (Sender: TObject; Channel: string; Payload: string; ProcessID : Integer) of object;

  TPSQLNotify = class (TComponent)
  private
    FHandle : hDBIObj;
    FActive : Boolean;
    FAutoOpen: Boolean;
    FListenList: TStrings;
{$IFDEF USE_THREAD_POLLING}
    FNotifyThread : TNotifyThread;
{$ELSE}
    FTimer: TTimer;
{$ENDIF}
    FDatabase: TPSQLDatabase;
    FBackupList: TStringList;
    FFirstConnect: Boolean;
    FNotifyFired: TPSQLNotifyEvent;
    FNotifyFiredEx: TPSQLNotifyEventEx;
    function GetStoreActive: boolean;
  protected
    procedure SetActive(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetListenList(Value: TStrings);
    procedure SetDatabase(Value: TPSQLDatabase);
    procedure ListenProc(Sender: TObject);
    procedure CheckEvents;
    procedure ListenChange(Sender: TObject);
    procedure ListenChanging(Sender: TObject);
    procedure CheckActive;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function  Engine : TPSQLEngine;
    function CreateHandle : hDBIObj;
    property Handle: hDBIObj read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenNotify;
    procedure CloseNotify;
    procedure ListenTo(Event: string);
    procedure SendNotify(Event: string); overload;
    procedure SendNotify(Channel: string; Payload: string); overload;
    procedure UnlistenTo(Event: string);
    procedure UnlistenAll;
  published
    property Database: TPSQLDatabase read FDatabase write SetDatabase;
    property Active: Boolean read FActive write SetActive stored GetStoreActive;
    property ListenList: TStrings read FListenList write SetListenList;
    property Interval: Cardinal read GetInterval write SetInterval default 250;
    property OnNotify: TPSQLNotifyEvent read FNotifyFired write FNotifyFired;
    property OnNotifyEx: TPSQLNotifyEventEx read FNotifyFiredEx write FNotifyFiredEx;
  end;


implementation

uses SysUtils, DB{$IFDEF DELPHI_15}, Diagnostics{$ELSE}, Windows{$ENDIF};

const LoopDelayStep = 50;

constructor TNotifyThread.Create(Owner: TPSQLNotify);
begin
   inherited Create(False);
   FOwner := Owner;
   FInterval := 250;
   FActive := False;
   FreeOnTerminate := True;
end;

(*
{$IFDEF DELPHI_15}
procedure TNotifyThread.Execute;
var
  StopWatch: TStopWatch;
begin
  StopWatch.Reset;
  repeat
    Self.Sleep(LoopDelayStep); //for quick reaction if termination needed
    StopWatch.Start;
    if (StopWatch.ElapsedMilliseconds > int64(FInterval)) and FActive then
    begin
       //StopWatch.Stop;
       Synchronize(FOwner.CheckEvents);
       StopWatch.Reset;
    end;
  until Terminated;
end;
{$ELSE}*)

procedure TNotifyThread.Execute;
var
  Start : cardinal;
begin
  Start := 0;
  repeat
    Sleep(LoopDelayStep); //for quick reaction if termination needed
    if (GetTickCount() - Start > FInterval) and FActive then
    begin
       Synchronize(FOwner.CheckEvents);
       Start := GetTickCount();
    end;
  until Terminated;
end;
// {$ENDIF}

constructor TPSQLNotify.Create(AOwner: TComponent);
var I: integer;
begin
  inherited Create(AOwner);
  FListenList := TStringList.Create;
  with TStringList(FListenList) do
  begin
    Duplicates := dupIgnore;
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
  FBackupList := TStringList.Create;
{$IFDEF USE_THREAD_POLLING}
  FNotifyThread := TNotifyThread.Create(self);
{$ELSE}
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  SetInterval(250);
  FTimer.OnTimer := ListenProc;
{$ENDIF}
  FActive := False;
  FFirstConnect := True;

  if (csDesigning in ComponentState) and Assigned(AOwner) then
    for I := AOwner.ComponentCount - 1 downto 0 do
      if AOwner.Components[I] is TPSQLDatabase then
      begin
         Database := AOwner.Components[I] as TPSQLDatabase;
         Break;
      end;
end;

destructor TPSQLNotify.Destroy;
begin
  CloseNotify;
  FListenList.Free;
  FBackupList.Free;
{$IFDEF USE_THREAD_POLLING}
  FNotifyThread.Terminate;
{$ELSE}
  FTimer.Free;
{$ENDIF}
  if FHandle <> nil then
     Engine.ClosePGNotify(FHandle);
  inherited Destroy;
end;

procedure TPSQLNotify.SetInterval(Value: Cardinal);
begin
{$IFDEF USE_THREAD_POLLING}
  FNotifyThread.FInterval := Value;
{$ELSE}
  FTimer.Interval := Value;
{$ENDIF}
end;

function TPSQLNotify.GetInterval;
begin
{$IFDEF USE_THREAD_POLLING}
  Result := FNotifyThread.FInterval;
{$ELSE}
  Result := FTimer.Interval;
{$ENDIF}
end;

procedure TPSQLNotify.SetListenList(Value: TStrings);
var
  I: Integer;
begin
  FListenList.Assign(Value);
  for I := 0 to FListenList.Count -1 do
    FListenList[I] := Trim(FListenList[I]);
end;

function TPSQLNotify.GetStoreActive: boolean;
begin
 Result := Active
            and Assigned(FDatabase)
            and (
              (ddoStoreConnected in FDatabase.DesignOptions)
               or not (csDesigning in ComponentState)
                );
end;

procedure TPSQLNotify.SendNotify(Channel, Payload: string);
begin
  CheckActive;
  Check(Engine, Engine.DoNotifyEx(FHandle, Channel, Payload));
end;

procedure TPSQLNotify.SetActive(Value: Boolean);
begin
  if FActive <> Value then
    if Value then
      OpenNotify
    else
      CloseNotify;
end;

procedure TPSQLNotify.SetDatabase(Value: TPSQLDatabase);
begin
  if FDatabase <> Value then
  begin
    CloseNotify;
    if FDatabase <> nil then FDatabase.RemoveNotify(Self);
    FDatabase := Value;
    if FDatabase <> nil then FDatabase.AddNotify(Self);
  end;
end;

procedure TPSQLNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase) and (Operation = opRemove) then
  begin
    CloseNotify;
    if  FDatabase <> nil then FDatabase.RemoveNotify(Self);
    FDatabase := nil;
  end;
end;

procedure TPSQLNotify.ListenChanging(Sender: TObject);
begin
  if not Active then Exit;
  FBackupList.Text := FListenList.Text;
end;

procedure TPSQLNotify.ListenChange(Sender: TObject);
var
  I: Integer;
begin
  if not Active then Exit;
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
  end;
  try
    for I := 0 to FBackupList.Count-1 do
    begin
      if FListenList.IndexOf(FBackupList[I]) = -1 then
         Check(Engine,Engine.UnlistenTo(FHandle, Trim(FBackupList[I])));
    end;
    for I := 0 to FListenList.Count-1 do
    begin
      if FBackupList.IndexOf(FListenList[I])=-1 then
         Check(Engine,Engine.ListenTo(fHandle,Trim(FListenList[I])));
    end;
  finally
    with TStringList(FListenList) do
    begin
      OnChange := ListenChange;
      OnChanging := ListenChanging;
    end;
    FBackupList.Clear;
  end;
end;

procedure TPSQLNotify.ListenProc(Sender: TObject);
begin
  if not Active then
{$IFDEF USE_THREAD_POLLING}
    FNotifyThread.fActive := False
{$ELSE}
    FTimer.Enabled := False
{$ENDIF}
  else
    CheckEvents;
end;

procedure TPSQLNotify.CheckActive;
begin
  if not Assigned(FDatabase) then DatabaseError('Property Database not set!');
  if not Active then DatabaseError('TPSQLNotify not in active mode');
end;

procedure TPSQLNotify.Loaded;
begin
  inherited Loaded;
  if FAutoOpen then
  begin
    FAutoOpen := False;
    OpenNotify;
  end;
end;

function TPSQLNotify.Engine : TPSQLEngine;
begin
   Result := FDataBase.Engine;
end;

function TPSQLNotify.CreateHandle:hDBIObj;
var
  PObj: phDBIObj;
begin
   PObj := @Result;
   Check(Engine, Engine.OpenPGNotify(FDatabase.Handle, PObj^));
end;

procedure TPSQLNotify.OpenNotify;
var
  I: Integer;
begin
  if Active then Exit;
  if not Assigned(FDatabase) and (csLoading in ComponentState) then
  begin
    FAutoOpen := True;
    Exit;
  end;
  if not Assigned(FDatabase) then DatabaseError('Property Database not set!');
  if not FDatabase.Connected then FDatabase.Open;
  if not Assigned(FHandle) then FHandle := CreateHandle;
  for I := 0 to FListenList.Count - 1 do
      Check(Engine, Engine.ListenTo(FHandle, FListenList[I]));
  FActive := True;
{$IFDEF USE_THREAD_POLLING}
  FNotifyThread.FActive := True;
{$ELSE}
  FTimer.Enabled := True;
{$ENDIF}
end;

procedure TPSQLNotify.CloseNotify;
begin
  if not Active then Exit;
  FActive := False;
{$IFDEF USE_THREAD_POLLING}
  FNotifyThread.fActive := False;
{$ELSE}
  FTimer.Enabled := False;
{$ENDIF}
  Check(Engine, Engine.UnlistenTo(FHandle, '*'));
end;

procedure TPSQLNotify.ListenTo(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.ListenTo(FHandle, Trim(Event)));
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
    if IndexOf(Event) = -1 then Append(Event);
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
end;

procedure TPSQLNotify.SendNotify(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.DoNotify(FHandle, Event));
end;

procedure TPSQLNotify.UnlistenAll;
begin
  CheckActive;
  Check(Engine, Engine.UnlistenTo(FHandle, '*'));
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
    Clear;
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
end;

procedure TPSQLNotify.UnlistenTo(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.UnlistenTo(FHandle, Trim(Event)));
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
    Delete(IndexOf(Event));
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
end;

procedure TPSQLNotify.CheckEvents;
var
  Notify, Payload : string;
  Pid    : Integer;
begin
  CheckActive;
  while True do
  begin
    Check(Engine,Engine.CheckEvents(FHandle, Pid, Notify, Payload));
    if Notify = '' then Break;
    if Assigned(FNotifyFired) then FNotifyFired(Self, Notify, Pid);
    if Assigned(FNotifyFiredEx) then FNotifyFiredEx(Self, Notify, Payload, Pid);
  end;
end;

end.
