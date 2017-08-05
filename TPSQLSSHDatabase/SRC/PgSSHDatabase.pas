{$I PSQLDAC.inc}
unit PgSSHDatabase;

// One should uncomment only one directive
{$DEFINE USE_SSH_PLINK} 	// this allows to connect via plink.exe
{ .$DEFINE USE_SSH_WEONLYDO } // this allows to connect via WeOnlyDo

interface

uses
  Classes, Windows, SysUtils,
  {$IFDEF USE_SSH_WEONLYDO}wodSSHTunnelLib_TLB, {$ENDIF}
  PSQLDbTables, PSQLTypes;

type
  ESSHConnectError = class(Exception);

  TPgSSHDatabase = class(TPSQLDatabase)
  private
    FSSHEnabled: boolean;
    FInitPort: Cardinal;
    FInitHost: string;
    FSSHPort: integer;
    FSSHHost: string;
    FSSHLogin: string;
    FSSHPassword: string;
    FOnTunnelConnected: TNotifyEvent;
{$IFDEF USE_SSH_WEONLYDO}
    FTunnel: TwodTunnel;
    FIsSSHConnecting: boolean;
    FLastSSHError: string;
    FOnTunnelDisconnected: TwodTunnelDisconnected;
    FOnTunnelChannelStart: TwodTunnelChannelStart;
    FOnTunnelChannelStop: TwodTunnelChannelStop;
    FOnTunnelCryptoInformation: TwodTunnelCryptoInformation;
    FOnTunnelUserConnected: TwodTunnelUserConnected;
    FOnTunnelUserDisconnected: TwodTunnelUserDisconnected;
{$ELSE} // USE_SSH_PLINK defined
    FPLinkPath: String;
    FPlinkProcInfo: TProcessInformation;
{$ENDIF}
    FSSHUseCompression: boolean;
    FSSHCompressionLevel: integer;
    FSSHTimeout: integer;
    FInitialLocalPort: integer;
    FShowPLinkConsole: boolean;
    FAddSSHKeyToSystemCache: boolean;
{$IFDEF USE_SSH_WEONLYDO}
    procedure DoOnTunnelUserConnecting(Sender: TObject; const Chan: IChannel; const Hostname: WideString; Port: integer;
      var Allow: WordBool);
    procedure DoOnTunnelDisconnected(Sender: TObject; ErrorCode: Smallint; const ErrorText: WideString);
    procedure DoOnTunnelConnected(Sender: TObject);
    procedure DoOnTunnelChannelStop(Sender: TObject; const Chan: IChannel; ErrorCode: Smallint;
      const ErrorText: WideString);
    procedure DoOnTunnelChannelStart(Sender: TObject; const Chan: IChannel);
    procedure DoOnTunnelUserDisconnected(ASender: TObject; const Chan: IChannel; const User: IUser; ErrorCode: Smallint;
      const ErrorText: WideString);
    function CreateSSHTunnel: TwodTunnel;
{$ELSE} // USE_SSH_PLINK defined
    procedure ShutdownPlink;
{$IFEND}
    procedure RemoveSSHConnection;
    procedure SetInitialLocalPort(const Value: integer);
  protected
    procedure DoConnect; override;
    procedure SetConnected(Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

  published
{$IFDEF USE_SSH_PLINK}
    property AddSSHKeyToSystemCache: boolean read FAddSSHKeyToSystemCache write FAddSSHKeyToSystemCache;
    property PathPLinkExe: String read FPLinkPath write FPLinkPath;
    property ShowPLinkConsole: boolean read FShowPLinkConsole write FShowPLinkConsole default False;
{$ENDIF}
    property SSHEnabled: boolean read FSSHEnabled write FSSHEnabled default False;
    property SSHHost: string read FSSHHost write FSSHHost;
    property SSHLogin: string read FSSHLogin write FSSHLogin;
    property SSHPassword: string read FSSHPassword write FSSHPassword;
    property SSHPort: integer read FSSHPort write FSSHPort default 22;
    property SSHTimeout: integer read FSSHTimeout write FSSHTimeout;
    property SSHUseCompression: boolean read FSSHUseCompression write FSSHUseCompression;
    property SSHCompressionLevel: integer read FSSHCompressionLevel write FSSHCompressionLevel;
    property InitialLocalPort: integer read FInitialLocalPort write SetInitialLocalPort default 3380;
    property OnTunnelConnected: TNotifyEvent read FOnTunnelConnected write FOnTunnelConnected;
{$IFDEF USE_SSH_WEONLYDO}
    property OnTunnelDisconnected: TwodTunnelDisconnected read FOnTunnelDisconnected write FOnTunnelDisconnected;
    property OnTunnelChannelStart: TwodTunnelChannelStart read FOnTunnelChannelStart write FOnTunnelChannelStart;
    property OnTunnelChannelStop: TwodTunnelChannelStop read FOnTunnelChannelStop write FOnTunnelChannelStop;
    property OnTunnelUserConnected: TwodTunnelUserConnected read FOnTunnelUserConnected write FOnTunnelUserConnected;
    property OnTunnelUserDisconnected: TwodTunnelUserDisconnected read FOnTunnelUserDisconnected
      write FOnTunnelUserDisconnected;
    property OnTunnelCryptoInformation: TwodTunnelCryptoInformation read FOnTunnelCryptoInformation
      write FOnTunnelCryptoInformation;
{$ENDIF}
  end;

  procedure Register;

implementation

uses ShellAPI;

{$R SSHDB.DCR}

{$IFDEF USE_SSH_WEONLYDO}
var
  TunnelList: TList;
  InitLocalPort: integer;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('PostgresDAC',[TPgSSHDatabase] );
end;

{ TPgSSHDatabase }

constructor TPgSSHDatabase.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF USE_SSH_WEONLYDO}
  FTunnel := nil;
{$ENDIF}
  FSSHEnabled := False;
  FSSHTimeout := 120;
  FSSHUseCompression := False;
  FSSHCompressionLevel := 6;
  FSSHPort := 22;
  FInitialLocalPort := 3380;
  CheckIfActiveOnParamChange := False;
  PathPLinkExe := 'plink' {$IFDEF MSWINDOWS} + '.exe' {$ENDIF};
end;

destructor TPgSSHDatabase.Destroy;
begin
  RemoveSSHConnection();
  inherited;
end;

{$IFDEF USE_SSH_WEONLYDO}
procedure TPgSSHDatabase.RemoveSSHConnection;
begin
  if Assigned(FTunnel) then
  begin
    FTunnel.Tag := FTunnel.Tag - 1;
    if FTunnel.Tag = 0 then
    begin
      TunnelList.Remove(FTunnel);
      FTunnel.Channels.StopAll();
      FTunnel.Channels.RemoveAll();
      Dec(InitLocalPort);
      FTunnel.Disconnect();
      FTunnel.Free();
      FTunnel := nil;
    end;
  end;
end;
{$ELSE}
procedure TPgSSHDatabase.RemoveSSHConnection;
begin
  ShutdownPlink;
end;
{$ENDIF}

{$IFDEF USE_SSH_WEONLYDO}
function TPgSSHDatabase.CreateSSHTunnel: TwodTunnel;
var
  AErrorMsg: string;
begin
  Result := TwodTunnel.Create(nil);
  Result.Encryption := encAny;
  Result.Authentication := authPassword;
  Result.Protocol := SSHAuto;
  Result.OnConnected := DoOnTunnelConnected;
  Result.OnDisconnected := DoOnTunnelDisconnected;
  Result.OnUserConnecting := DoOnTunnelUserConnecting;
  Result.OnChannelStop := DoOnTunnelChannelStop;
  Result.OnChannelStart := DoOnTunnelChannelStart;
  Result.OnCryptoInformation := FOnTunnelCryptoInformation;
  Result.OnUserConnected := FOnTunnelUserConnected;
  Result.OnUserDisconnected := DoOnTunnelUserDisconnected;
  Result.Hostname := FSSHHost;
  Result.Port := FSSHPort;
  Result.Login := FSSHLogin;
  Result.Password := FSSHPassword;
  Result.Timeout := FSSHTimeout;
  if FSSHUseCompression then
    Result.Compression := FSSHCompressionLevel
  else
    Result.Compression := 0;
  Result.Threads := True;
  Inc(InitLocalPort);
  Result.Channels.Add(LocalListen, '0.0.0.0', InitLocalPort, Host, FInitPort);
  FLastSSHError := EmptyStr;
  FIsSSHConnecting := True;
  Result.Connect();
  while FIsSSHConnecting do
    Application.ProcessMessages();
  if not((Result.State = wodSSHTunnelLib_TLB.Connected) and (Result.Channels.Count > 0) and Result.Channels[0].Activated)
  then
  begin
    TunnelList.Remove(Result);
    Result.Free();
    AErrorMsg := 'SSH Connection Failed';
    if FLastSSHError <> EmptyStr then
      AErrorMsg := Format('%s with Error: %s', [AErrorMsg, FLastSSHError]);
    raise ESSHConnectError.Create(AErrorMsg);
  end;
end;

procedure TPgSSHDatabase.DoConnect;
var
  i: integer;
  ATunnel: TwodTunnel;
begin
  FInitPort := Port;
  FInitHost := Host;
  try
    if FSSHEnabled then
    begin
      ATunnel := nil;
      for i := 0 to TunnelList.Count - 1 do
      begin
        ATunnel := TwodTunnel(TunnelList[i]);
        if (ATunnel.Hostname = FSSHHost) and (ATunnel.Port = SSHPort) and (ATunnel.Password = SSHPassword) and
          (ATunnel.Login = SSHLogin) and (ATunnel.Channels.Count > 0) and
          (ATunnel.Channels[0].RemotePort = integer(Port)) then
          Break
        else
          ATunnel := nil;
      end;
      if ATunnel = nil then
        ATunnel := CreateSSHTunnel();
      if ATunnel = nil then
        raise ESSHConnectError.Create('SSH Connection Failed');
      ATunnel.Tag := ATunnel.Tag + 1;
      FTunnel := ATunnel;
      Host := 'localhost';
      Port := ATunnel.Channels[0].LocalPort;
    end;
    inherited DoConnect();
  finally
    Port := FInitPort;
    Host := FInitHost;
  end;
end;
{$ELSE}
procedure TPgSSHDatabase.DoConnect;
var
  ConnectStr: String;
  StartupInfo: TStartupInfo;
  ExCode: LongWord;
  StartFlags: cardinal;

  procedure StartTunnelAndAcceptKey;
  var ShExecInfo: TShellExecuteInfo;
  begin
    FillChar(ShExecInfo, SizeOf(ShExecInfo), 0);
    ShExecInfo.cbSize := SizeOf(TShellExecuteInfo);
    ShExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
    ShExecInfo.Wnd := 0;
    ShExecInfo.lpVerb := nil;
    ShExecInfo.lpFile := 'cmd.exe';
    ShExecInfo.lpParameters := PChar(Format('/c echo y | %s -t exit', [ConnectStr]));
    ShExecInfo.lpDirectory := nil;
    ShExecInfo.nShow := SW_HIDE;
    ShExecInfo.hInstApp := 0;
    ShellExecuteEx(@ShExecInfo);
    WaitForSingleObject(ShExecInfo.hProcess, INFINITE);
  end;

begin
  FInitPort := Port;
  FInitHost := Host;
  StartFlags :=  CREATE_DEFAULT_ERROR_MODE + NORMAL_PRIORITY_CLASS;
  if not FShowPLinkConsole then
    StartFlags := StartFlags + CREATE_NO_WINDOW;
  try
    if FSSHEnabled then
    begin
      if not FileExists(FPLinkPath) then
        raise ESSHConnectError.Create('Please specify path to PLink.exe!');
      ConnectStr := AnsiQuotedStr(FPLinkPath, '"') + ' -ssh -batch ';
      if SSHLogin <> '' then
        ConnectStr := ConnectStr + SSHLogin + '@';
      if SSHHost <> '' then
        ConnectStr := ConnectStr + SSHHost
      else
        ConnectStr := ConnectStr + Host;
      if SSHPassword <> '' then
        ConnectStr := ConnectStr + ' -pw ' + AnsiQuotedStr(SSHPassword, '"');
      if SSHPort > 0 then
        ConnectStr := ConnectStr + Format(' -P %u', [SSHPort]);
      if FAddSSHKeyToSystemCache then
        StartTunnelAndAcceptKey();
      ConnectStr := ConnectStr + Format(' -v -T -N -L %u:%s:%u', [InitialLocalPort, Host, Port]);
      FillChar(FPlinkProcInfo, SizeOf(TProcessInformation), 0);
      FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
      StartupInfo.cb := SizeOf(TStartupInfo);
      if CreateProcess(nil, PChar(ConnectStr), nil, nil, False, StartFlags, nil, nil, StartupInfo, FPlinkProcInfo) then
      begin
        WaitForSingleObject(FPlinkProcInfo.hProcess, SSHTimeout);
        GetExitCodeProcess(FPlinkProcInfo.hProcess, ExCode);
        if ExCode <> STILL_ACTIVE then
          raise ESSHConnectError.CreateFmt('Failed to connect. Check the settings for PLink. Exit code: %d', [ExCode]);
      end
      else
      begin
        ShutdownPlink;
        RaiseLastOSError();
      end;
      if Assigned(FOnTunnelConnected) then
        FOnTunnelConnected(Self);
      Host := 'localhost';
      Port := InitialLocalPort;
    end;
    try
      inherited DoConnect();
    except
      ShutdownPlink;
      raise;
    end;
  finally
    Port := FInitPort;
    Host := FInitHost;
  end;
end;
{$ENDIF}

procedure TPgSSHDatabase.SetConnected(Value: Boolean);
begin
  inherited;
  if (not (csDestroying in ComponentState)) and (not Value) then
    RemoveSSHConnection();
end;

{$IFDEF USE_SSH_WEONLYDO}
procedure TPgSSHDatabase.DoOnTunnelUserConnecting(Sender: TObject; const Chan: IChannel; const Hostname: WideString;
  Port: integer; var Allow: WordBool);
begin
  Allow := True;
end;

procedure TPgSSHDatabase.DoOnTunnelConnected(Sender: TObject);
begin
  try
    TwodTunnel(Sender).Channels.StartAll();
  except
  	FIsSSHConnecting := False;
    Exit;
  end;

  if Assigned(FOnTunnelConnected) then 
    FOnTunnelConnected(Sender);
end;

procedure TPgSSHDatabase.DoOnTunnelDisconnected(Sender: TObject; ErrorCode: Smallint; const ErrorText: WideString);
begin
  FIsSSHConnecting := False;

  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else
    FLastSSHError := 'Connection broke unexpectedly';

  if Assigned(FOnTunnelDisconnected) then
    FOnTunnelDisconnected(Sender, ErrorCode, ErrorText);
end;

procedure TPgSSHDatabase.DoOnTunnelChannelStart(Sender: TObject; const Chan: IChannel);
begin
  FIsSSHConnecting := False;
end;

procedure TPgSSHDatabase.DoOnTunnelChannelStop(Sender: TObject; const Chan: IChannel; ErrorCode: Smallint;
  const ErrorText: WideString);
begin
  FIsSSHConnecting := False;

  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else
    FLastSSHError := 'Channel stopped unexpectedly';
end;

procedure TPgSSHDatabase.DoOnTunnelUserDisconnected(ASender: TObject; const Chan: IChannel; const User: IUser;
  ErrorCode: Smallint; const ErrorText: WideString);
begin
  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else
    FLastSSHError := 'User disconnected unexpectedly';
  if Assigned(FOnTunnelUserDisconnected) then
    FOnTunnelUserDisconnected(ASender, Chan, User, ErrorCode, ErrorText);
end;

{$ELSE}

procedure TPgSSHDatabase.ShutdownPlink;
begin
  if FPlinkProcInfo.hProcess > 0 then
  begin
    TerminateProcess(FPlinkProcInfo.hProcess, 0);
    CloseHandle(FPlinkProcInfo.hProcess);
    ZeroMemory(@FPlinkProcInfo, SizeOf(FPlinkProcInfo));
  end;
end;

{$ENDIF}

procedure TPgSSHDatabase.SetInitialLocalPort(const Value: integer);
begin
  FInitialLocalPort := Value;
{$IFDEF USE_SSH_WEONLYDO}
  if InitLocalPort = 3380 then
    InitLocalPort := Value;
{$ENDIF}
end;

{$IFDEF USE_SSH_WEONLYDO}

initialization

  TunnelList := TList.Create();
  InitLocalPort := 3380;

finalization

  TunnelList.Free();

{$ENDIF}

end.
