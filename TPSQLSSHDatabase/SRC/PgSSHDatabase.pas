unit PgSSHDatabase;

interface

uses
  Classes, Windows, SysUtils, PSQLDbTables, WinSock, wodSSHTunnelLib_TLB,
  Forms;

type
  ESSHConnectError = class(Exception);

  TPgSSHDatabase = class(TPSQLDatabase)
  private
    FTunnel: TwodTunnel;

    FSSHEnabled: boolean;
    FInitPort: Cardinal;
    FInitHost: string;
    FSSHPort: integer;
    FSSHHost: string;
    FSSHLogin: string;
    FSSHPassword: string;

    FIsSSHConnecting: boolean;
    FLastSSHError: string;

    FOnTunnelConnected: TNotifyEvent;
    FOnTunnelChannelStart: TwodTunnelChannelStart;
    FOnTunnelChannelStop: TwodTunnelChannelStop;
    FOnTunnelCryptoInformation: TwodTunnelCryptoInformation;
    FOnTunnelDisconnected: TwodTunnelDisconnected;
    FOnTunnelUserConnected: TwodTunnelUserConnected;
    FOnTunnelUserDisconnected: TwodTunnelUserDisconnected;
    FSSHUseCompression: boolean;
    FSSHCompressionLevel: integer;
    FSSHTimeout: integer;
    FInitialLocalPort: integer;

    procedure DoOnTunnelUserConnecting(Sender: TObject;
      const Chan: IChannel; const Hostname: WideString; Port: Integer;
      var Allow: WordBool);
    procedure DoOnTunnelDisconnected(Sender: TObject; ErrorCode: Smallint;
     const ErrorText: WideString);
    procedure DoOnTunnelConnected(Sender: TObject);
    procedure DoOnTunnelChannelStop(Sender: TObject; const Chan: IChannel; ErrorCode: Smallint;
      const ErrorText: WideString);
    procedure DoOnTunnelChannelStart(Sender: TObject; const Chan: IChannel);
    procedure DoOnTunnelUserDisconnected(ASender: TObject;
      const Chan: IChannel; const User: IUser; ErrorCode: Smallint;
      const ErrorText: WideString);

    procedure RemoveSSHConnection;
    function CreateSSHTunnel: TwodTunnel;
    procedure SetInitialLocalPort(const Value: integer);
  protected
    procedure DoConnect; override;
//    procedure DoDisconnect; override;
    procedure SetConnected(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property SSHEnabled: boolean read FSSHEnabled write FSSHEnabled default False;
    property SSHHost: string read FSSHHost write FSSHHost;
    property SSHLogin: string read FSSHLogin write FSSHLogin;
    property SSHPassword: string read FSSHPassword write FSSHPassword;
    property SSHPort: integer read FSSHPort write FSSHPort default 22;
    property SSHTimeout: integer read FSSHTimeout write FSSHTimeout;
    property SSHUseCompression: boolean read FSSHUseCompression write FSSHUseCompression;
    property SSHCompressionLevel: integer read FSSHCompressionLevel write FSSHCompressionLevel;
    property InitialLocalPort: integer read FInitialLocalPort write SetInitialLocalPort default 3380;

    property OnTunnelConnected: TNotifyEvent read FOnTunnelConnected
      write FOnTunnelConnected;
    property OnTunnelDisconnected: TwodTunnelDisconnected read FOnTunnelDisconnected
      write FOnTunnelDisconnected;

    property OnTunnelChannelStart: TwodTunnelChannelStart read FOnTunnelChannelStart
      write FOnTunnelChannelStart;
    property OnTunnelChannelStop: TwodTunnelChannelStop read FOnTunnelChannelStop
      write FOnTunnelChannelStop;

    property OnTunnelUserConnected: TwodTunnelUserConnected read FOnTunnelUserConnected
      write FOnTunnelUserConnected;
    property OnTunnelUserDisconnected: TwodTunnelUserDisconnected read FOnTunnelUserDisconnected
      write FOnTunnelUserDisconnected;
    property OnTunnelCryptoInformation: TwodTunnelCryptoInformation read FOnTunnelCryptoInformation
      write FOnTunnelCryptoInformation;
  end;

  procedure Register;

implementation

{$R SSHDB.DCR}


var
  TunnelList: TList;
  InitLocalPort: integer;

procedure Register;
begin
  RegisterComponents('PostgresDAC',[TPgSSHDatabase] );
end;

{ TPgSSHDatabase }

constructor TPgSSHDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FTunnel := nil;
  FSSHEnabled := False;
  FSSHTimeout := 120;
  FSSHUseCompression := False;
  FSSHCompressionLevel := 6;
  FInitialLocalPort := 3380;

  CheckIfActiveOnParamChange := False;
end;

destructor TPgSSHDatabase.Destroy;
begin
  RemoveSSHConnection();
  inherited;
end;

procedure TPgSSHDatabase.RemoveSSHConnection;
begin
  if Assigned(FTunnel) then begin
    FTunnel.Tag := FTunnel.Tag - 1;
    if FTunnel.Tag = 0 then begin
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
  else Result.Compression := 0;
  Result.Threads := True;

  Inc(InitLocalPort);
  Result.Channels.Add(LocalListen, '0.0.0.0', InitLocalPort, Host, FInitPort);

  FLastSSHError := EmptyStr;
  FIsSSHConnecting := True;
  Result.Connect();

  while FIsSSHConnecting do
    Application.ProcessMessages();

  if not ((Result.State = wodSSHTunnelLib_TLB.Connected) and (Result.Channels.Count > 0) and
     Result.Channels[0].Activated) then begin
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
    if FSSHEnabled then begin
      ATunnel := nil;
      for i := 0 to TunnelList.Count - 1 do begin
        ATunnel := TwodTunnel(TunnelList[i]);
        if (ATunnel.Hostname = FSSHHost) and
           (ATunnel.Port = SSHPort) and
           (ATunnel.Password = SSHPassword) and
           (ATunnel.Login = SSHLogin) and
           (ATunnel.Channels.Count > 0) and
           (ATunnel.Channels[0].RemotePort = Integer(Port)) then Break
        else ATunnel := nil;
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

procedure TPgSSHDatabase.SetConnected(Value: Boolean);
begin
  inherited;
  if (not (csDestroying in ComponentState)) and (not Value) then
    RemoveSSHConnection();
end;

procedure TPgSSHDatabase.DoOnTunnelUserConnecting(Sender: TObject;
  const Chan: IChannel; const Hostname: WideString; Port: Integer;
  var Allow: WordBool);
begin
  Allow := True;
end;

procedure TPgSSHDatabase.DoOnTunnelConnected(Sender: TObject);
begin
  try
  TwodTunnel(Sender).Channels.StartAll();
  except
    Exit;
  end;

  if Assigned(FOnTunnelConnected) then 
    FOnTunnelConnected(Sender);
end;

procedure TPgSSHDatabase.DoOnTunnelDisconnected(Sender: TObject;
  ErrorCode: Smallint; const ErrorText: WideString);
begin
  FIsSSHConnecting := False;
  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else FLastSSHError := 'Connection broke unexpectedly';
  if Assigned(FOnTunnelDisconnected) then FOnTunnelDisconnected(Sender, ErrorCode,
    ErrorText);
end;

procedure TPgSSHDatabase.DoOnTunnelChannelStart(Sender: TObject;
  const Chan: IChannel);
begin
  FIsSSHConnecting := False;
end;

procedure TPgSSHDatabase.DoOnTunnelChannelStop(Sender: TObject;
  const Chan: IChannel; ErrorCode: Smallint; const ErrorText: WideString);
begin
  FIsSSHConnecting := False;
  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else FLastSSHError := 'Channel stopped unexpectedly';
end;

procedure TPgSSHDatabase.DoOnTunnelUserDisconnected(ASender: TObject;
  const Chan: IChannel; const User: IUser; ErrorCode: Smallint;
  const ErrorText: WideString);
begin
  if ErrorText <> '' then
    FLastSSHError := ErrorText
  else FLastSSHError := 'User disconnected unexpectedly';
  if Assigned(FOnTunnelUserDisconnected) then
    FOnTunnelUserDisconnected(ASender, Chan, User, ErrorCode, ErrorText);
end;

procedure TPgSSHDatabase.SetInitialLocalPort(const Value: integer);
begin
  FInitialLocalPort := Value;
  if InitLocalPort = 3380 then InitLocalPort := Value;
end;

initialization
  TunnelList := TList.Create();
  InitLocalPort := 3380;

finalization

  TunnelList.Free();

end.

