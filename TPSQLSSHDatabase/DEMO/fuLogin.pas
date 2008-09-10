unit fuLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ExtCtrls, ComCtrls, Spin, PgSSHDatabase, StdCtrls;

type
  TfmLogin = class(TForm)
    pcProperties: TPageControl;
    tsConnection: TTabSheet;
    laHost: TLabel;
    laUser: TLabel;
    laPassword: TLabel;
    laPort: TLabel;
    laTimeOut: TLabel;
    laDBName: TLabel;
    bvOptions: TBevel;
    edHost: TEdit;
    edUser: TEdit;
    edShowPassword: TCheckBox;
    edPassword: TEdit;
    edPort: TSpinEdit;
    edTimeOut: TSpinEdit;
    tsSSH: TTabSheet;
    laSSHHost: TLabel;
    laSSHUser: TLabel;
    laSSHPassword: TLabel;
    laSSHPort: TLabel;
    laSSHTimeout: TLabel;
    laSSHCompression: TLabel;
    edSSHUser: TEdit;
    edSSHPassword: TEdit;
    edSSHPort: TSpinEdit;
    edShowSSHPassword: TCheckBox;
    edSSHTimeout: TSpinEdit;
    chEnableSSH: TCheckBox;
    chSSHCompression: TCheckBox;
    edSSHCompression: TComboBox;
    edSSHHost: TEdit;
    buCancel: TButton;
    buOk: TButton;
    Bevel1: TBevel;
    alProfileProps: TActionList;
    aReady: TAction;
    edDBName: TEdit;
    procedure aReadyUpdate(Sender: TObject);
    procedure aReadyExecute(Sender: TObject);
    procedure edShowPasswordClick(Sender: TObject);
    procedure edShowSSHPasswordClick(Sender: TObject);
    procedure chEnableSSHClick(Sender: TObject);
    procedure chSSHCompressionClick(Sender: TObject);
  private
    { Private declarations }
    Database: TPgSSHDatabase;
    function Edit: Boolean;
  public
    { Public declarations }
    procedure GetDatabaseProperty(Db: TPgSSHDatabase);
    procedure SetDatabaseProperty(Db: TPgSSHDatabase);
  end;

function ShowConnectDlg(ADatabase: TPgSSHDatabase): Boolean;
procedure SetControlGroupEnabled(ParentControl: TWinControl; const Value: boolean;
  ForbiddenCtrls: TStrings = nil);


var
  fmLogin: TfmLogin;

implementation

{$R *.dfm}

function ShowConnectDlg(ADatabase: TPgSSHDatabase): Boolean;
begin
  with TfmLogin.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TfmLogin.Edit: Boolean;
begin
  GetDatabaseProperty(Database);
  Result := False;
  if ShowModal = mrOk then
  begin
    SetDatabaseProperty(Database);
    Result := True;
  end;
end;

procedure TfmLogin.GetDatabaseProperty(Db: TPgSSHDatabase);
begin
  //Standard
  edDBName.Text  := DB.DatabaseName;
  edUser.Text    := db.UserName;
  edPassword.Text:= db.UserPassword;
  edHost.Text    := Db.Host;
  edPort.Value   := Db.Port;
  edTimeOut.Value:= DB.ConnectionTimeout;
  //SSH
  chEnableSSH.Checked := DB.SSHEnabled;
  edSSHHost.Text      := DB.SSHHost;
  edSSHPort.Value     := DB.SSHPort;
  edSSHUser.Text      := DB.SSHLogin;
  edSSHPassword.Text  := DB.SSHPassword;
  chSSHCompression.Checked := DB.SSHUseCompression;
  edSSHCompression.Text := IntToStr(DB.SSHCompressionLevel);
  edSSHTimeout.Value    := DB.SSHTimeout;
end;

procedure TfmLogin.SetDatabaseProperty(Db: TPgSSHDatabase);
begin
  //Standard
  DB.DatabaseName := edDBName.Text;
  db.UserName     := edUser.Text;
  db.UserPassword := edPassword.Text;
  Db.Host         := edHost.Text;
  Db.Port         := edPort.Value;
  DB.ConnectionTimeout := edTimeOut.Value;
  //SSH
  DB.SSHEnabled := chEnableSSH.Checked;
  DB.SSHHost    := edSSHHost.Text;
  DB.SSHPort    := edSSHPort.Value;
  DB.SSHLogin   := edSSHUser.Text;
  DB.SSHPassword:= edSSHPassword.Text;
  DB.SSHUseCompression := chSSHCompression.Checked;
  DB.SSHCompressionLevel := StrtoInt(edSSHCompression.Text);
  DB.SSHTimeout          := edSSHTimeout.Value;
end;

procedure TfmLogin.aReadyUpdate(Sender: TObject);
begin
   (Sender as TAction).Enabled :=  (Trim(edHost.Text) <> EmptyStr) and (Trim(edUser.Text) <> EmptyStr) and (Trim(edDBName.Text) <> EmptyStr);
end;

procedure TfmLogin.aReadyExecute(Sender: TObject);
begin
   ModalResult := mrOk;
end;

procedure TfmLogin.edShowPasswordClick(Sender: TObject);
begin
   if edShowPassword.Checked then
      edPassword.PasswordChar := #0 else
      edPassword.PasswordChar := '*';
end;

procedure TfmLogin.edShowSSHPasswordClick(Sender: TObject);
begin
   if edShowSSHPassword.Checked then
      edSSHPassword.PasswordChar := #0 else
      edSShPassword.PasswordChar := '*';
end;

procedure SetControlGroupEnabled(ParentControl: TWinControl; const Value: boolean;
  ForbiddenCtrls: TStrings);
var
  i: integer;
begin
  if not Assigned(ParentControl) then Exit;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if Assigned(ForbiddenCtrls) and
      (ForbiddenCtrls.IndexOf(ParentControl.Controls[i].Name) > -1) then Continue; 
    if ParentControl.Controls[i] is TWinControl then
      SetControlGroupEnabled(TWinControl(ParentControl.Controls[i]), Value)
    else ParentControl.Controls[i].Enabled := Value;
  end;
  ParentControl.Enabled := Value;
end;

procedure TfmLogin.chEnableSSHClick(Sender: TObject);
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create();
  try
    AStrings.Add(tsSSH.Name);
    AStrings.Add(chEnableSSH.Name);
    if chEnableSSH.Checked and (not chSSHCompression.Checked) then
    begin
      AStrings.Add(laSSHCompression.Name);
      AStrings.Add(edSSHCompression.Name);
    end;
    SetControlGroupEnabled(tsSSH, chEnableSSH.Checked, AStrings);
  finally
    AStrings.Free();
  end;
end;

procedure TfmLogin.chSSHCompressionClick(Sender: TObject);
begin
   laSSHCompression.Enabled := chEnableSSH.Checked and chSSHCompression.Checked;
   edSSHCompression.Enabled := chEnableSSH.Checked and chSSHCompression.Checked;
end;

end.
