unit ConnFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,PSQLDbTables;

type
  TConnectDlg = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    DBUserID: TEdit;
    DBPasswd: TEdit;
    Label3: TLabel;
    DBName: TEdit;
    Label4: TLabel;
    DBHost: TEdit;
    Label5: TLabel;
    DBPort: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
  private
    { Private declarations }
    Database: TPSQLDatabase;
    function Edit: Boolean;
  public
    { Public declarations }
    procedure GetDatabaseProperty(Db: TPSQLDatabase);
    procedure SetDatabaseProperty(Db: TPSQLDatabase);
  end;

function ShowConnectDlg(ADatabase: TPSQLDatabase): Boolean;

var
  ConnectDlg: TConnectDlg;

implementation

{$R *.DFM}

function ShowConnectDlg(ADatabase: TPSQLDatabase): Boolean;
begin
  with TConnectDlg.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TConnectDlg.Edit: Boolean;
begin
  GetDatabaseProperty(Database);
  Result := False;
  if ShowModal = mrOk then
  begin
    SetDatabaseProperty(Database);
    Result := True;
  end;
end;

procedure TConnectDlg.GetDatabaseProperty(Db: TPSQLDatabase);
begin
  DBName.Text := DB.DatabaseName;
  DBUserId.Text := db.UserName;
  DBPasswd.Text := db.UserPassword;
  DBHost.Text := Db.Host;
  DBPort.Text := IntToStr(Db.Port);
end;

procedure TConnectDlg.SetDatabaseProperty(Db: TPSQLDatabase);
begin
  DB.DatabaseName := DBName.Text;
  db.UserName := DBUserId.Text;
  db.UserPassword := DBPasswd.Text;
  Db.Host := DBHost.Text;
  Db.Port := StrToInt(DBPort.Text);
end;

end.


