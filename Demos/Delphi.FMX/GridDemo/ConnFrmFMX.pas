unit ConnFrmFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit,PSQLDbTables, FMX.StdCtrls;

type
  TPConnDlgFMX = class(TForm)
    Lb1: TLabel;
    Lb2: TLabel;
    Lb3: TLabel;
    Lb4: TLabel;
    Lb5: TLabel;
    DBName: TEdit;
    DBUserID: TEdit;
    DBPasswd: TEdit;
    DBHost: TEdit;
    DBPort: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    StyleBook1: TStyleBook;
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
  PConnDlgFMX: TPConnDlgFMX;

implementation

{$R *.fmx}

function ShowConnectDlg(ADatabase: TPSQLDatabase): Boolean;
begin
  with TPConnDlgFMX.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TPConnDlgFMX.Edit: Boolean;
begin
  GetDatabaseProperty(Database);
  Result := False;
  if ShowModal = mrOk then
  begin
    SetDatabaseProperty(Database);
    Result := True;
  end;
end;

procedure TPConnDlgFMX.GetDatabaseProperty(Db: TPSQLDatabase);
begin
  DBName.Text := DB.DatabaseName;
  DBUserId.Text := db.UserName;
  DBPasswd.Text := db.UserPassword;
  DBHost.Text := Db.Host;
  DBPort.Text := IntToStr(Db.Port);
end;

procedure TPConnDlgFMX.SetDatabaseProperty(Db: TPSQLDatabase);
begin
  DB.DatabaseName := DBName.Text;
  db.UserName := DBUserId.Text;
  db.UserPassword := DBPasswd.Text;
  Db.Host := DBHost.Text;
  Db.Port := StrToInt(DBPort.Text);
end;

end.
