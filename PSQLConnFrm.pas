unit PSQLConnFrm;

interface

{SVN revision: $Id$}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, PSQLDbTables, PSQLTypes;

type
  TPSQLConnForm = class(TForm)
    Bevel1: TBevel;
    laUser: TLabel;
    laPass: TLabel;
    DBUserID: TEdit;
    DBPasswd: TEdit;
    laDbName: TLabel;
    DBName: TEdit;
    laHost: TLabel;
    DBHost: TEdit;
    laPort: TLabel;
    DBPort: TEdit;
    DBLogin: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    Panel1: TPanel;
  private
    { Private declarations }
    Database: TPSQLDatabase;
    function Edit: Boolean;
  public
    { Public declarations }
    procedure GetDatabaseProperty(Db: TPSQLDatabase);
    procedure SetDatabaseProperty(Db: TPSQLDatabase);
  end;

function EditDatabase(ADatabase: TPSQLDatabase): Boolean;

var
  PSQLConnForm: TPSQLConnForm;

implementation

{$R *.DFM}

function EditDatabase(ADatabase: TPSQLDatabase): Boolean;
begin
  with TPSQLConnForm.Create(Application) do
  try
    Database := ADatabase;
    Result := Edit;
  finally
    Free;
  end;
end;

function TPSQLConnForm.Edit: Boolean;
begin
  GetDatabaseProperty(Database);
  Result := False;
  if ShowModal = mrOk then
  begin
    SetDatabaseProperty(Database);
    Result := True;
  end;
end;

procedure TPSQLConnForm.GetDatabaseProperty(Db: TPSQLDatabase);
begin
  DBName.Text := DB.DatabaseName;
  DBUserId.Text := db.UserName;
  DBPasswd.Text := db.UserPassword;
  DBHost.Text := Db.Host;
  DBPort.Text := IntToStr(Db.Port);
  DBLogin.Checked := db.LoginPrompt;
end;

procedure TPSQLConnForm.SetDatabaseProperty(Db: TPSQLDatabase);
begin
  DB.DatabaseName := DBName.Text;
  db.UserName := DBUserId.Text;
  db.UserPassword := DBPasswd.Text;
  Db.Host := DBHost.Text;
  Db.Port := StrToIntDef(DBPort.Text, PSQLTypes.PSQL_PORT);
  db.LoginPrompt := DBLogin.Checked;
end;

end.


