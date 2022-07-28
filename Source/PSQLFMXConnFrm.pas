unit PSQLFMXConnFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, PSQLDbTables, PSQLTypes;

type
  TPSQLFmxConnForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    laDBName: TLabel;
    lbUser: TLabel;
    laPass: TLabel;
    laHost: TLabel;
    laPort: TLabel;
    DBName: TEdit;
    DBPasswd: TEdit;
    DBHost: TEdit;
    DBUserID: TEdit;
    DBPort: TEdit;
    DBLogin: TCheckBox;
    OKBtn: TButton;
    CancelBtn: TButton;
  public
    procedure GetDatabaseProperty(Db: TPSQLDatabase);
    procedure SetDatabaseProperty(Db: TPSQLDatabase);
  end;

var
  PSQLFmxConnForm: TPSQLFmxConnForm;

implementation

{$R *.fmx}

procedure TPSQLFmxConnForm.GetDatabaseProperty(Db: TPSQLDatabase);
begin
  DBName.Text := DB.DatabaseName;
  DBUserId.Text := db.UserName;
  DBPasswd.Text := db.UserPassword;
  DBHost.Text := Db.Host;
  DBPort.Text := IntToStr(Db.Port);
  DBLogin.IsChecked := db.LoginPrompt;
end;

procedure TPSQLFmxConnForm.SetDatabaseProperty(Db: TPSQLDatabase);
begin
  DB.DatabaseName := DBName.Text;
  db.UserName := DBUserId.Text;
  db.UserPassword := DBPasswd.Text;
  Db.Host := DBHost.Text;
  Db.Port := StrToIntDef(DBPort.Text, PSQLTypes.PSQL_PORT);
  db.LoginPrompt := DBLogin.IsChecked;
end;

end.
