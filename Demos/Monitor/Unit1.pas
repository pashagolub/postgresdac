unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Db, StdCtrls, ComCtrls, PSQLDbTables, PSQLMonitor, DBGrids;

type
  TForm1 = class(TForm)
    DataSource: TDataSource;
    GBQuery: TGroupBox;
    DBGrid: TDBGrid;
    bConnect: TButton;
    mSQL: TMemo;
    bOpen: TButton;
    resultLb: TLabel;
    GBMonitor: TGroupBox;
    mMonitor: TMemo;
    StatusBar: TStatusBar;
    PSQLMonitor: TPSQLMonitor;
    PSQLQuery: TPSQLQuery;
    PSQLDatabase: TPSQLDatabase;
    procedure bConnectClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PSQLDatabaseAfterConnect(Sender: TObject);
    procedure PSQLMonitorSQL(const Application, Database, Msg, SQL,
      ErrorMsg: String; DataType: TPSQLTraceFlag;
      const ExecutedOK: Boolean; EventTime: TDateTime);
    procedure PSQLDatabaseBeforeDisconnect(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
  uses ConnFrm;
{$R *.DFM}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  PSQLDatabase.Connected:=false;
end;

procedure TForm1.bConnectClick(Sender: TObject);
begin
  ShowConnectDlg(PSQLDatabase);
  PSQLDatabase.Connected:=true;
end;

procedure TForm1.bOpenClick(Sender: TObject);
begin
  if PSQLQuery.Active then
    PSQLQuery.Close();

  PSQLQuery.SQL.Assign(mSQL.Lines);
  PSQLQuery.Open();
end;

procedure TForm1.PSQLDatabaseAfterConnect(Sender: TObject);
begin
  GBQuery.Enabled := true;
  StatusBar.SimpleText := 'Successfully connected to ' + PSQLDatabase.ServerVersion;
end;

procedure TForm1.PSQLMonitorSQL(const Application, Database, Msg, SQL,
  ErrorMsg: String; DataType: TPSQLTraceFlag; const ExecutedOK: Boolean;
  EventTime: TDateTime);
begin
  mMonitor.Lines.Add(Database + ': ' + Msg + #13#10 + SQL);
end;

procedure TForm1.PSQLDatabaseBeforeDisconnect(Sender: TObject);
begin
 GBQuery.Enabled := false;
  StatusBar.SimpleText := 'Not connected';
end;

end.
