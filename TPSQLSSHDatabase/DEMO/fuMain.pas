unit fuMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, DB, PSQLDbTables,
  PgSSHDatabase;

type
  TfmMain = class(TForm)
    lbTables: TListBox;
    Label1: TLabel;
    dbgData: TDBGrid;
    dbnData: TDBNavigator;
    Label2: TLabel;
    buConnect: TButton;
    buDisconnect: TButton;
    buClose: TButton;
    DataSource1: TDataSource;
    PgSSHDB: TPgSSHDatabase;
    PSQLT: TPSQLTable;
    procedure buCloseClick(Sender: TObject);
    procedure buConnectClick(Sender: TObject);
    procedure buDisconnectClick(Sender: TObject);
    procedure lbTablesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation
uses fuLogin;

{$R *.dfm}

procedure TfmMain.buCloseClick(Sender: TObject);
begin
   Close;
end;


procedure TfmMain.buConnectClick(Sender: TObject);
begin
   if ShowConnectDlg(PgSSHDB) then
   begin
      try
        PgSSHDB.Connected := true;
        Screen.Cursor := crSQLWait;
        PgSSHDB.GetTableNames('',False,lbTables.Items);
        Screen.Cursor := crDefault;
        buConnect.Enabled := False;
        buDisconnect.Enabled := True;
      except
        on E:Exception do
        begin
           Application.MessageBox(PChar(E.Message), 'Connection fault',
           MB_OK or MB_ICONINFORMATION);
        end;
      end;
   end;
end;

procedure TfmMain.buDisconnectClick(Sender: TObject);
begin
   PgSSHDB.Close;
   buConnect.Enabled := True;
   buDisConnect.Enabled := False;
   lbTables.Clear;
end;

procedure TfmMain.lbTablesClick(Sender: TObject);
begin
   if lbTables.ItemIndex = -1 then Exit;
   PSQLT.Close;
   PSQLT.TableName := lbTables.Items[lbTables.ItemIndex];
   PSQLT.Open;
end;

end.
