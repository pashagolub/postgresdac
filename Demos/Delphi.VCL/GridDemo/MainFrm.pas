unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, Db, PSQLDbTables;

type
  TForm1 = class(TForm)
    Database1: TPSQLDatabase;
    Table1: TPSQLTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    Closebtn: TButton;
    Connectbtn: TButton;
    Label1: TLabel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    procedure ConnectbtnClick(Sender: TObject);
    procedure Table1AfterScroll(DataSet: TDataSet);
    procedure ClosebtnClick(Sender: TObject);
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

procedure TForm1.ConnectbtnClick(Sender: TObject);
begin
    if Database1.Connected then
    begin
       ConnectBtn.Caption := 'Connect';
       Database1.Connected := False;
       Label1.Caption := Format('Row %d, Total %d',[0, 0]);
    end else
    begin
       if ShowConnectDlg(Database1) then
       begin
          try
            Database1.Connected := true;
            Screen.Cursor := crSQLWait;
            Table1.Active := true;
            Screen.Cursor := crDefault;
            ConnectBtn.Caption := 'Disconnect';
          except
            on E:Exception do
            begin
               Application.MessageBox(PChar(E.Message), 'FishFact connection fault',
               MB_OK or MB_ICONINFORMATION);
            end;
          end;
       end;
    end;
end;

procedure TForm1.Table1AfterScroll(DataSet: TDataSet);
begin
   Label1.Caption := Format('Row %d, Total %d',[Table1.RecNo, Table1.RecordCount])
end;

procedure TForm1.ClosebtnClick(Sender: TObject);
begin
   Close;
end;

end.
