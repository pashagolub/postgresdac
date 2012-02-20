unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.DB, PSQLDbTables,
  FMX.Layouts,FMX.Grid, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.DBScope, Fmx.Bind.Editors, Data.Bind.DBLinks,
  Fmx.Bind.DBLinks, Fmx.Bind.Navigator, FMX.Effects, FMX.Filter.Effects, FMX.Ani;

type
  TForm1 = class(TForm)
    Text: TLabel;
    DSourceTb: TDataSource;
    StrGridTb: TStringGrid;
    Table: TPSQLTable;
    Database: TPSQLDatabase;
    ConnectBtn: TButton;
    StyleBook: TStyleBook;
    BindScDBTb: TBindScopeDB;
    BindingsList: TBindingsList;
    DBLinkStringGrid11: TBindDBGridLink;
    LbCountryInfo: TLabel;
    BindNvTb: TBindNavigator;
    ExitBtn: TButton;
    StrGridQuery: TStringGrid;
    BindNvrQuery: TBindNavigator;
    DSourceQuery: TDataSource;
    Query: TPSQLQuery;
    DBLinkStrGridQuery1: TBindDBGridLink;
    LbCustomerInfo: TLabel;
    GBTable: TGroupBox;
    GBQuery: TGroupBox;
    SlideBtn: TButton;
    AniIndicator: TAniIndicator;
    PanelWait: TPanel;
    LbWait: TLabel;
    ReflectionEffect1: TReflectionEffect;
    ReflectionEffect2: TReflectionEffect;
    ReflectionEffect3: TReflectionEffect;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    PanelBox: TPanel;
    BindScDBQuery: TBindScopeDB;
    procedure ConnectBtnClick(Sender: TObject);
    procedure TableAfterScroll(DataSet: TDataSet);
    procedure ExitBtnClick(Sender: TObject);
    procedure QueryAfterScroll(DataSet: TDataSet);
    procedure SlideBtnClick(Sender: TObject);
    private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
    uses ConnFrmFMX;
{$R *.fmx}

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
    if Database.Connected then
    begin
       ConnectBtn.Text:= 'Connect';
       Database.Connected := False;
       LbCountryInfo.Text := Format('Table "Country": Row %d, Total %d',[0, 0]);
       LbCustomerInfo.Text := Format('Table "Customer": Row %d, Total %d',[0, 0]);
    end else
    begin
       if ShowConnectDlg(Database) then
       begin
          try
            PanelWait.Visible:=true;
            AniIndicator.Enabled:=true;
            Application.ProcessMessages;
            Database.Connected := true;
            Table.Active := true;
            Query.Active := true;
            PanelWait.Visible:=false;
            AniIndicator.Enabled:=false;
            ConnectBtn.Text := 'Disconnect';
          except
            on E:Exception do
            begin
              PanelWait.Visible:=false;
              AniIndicator.Enabled:=false;
              Application.ShowException(E);
            end;
          end;
       end;
    end;
end;

procedure TForm1.TableAfterScroll(DataSet: TDataSet);
var i:integer;
begin
 LbCountryInfo.Text := Format('Table "Country": Row %d, Total %d',[Table.RecNo, Table.RecordCount]);
 for i := 0 to StrGridTb.ColumnCount-1 do
    StrGridTb.Columns[i].Width:=100;
end;

procedure TForm1.ExitBtnClick(Sender: TObject);
begin
Close;
end;

procedure TForm1.QueryAfterScroll(DataSet: TDataSet);
var i:integer;
begin
LbCustomerInfo.Text := Format('Table "Customer": Row %d, Total %d',[Table.RecNo, Table.RecordCount]);
for i := 0 to StrGridQuery.ColumnCount-1 do
    StrGridQuery.Columns[i].Width:=100;
end;

procedure TForm1.SlideBtnClick(Sender: TObject);
begin
GBTable.AnimateFloat('Height',27,1);
GBQuery.AnimateFloat('Height',27,1);

GBTable.AnimateFloatDelay('Position.Y',GBQuery.Position.Y,1,1);
GBQuery.AnimateFloatDelay('Position.Y',GBTable.Position.Y,1,1);

GBTable.AnimateFloatDelay('Height',209,1,2);
GBQuery.AnimateFloatDelay('Height',209,1,2);
end;

end.
