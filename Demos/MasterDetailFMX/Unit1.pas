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
    DSourceTb: TDataSource;
    StrGridMsTb: TStringGrid;
    MasterTb: TPSQLTable;
    Database: TPSQLDatabase;
    ConnectBtn: TButton;
    StyleBook: TStyleBook;
    BindScDBTb: TBindScopeDB;
    BindingsList: TBindingsList;
    DBLinkStringGrid11: TBindDBGridLink;
    LbCustomerInfo: TLabel;
    BindNvMsTb: TBindNavigator;
    ExitBtn: TButton;
    StrGridDtTb: TStringGrid;
    BindNvrDtTb: TBindNavigator;
    DSourceQuery: TDataSource;
    DBLinkStrGridQuery1: TBindDBGridLink;
    GBMasterTb: TGroupBox;
    GBDetailTb: TGroupBox;
    SlideBtn: TButton;
    AniIndicator: TAniIndicator;
    PanelWait: TPanel;
    LbWait: TLabel;
    ReflectionEffect1: TReflectionEffect;
    ReflectionEffect2: TReflectionEffect;
    ReflectionEffect3: TReflectionEffect;
    ShadowEffect1: TShadowEffect;
    PanelBox: TPanel;
    BindScDBQuery: TBindScopeDB;
    DetailTb: TPSQLTable;
    FloatAnimation1: TFloatAnimation;
    FloatAnimationMs: TFloatAnimation;
    FloatAnimationDt: TFloatAnimation;
    procedure ConnectBtnClick(Sender: TObject);
    procedure MasterTbAfterScroll(DataSet: TDataSet);
    procedure ExitBtnClick(Sender: TObject);
    procedure SlideBtnClick(Sender: TObject);
    procedure ConnectBtnMouseEnter(Sender: TObject);
    procedure ConnectBtnMouseLeave(Sender: TObject);
    procedure FloatAnimationMsFinish(Sender: TObject);
    procedure FloatAnimationDtFinish(Sender: TObject);

    private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MsX,MsY,DtX,DtY:single;

implementation
    uses ConnFrmFMX;
{$R *.fmx}

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
    if Database.Connected then
    begin
       ConnectBtn.Text:= 'Connect';
       Database.Connected := False;
       FloatAnimation1.Start;
       LbCustomerInfo.Text := Format('Table "Customer": Row %d, Total %d',[0, 0]);
    end else
    begin
       if ShowConnectDlg(Database) then
       begin
          try
            PanelWait.Visible := true;
            AniIndicator.Enabled := true;
            Application.ProcessMessages;
            Database.Connected := true;
            MasterTb.Active := true;
            DetailTb.Active := true;
            PanelWait.Visible := false;
            AniIndicator.Enabled := false;
            ConnectBtn.Text := 'Disconnect';
            FloatAnimation1.Stop;
          except
            on E:Exception do
            begin
              PanelWait.Visible := false;
              AniIndicator.Enabled := false;
              Application.ShowException(E);
            end;
          end;
       end;
    end;
end;

procedure TForm1.MasterTbAfterScroll(DataSet: TDataSet);
var i:integer;
begin
 LbCustomerInfo.Text := Format('Table "Customer": Row %d, Total %d',[MasterTb.RecNo, MasterTb.RecordCount]);
 for i := 0 to StrGridMsTb.ColumnCount-1 do
    StrGridMsTb.Columns[i].Width:=100;
end;

procedure TForm1.ConnectBtnMouseEnter(Sender: TObject);
begin
   FloatAnimation1.Enabled:=false;
   ConnectBtn.Opacity:=1;
end;

procedure TForm1.ConnectBtnMouseLeave(Sender: TObject);
begin
   FloatAnimation1.Enabled:=true;
end;

procedure TForm1.ExitBtnClick(Sender: TObject);
begin
Close;
end;

procedure TForm1.FloatAnimationMsFinish(Sender: TObject);
begin
GBMasterTb.Position.X:=DtX;
GBMasterTb.Position.Y:=DtY;
if GBMasterTb.Opacity=1 then
Begin
FloatAnimationMs.StartValue:=1;
FloatAnimationMs.StopValue:=0;
FloatAnimationMs.Enabled:=false;
End
else
begin
FloatAnimationMs.StartValue:=0;
FloatAnimationMs.StopValue:=1;
FloatAnimationMs.Start;
end;
end;

procedure TForm1.FloatAnimationDtFinish(Sender: TObject);
begin
GBDetailTb.Position.X:=MsX;
GBDetailTb.Position.Y:=MsY;
if GBDetailTb.Opacity=1 then
Begin
FloatAnimationDt.StartValue:=1;
FloatAnimationDt.StopValue:=0;
FloatAnimationDt.Enabled:=false;
End
else
Begin
FloatAnimationDt.StartValue:=0;
FloatAnimationDt.StopValue:=1;
FloatAnimationDt.Start;
End;
end;

procedure TForm1.SlideBtnClick(Sender: TObject);
begin
DtX:=GBDetailTb.Position.X;
DtY:=GBDetailTb.Position.Y;
MsX:=GBMasterTb.Position.X;
MsY:=GBMasterTb.Position.Y;

FloatAnimationMs.Enabled:=true;
FloatAnimationDt.Enabled:=true;
end;

end.
