unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.Edit, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components, FMX.Grid,
  Fmx.Bind.Navigator, PSQLDbTables, Data.DB, Data.Bind.DBScope,
  Fmx.Bind.Editors, Data.Bind.DBLinks, Fmx.Bind.DBLinks, FMX.Effects;

type
  TForm1 = class(TForm)
    BtnConnect: TButton;
    BtnGo: TButton;
    PanelOption: TPanel;
    rbLocate: TRadioButton;
    rbFilter: TRadioButton;
    cbPartialKey: TCheckBox;
    cbCaseinsensitive: TCheckBox;
    PanelTbQuery: TPanel;
    rbTable: TRadioButton;
    rbQuery: TRadioButton;
    StyleBook1: TStyleBook;
    PanelData: TPanel;
    cbField: TComboBox;
    cbCompare: TComboBox;
    Edit: TEdit;
    Item1: TListBoxItem;
    Item2: TListBoxItem;
    Item3: TListBoxItem;
    Item4: TListBoxItem;
    Item5: TListBoxItem;
    Item6: TListBoxItem;
    Item7: TListBoxItem;
    BindNavigator: TBindNavigator;
    StringGrid: TStringGrid;
    Database: TPSQLDatabase;
    Table: TPSQLTable;
    Query: TPSQLQuery;
    DataSource: TDataSource;
    PanelWait: TPanel;
    AniIndicator: TAniIndicator;
    LbWait: TLabel;
    BindScopeDB1: TBindScopeDB;
    BindingsList: TBindingsList;
    DBLinkStringGrid1: TBindDBGridLink;
    PanelNavig: TPanel;
    PanelStatus: TPanel;
    LbStatus: TLabel;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    ShadowEffect4: TShadowEffect;
    ShadowEffect5: TShadowEffect;
    ShadowEffect6: TShadowEffect;
    ShadowEffect7: TShadowEffect;
    ShadowEffect8: TShadowEffect;
    ShadowEffect9: TShadowEffect;
    ShadowEffect10: TShadowEffect;
    ShadowEffect11: TShadowEffect;
    ShadowEffect13: TShadowEffect;
    procedure BtnConnectClick(Sender: TObject);
    procedure QueryAfterOpen(DataSet: TDataSet);
    procedure rbTableChange(Sender: TObject);
    procedure rbLocateChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure BtnGoClick(Sender: TObject);
    procedure EditChangeTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { Private declarations }
    procedure SetControls;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses ConnFrmFMX;
{$R *.fmx}
  const FoundStatus: array[boolean] of string = (
  ' not found', ' found'
);

procedure TForm1.BtnConnectClick(Sender: TObject);
begin
 if Database.Connected then
    begin
       BtnConnect.Text:= 'Connect';
       Database.Connected := false;
       SetControls;
    end else
    begin
       if ShowConnectDlg(Database) then
       begin
          try
            PanelWait.Visible := true;
            AniIndicator.Enabled := true;
            Application.ProcessMessages;
            Database.Connected := true;
            DataSource.DataSet.Active := true;
            SetControls;
            PanelTbQuery.Enabled := true;
            PanelOption.Enabled := true;
            PanelData.Enabled := true;
            PanelWait.Visible := false;
            AniIndicator.Enabled := false;
            BtnConnect.Text := 'Disconnect';
            LbStatus.Text := Format('Connected to %s on %s',[Database.DatabaseName, Database.Host]);

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

procedure TForm1.BtnGoClick(Sender: TObject);
var
  V: Variant;
  lOpt: set of TLocateOption;
  fOpt: set of TFilterOption absolute lOpt;
begin
  V := Edit.Text;
  DataSource.Dataset.Filtered := false;
  if rbLocate.IsChecked then
  begin
    lOpt := [];
    if cbCaseInsensitive.IsChecked then Include(lOpt, loCaseInsensitive);
    if cbPartialKey.IsChecked then Include(lOpt, loPartialKey);
    LbStatus.Text := FoundStatus[
      DataSource.Dataset.Locate(cbField.Items.Strings[cbField.ItemIndex], V , lOpt) ];
  end else
  begin
    fOpt := [foNoPartialCompare];
    if cbCaseInsensitive.IsChecked then Include(fOpt, foCaseInsensitive);
    if cbPartialKey.IsChecked then Exclude(fOpt, foNoPartialCompare);
    DataSource.Dataset.FilterOptions := fOpt;
    DataSource.Dataset.Filter := Format('%s %s %s',
     [cbField.Items.Strings[cbField.ItemIndex], cbCompare.Items.Strings[cbCompare.ItemIndex], V]);
    DataSource.Dataset.Filtered := true;
    LbStatus.Text := DataSource.Dataset.Filter;
  end;
end;

procedure TForm1.EditChangeTracking(Sender: TObject);
begin
SetControls;
end;

procedure TForm1.EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if (KeyChar <> chr(13)) then exit;
  SetControls;
  if btnGo.Enabled then BtnGoClick(Sender);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
SetControls;
end;

procedure TForm1.QueryAfterOpen(DataSet: TDataSet);
var i: integer;
begin
  cbField.Clear();
  for i := 0 to DataSource.Dataset.FieldDefs.Count-1 do
    cbField.Items.Add(DataSource.Dataset.FieldDefs[i].Name);
  SetControls;
end;

procedure TForm1.rbLocateChange(Sender: TObject);
begin
SetControls;
end;

procedure TForm1.rbTableChange(Sender: TObject);
begin
  DataSource.DataSet.Close;
  if (rbTable.IsChecked) then
    DataSource.DataSet := Table
  else
    DataSource.DataSet := Query;
  if Database.Connected then
    begin
    PanelWait.Visible := true;
    AniIndicator.Enabled := true;
    Application.ProcessMessages;
    DataSource.DataSet.Open;
    PanelWait.Visible := false;
    AniIndicator.Enabled := false;
    end;
end;

procedure TForm1.SetControls;
begin
  btnGo.Enabled := DataSource.Dataset.Active and (cbField.ItemIndex > -1) and (Edit.Text <> '');
  cbCompare.Enabled  := rbFilter.IsChecked;
  if not(cbCompare.Enabled) then cbCompare.ItemIndex := 4;
  if rbLocate.IsChecked then
  begin
     DataSource.Dataset.Filtered := False;
     DataSource.Dataset.Filter := '';
  end;
end;
end.
