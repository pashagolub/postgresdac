unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.ListBox, PSQLDirectQuery,
  Data.DB, PSQLDbTables, FMX.Effects,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  Posix.Stdlib;
  {$ENDIF POSIX}

type
  TForm1 = class(TForm)
    LbInfo: TLabel;
    StyleBook1: TStyleBook;
    BtnConnect: TButton;
    BtnSave: TButton;
    BtnClose: TButton;
    LbStatus: TLabel;
    LbTable: TLabel;
    CbTables: TComboBox;
    PSQLDatabase: TPSQLDatabase;
    PSQLDirectQuery: TPSQLDirectQuery;
    cbNull: TCheckBox;
    SaveDialog: TSaveDialog;
    ShadowEffect1: TShadowEffect;
    ShadowEffect2: TShadowEffect;
    ShadowEffect3: TShadowEffect;
    ShadowEffect4: TShadowEffect;
    ShadowEffect5: TShadowEffect;
    ShadowEffect6: TShadowEffect;
    ShadowEffect7: TShadowEffect;
    ShadowEffect8: TShadowEffect;
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
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

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
Close();
end;

procedure TForm1.BtnConnectClick(Sender: TObject);
begin
 if not ShowConnectDlg(PSQLDatabase) then
    Exit;
  PSQLDatabase.Connected := True;
  cbTables.Clear();
  PSQLDatabase.GetTableNames('', False, cbTables.Items);

  cbTables.Items.Insert(0, '<select table>');
  cbTables.ItemIndex := 0;

  LbStatus.Text := 'Connected to ' + PSQLDatabase.ServerVersion;
  cbTables.SetFocus;
end;

procedure TForm1.BtnSaveClick(Sender: TObject);
var
  fn, s : string;
  i : integer;
  sl : TStringList; //consider using TStringList to be able to save unicode strings under Delphi 2009
begin
  if not PSQLDatabase.Connected then
  begin
    ShowMessage('Please connect to database first!');
    Exit;
  end;

  if cbTables.ItemIndex < 1 then
  begin
    ShowMessage('Please select table from drop down list!');
    exit;
  end;

  PSQLDirectQuery.SQL.Clear();
  PSQLDirectQuery.SQL.Add('SELECT * FROM ' + cbTables.Items.Strings[cbTables.ItemIndex]);
  PSQLDirectQuery.Open();

  if not SaveDialog.Execute() then
  begin
    PSQLDirectQuery.Close();
    exit;
  end;

  fn := SaveDialog.FileName;

  sl := TStringList.Create();
  try
    while not PSQLDirectQuery.Eof do
    begin
      s := EmptyStr;

      for i := 0 to PSQLDirectQuery.FieldsCount - 1 do
      begin
        if cbNull.IsChecked and PSQLDirectQuery.FieldIsNull(i) then
          s := s + '(NULL)' + #9
        else
          s := s + PSQLDirectQuery.FieldValues[i] + #9;
      end;

      sl.Add(s);

      PSQLDirectQuery.Next();
    end;

    sl.SaveToFile(fn, TEncoding.UTF8);
  finally
    sl.Free();
  end;

  PSQLDirectQuery.Close();
  if MessageDlg('Operation complete. Do you want to open created file?',
                TMsgDlgType.mtInformation, mbYesNo,0) = mrYes
  then
  begin
  {$IFDEF MSWINDOWS}
    ShellExecute(0, 'Open', PChar(fn), nil, nil, SW_SHOW);
  {$ENDIF WINDOWS}
  {$IFDEF POSIX}
    _system(PAnsiChar('open ' + AnsiString(fn)));
  {$ENDIF POSIX}
  end;
end;

end.
