{$I PSQLDAC.inc}
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Generic SQL Property Editor                     }
{                                                       }
{       Copyright (c) 1999 Borland Software Corp.       }
{                                                       }
{*******************************************************}

unit PSQLQueryEdit;

interface

uses SysUtils, Forms, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls {$IFDEF DELPHI_14}, RTTI{$ENDIF};


type

  TGetTableNamesProc = procedure(List: TStrings; SystemTables: Boolean) of object;
  TGetFieldNamesProc = procedure(const TableName: string; List: TStrings; SystemTables: Boolean) of Object;

  TSQLEditForm = class(TForm)
    OkButton: TButton;
    HelpButton: TButton;
    CancelButton: TButton;
    AddFieldButton: TButton;
    AddTableButton: TButton;
    SQLLabel: TLabel;
    FieldListLabel: TLabel;
    TableListLabel: TLabel;
    TopPanel: TPanel;
    ButtonPanel: TPanel;
    FieldsPanel: TPanel;
    MetaInfoPanel: TPanel;
    TableListPanel: TPanel;
    TableFieldsSplitter: TSplitter;
    MetaInfoSQLSplitter: TSplitter;
    Image1: TImage;
    TableList: TListBox;
    FieldList: TListBox;
    chbSystemObjects: TCheckBox;
    SQLMemoPanel: TPanel;
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure TableFieldsSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure MetaInfoSQLSplitterCanResize(Sender: TObject;
      var NewSize: Integer; var Accept: Boolean);
    procedure MetaInfoSQLSplitterMoved(Sender: TObject);
    procedure TableListClick(Sender: TObject);
    procedure AddTableButtonClick(Sender: TObject);
    procedure AddFieldButtonClick(Sender: TObject);
    procedure SQLMemoExit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SQLMemoEnter(Sender: TObject);
    procedure chbSystemObjectsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    SynMemoUsed: boolean;
    SynMemo: TComponent;
    SynMemoLines: TStrings;
    CharHeight: Integer;
    FQuoteChar: char;
    FStartTable: string;
    GetTableNames: TGetTableNamesProc;
    GetFieldNames: TGetFieldNamesProc;
    SQLCanvas: TControlCanvas;
    SQLMemo: TMemo;
{$IFDEF DELPHI_14}
    RTTIContext: TRTTIContext;
{$ENDIF}
    procedure InsertText(AText: string; AddComma: Boolean = True);
    procedure DrawCaretPosIndicator;
    procedure PopulateTableList;
    procedure PopulateFieldList;
    procedure CreateSimpleMemo(aSQL: string);
    function GetSelStart: integer;
    procedure SetSelStart(const Value: integer);
	procedure MemoKeyPress(Sender: TObject; var Key: Char);
  protected
    property QuoteChar: char read FQuoteChar write FQuoteChar;
    property StartTable: string read FStartTable write FStartTable;
    property EditorSelStart: integer read GetSelStart write SetSelStart;
  end;

function EditSQL(var SQL: string; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''): Boolean; overload;

function EditSQL(SQL: TStrings; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''): Boolean; overload;

function DefaultReqQuoteChar( Name: string): Boolean;

implementation

{$R *.dfm}
uses PSQLTypes, Windows;

const S = '  object SQLMemoPanel: TPanel'+
'    Left = 24'+
'    Top = 16'+
'    Width = 321'+
'    Height = 241'+
'    object SynMemo1: TSynMemo'+
'      Left = 1'+
'      Top = 1'+
'      Width = 319'+
'      Height = 239'+
'      Align = alClient'+
'      Font.Charset = DEFAULT_CHARSET'+
'      Font.Color = clWindowText'+
'      Font.Height = -13'+
'      Font.Name = ''Courier New'''+
'      Font.Style = []'+
'      TabOrder = 0'+
'      Gutter.AutoSize = True'+
'      Gutter.Font.Charset = DEFAULT_CHARSET'+
'      Gutter.Font.Color = clWindowText'+
'      Gutter.Font.Height = -11'+
'      Gutter.Font.Name = ''Courier New'''+
'      Gutter.Font.Style = []'+
'      Gutter.DigitCount = 2'+
'      Gutter.ShowLineNumbers = True'+
'      Highlighter = PgSQLHighlighter'+
'      Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]'+
'      WantTabs = True'+
'      WordWrap = True'+
'    end'+
'    object PgSQLHighlighter: TSynSQLSyn'+
'      CommentAttri.Foreground = clSilver'+
'      DataTypeAttri.Foreground = clMaroon'+
'      FunctionAttri.Foreground = clNavy'+
'      KeyAttri.Foreground = clNavy'+
'      NumberAttri.Foreground = clGreen'+
'      SQLDialect = sqlPostgres'+
'      Left = 248'+
'      Top = 128'+
'    end'+
'  end';

const
  SSelect = 'SELECT'; { Do not localize }
  SFrom = 'FROM'; { Do not localize }

{$IFDEF DELPHI_14}
function StringToComponent(Value: string; Component: TComponent): TComponent;
var
  StrStream: TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;
{$ENDIF}

function EditSQL(var SQL: string; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''): Boolean; overload;
{$IFDEF DELPHI_14}
var FClass: TPersistentClass;
    T: TRTTIType;
{$ENDIF}
begin
  with TSQLEditForm.Create(nil) do
  try
{$IFDEF DELPHI_14}
    Result := False;
    FClass := GetClass('TSynMemo');
    SynMemoUsed := Assigned(FClass);
    if SynMemoUsed then
     try
      StringToComponent(S, SQLMemoPanel);
      SynMemo := SQLMemoPanel.FindComponent('SynMemo1');
      if not Assigned(SynMemo) then raise Exception.Create('SynMemo cannot be found');
      T := RTTIContext.GetType(FClass);
      SynMemoLines := T.GetProperty('Lines').GetValue(SynMemo).AsObject as TStrings;//  GetObjectProp(SynMemo, 'Lines') as TStrings;
      SynMemoLines.Text := SQL;
      T.GetProperty('OnExit').SetValue(SynMemo, TValue.From<TNotifyEvent>(SQLMemoExit));
      T.GetProperty('OnEnter').SetValue(SynMemo, TValue.From<TNotifyEvent>(SQLMemoEnter));
      TMemo(SynMemo).OnKeyPress := MemoKeyPress;
      except
        SynMemoUsed := False;
        CreateSimpleMemo(SQL);
      end
    else
{$ENDIF}
      CreateSimpleMemo(SQL);
    GetTableNames := AGetTableNames;
    GetFieldNames := AGetFieldNames;
    StartTable := AStartTblName;
    Result := ShowModal = mrOK;
    if Result then
      SQL := TrimRight(SynMemoLines.Text);
  finally
    Free;
  end;
end;

function EditSQL(SQL: TStrings; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''): Boolean; overload;
var
  SQLText: string;
begin
  SQLText := SQL.Text;
  Result := EditSQL(SQLText, AGetTableNames, AGetFieldNames, AStartTblName);
  if Result then
    SQL.Text := SQLText;
end;

procedure TSQLEditForm.FormShow(Sender: TObject);
begin
  TableList.Sorted := True;
  HelpContext := 27271; //hcDADOSQLEdit
  SQLCanvas := TControlCanvas.Create;
  if not SynMemoUsed then
    SQLCanvas.Control := SQLMemo
  else
    SQLCanvas.Control := TControl(SynMemo);
  CharHeight := SQLCanvas.TextHeight('0');
  PopulateTableList;
end;

function TSQLEditForm.GetSelStart: integer;
begin
{$IFDEF DELPHI_14}
  if SynMemoUsed then
    Result := RTTIContext.GetType(SynMemo.ClassType).GetProperty('SelStart').GetValue(SynMemo).AsInteger
  else
{$ENDIF}
    Result := SQLMemo.SelStart;
end;

procedure TSQLEditForm.FormCreate(Sender: TObject);
begin
{$IFDEF DELPHI_14}
  RTTIContext := RTTIContext.Create;
{$ENDIF}
end;

procedure TSQLEditForm.FormDestroy(Sender: TObject);
begin
  SQLCanvas.Free;
{$IFDEF DELPHI_14}
  RTTIContext.Free;
{$ENDIF}
end;

procedure TSQLEditForm.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TSQLEditForm.PopulateTableList;
var
  I: integer;
begin
  if @GetTableNames = nil then Exit;
  GetTableNames(TableList.Items, chbSystemObjects.Checked);
  if FStartTable <> '' then
    for I := 0 to TableList.Items.Count -1 do
    begin
      if AnsiCompareStr( FStartTable, TableList.Items[I] ) = 0 then
      begin
        TableList.ItemIndex := I;
        TableListClick(nil);
        break;
      end;
    end;
  if (TableList.Items.Count > 0) and (TableList.ItemIndex = -1) then
  begin
    TableList.ItemIndex := 0;
    PopulateFieldList();
  end;
end;

procedure TSQLEditForm.TableFieldsSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize > 44) and (NewSize < (MetaInfoPanel.Height - 65));
end;

procedure TSQLEditForm.MetaInfoSQLSplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := (NewSize > 100) and (NewSize < (ClientWidth - 100));
end;

procedure TSQLEditForm.MetaInfoSQLSplitterMoved(Sender: TObject);
begin
  SQLLabel.Left := SQLMemoPanel.Left;
end;

procedure TSQLEditForm.PopulateFieldList;
begin
  if @GetFieldNames = nil then Exit;
  GetFieldNames(TableList.Items[TableList.ItemIndex], FieldList.Items, chbSystemObjects.Checked);
  FieldList.Items.Insert(0, '*');
end;

procedure TSQLEditForm.TableListClick(Sender: TObject);
begin
  PopulateFieldList;
end;

procedure TSQLEditForm.InsertText(AText: string; AddComma: Boolean = True);
var
  StartSave: Integer;
  S: string;
begin
  S := SynMemoLines.Text;
  StartSave := EditorSelStart;
  if (S <> '') and (StartSave > 0) and not CharInSet(S[StartSave], [' ','(']) and
    not (AText[1] = ' ') then
  begin
    if AddComma and (S[StartSave] <> ',') then
      AText := ', ' + AText else
      AText := ' ' + AText;
  end;
  System.Insert(AText, S, StartSave + 1);
  SynMemoLines.Text := TrimRight(S);
  EditorSelStart := StartSave + Length(AText);
  if SynMemoUsed then (SynMemo as TControl).Update else SQLMemo.Update;
  DrawCaretPosIndicator;
end;

procedure TSQLEditForm.AddTableButtonClick(Sender: TObject);
begin
  if TableList.ItemIndex = -1 then Exit;
  if SynMemoLines.Text > '' then
    InsertText(TableList.Items[TableList.ItemIndex], False)
  else
    InsertText('SELECT * FROM ' + TableList.Items[TableList.ItemIndex], False);
end;

procedure TSQLEditForm.AddFieldButtonClick(Sender: TObject);
var
  I: Integer;
  ColumnName: string;
begin
  if FieldList.ItemIndex = -1 then Exit;
  if (SynMemoLines.Text = '') then
  begin
    SynMemoLines.Text := SSelect;
    EditorSelStart := Length(SSelect);
  end;
  for I := 0 to FieldList.Items.Count - 1 do
    if FieldList.Selected[I] then
    begin
      ColumnName := FieldList.Items[I];
      InsertText(ColumnName, (SynMemoLines.Text <> SSelect) and (ColumnName <> '*'));
    end;
end;

procedure TSQLEditForm.SQLMemoExit(Sender: TObject);
begin
  DrawCaretPosIndicator;
end;

procedure TSQLEditForm.SetSelStart(const Value: integer);
begin
{$IFDEF DELPHI_14}
  if SynMemoUsed then
     RTTIContext.GetType(SynMemo.ClassType).GetProperty('SelStart').SetValue(SynMemo, Value)
  else
{$ENDIF}
    SQLMemo.SelStart := Value;
end;

procedure TSQLEditForm.SQLMemoEnter(Sender: TObject);
begin
  (Sender as TControl).Invalidate; // Erase the CaretPos indicator
end;

procedure TSQLEditForm.DrawCaretPosIndicator;
var
  Pos: TPoint;
{$IFDEF DELPHI_14}
  T: TRTTIType;
  V: TValue;
  P: TRTTIProperty;
  M: TRTTIMethod;
{$ENDIF}
begin
{$IFDEF DELPHI_14}
  if SynMemoUsed then
  begin
    T := RTTIContext.GetType(SynMemo.ClassType);
    P := T.GetProperty('DisplayXY');
    M := T.GetMethod('RowColumnToPixels');
    V := P.GetValue(SynMemo);
    V := M.Invoke(SynMemo, [V]);
    Pos := V.AsType<TPoint>;
    Inc(Pos.Y, CharHeight);
  end
  else
{$ENDIF}
  begin
    Pos.Y := (SQLMemo.CaretPos.Y + 1) * CharHeight;
    Pos.X := SQLCanvas.TextWidth(Copy(SQLMemo.Lines[Pos.Y], 1, Pos.X)) - 3 ;
  end;
  SQLCanvas.Draw(Pos.X, Pos.Y, Image1.Picture.Graphic);
end;

function DefaultReqQuoteChar(Name: string): Boolean;
var
  p: PChar;
begin
  p := PChar(Name);
  Result := False;
  repeat
    if not CharInSet(p^, ['a'..'z', '0'..'9', '_']) then
    begin
      Result := True;
      break;
    end;
    Inc(p) 
  until p^ = #0;
end;

procedure TSQLEditForm.chbSystemObjectsClick(Sender: TObject);
begin
 PopulateTableList();
end;

procedure TSQLEditForm.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    TMemo(Sender).SelectAll;
    Key := #0;
  end;
end;

procedure TSQLEditForm.CreateSimpleMemo(aSQL: string);
begin
  SQLMemo := TMemo.Create(Self);
  SQLMemo.Name := 'mmSQL';
  SQLMemo.Parent := Self;
  SQLMemo.Align := alClient;
  SQLMemo.Lines.Text := aSQL;
  SQLMemo.OnEnter := SQLMemoEnter;
  SQLMemo.OnExit := SQLMemoExit;
  SQLMemo.ScrollBars := ssBoth;
  SQLMemo.OnKeyPress := MemoKeyPress;
  SynMemoLines := SQLMemo.Lines;
end;

end.
