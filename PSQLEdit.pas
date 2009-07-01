
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Generic SQL Property Editor                     }
{                                                       }
{       Copyright (c) 1999 Borland Software Corp.       }
{                                                       }
{*******************************************************}

unit PSQLEdit;

interface

uses Windows, Messages, ActiveX, SysUtils, Forms, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls;


type

  TExecuteEvent = procedure of Object;

  TPopulateThread = class(TThread)
  private
    FExecuteEvent: TExecuteEvent;
  public
    constructor Create(ExecuteEvent: TExecuteEvent);
    procedure Execute; override;
  end;

  TGetTableNamesProc = procedure(List: TStrings; SystemTables: Boolean) of object;
  TGetFieldNamesProc = procedure(const TableName: string; List: TStrings; SystemTables: Boolean) of Object;
  TRequiresQuoteCharProc = function(const Name: string): Boolean of Object;

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
    SQLMemo: TMemo;
    Image1: TImage;
    TableList: TListBox;
    FieldList: TListBox;
    chbSystemObjects: TCheckBox;
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
  private
    CharHeight: Integer;
    FQuoteChar: string;
    FPopulateThread: TPopulateThread;
    FStartTable: string;
    GetTableNames: TGetTableNamesProc;
    GetFieldNames: TGetFieldNamesProc;
    SQLCanvas: TControlCanvas;
    procedure InsertText(Text: string; AddComma: Boolean = True);
    procedure DrawCaretPosIndicator;
    procedure PopulateTableList;
    procedure PopulateFieldList;
  protected
    NameRequiresQuoteChar : TRequiresQuoteCharProc;
    property QuoteChar: string read FQuoteChar write FQuoteChar;
    property StartTable: string read FStartTable write FStartTable;
  end;

function EditSQL(var SQL: string; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''; 
  AQuoteChar : string = ''; ANeedsQuoteCharFunc: 
  TRequiresQuoteCharProc = nil ): Boolean; overload;

function EditSQL(SQL: TStrings; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''; 
  AQuoteChar : string = ''; ANeedsQuoteCharFunc: 
  TRequiresQuoteCharProc = nil ): Boolean; overload;

function DefaultReqQuoteChar( Name: string): Boolean;

implementation

{$R *.dfm}

const
  SSelect = 'select'; { Do not localize }
  SFrom = 'from'; { Do not localize }

function EditSQL(var SQL: string; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''; 
  AQuoteChar : string = ''; ANeedsQuoteCharFunc: 
  TRequiresQuoteCharProc = nil ): Boolean; overload;
begin
  with TSQLEditForm.Create(nil) do
  try
    GetTableNames := AGetTableNames;
    GetFieldNames := AGetFieldNames;
    QuoteChar := AQuoteChar;
    StartTable := AStartTblName;
    NameRequiresQuoteChar := ANeedsQuoteCharFunc;
    SQLMemo.Lines.Text := SQL;
    Result := ShowModal = mrOK;
    if Result then
      SQL := SQLMemo.Lines.Text;
  finally
    Free;
  end;
end;

function EditSQL(SQL: TStrings; AGetTableNames: TGetTableNamesProc;
  AGetFieldNames: TGetFieldNamesProc; AStartTblName : string = ''; 
  AQuoteChar : string = ''; ANeedsQuoteCharFunc: 
  TRequiresQuoteCharProc = nil ): Boolean; overload;
var
  SQLText: string;
begin
  SQLText := SQL.Text;
  Result := EditSQL(SQLText, AGetTableNames, AGetFieldNames, 
            AStartTblName, AQuoteChar, ANeedsQuoteCharFunc);
  if Result then
    SQL.Text := SQLText;
end;

procedure TSQLEditForm.FormShow(Sender: TObject);
begin
  TableList.Sorted := True;
  HelpContext := 27271; //hcDADOSQLEdit
  SQLCanvas := TControlCanvas.Create;
  SQLCanvas.Control := SQLMemo;
  CharHeight := SQLCanvas.TextHeight('0');
  PopulateTableList;
//  FPopulateThread := TPopulateThread.Create(PopulateTableList);
end;

procedure TSQLEditForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FPopulateThread) then
  begin
    FPopulateThread.Terminate;
    FPopulateThread.WaitFor;
    FPopulateThread.Free;
  end;
  SQLCanvas.Free;
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
  try
    Screen.Cursor := crHourGlass;
    GetTableNames(TableList.Items, chbSystemObjects.Checked);
    Screen.Cursor := crDefault;
    if FStartTable <> '' then
    begin
      for I := 0 to TableList.Items.Count -1 do
      begin
        if AnsiCompareStr( FStartTable, TableList.Items[I] ) = 0 then
        begin
          TableList.ItemIndex := I;
          TableListClick(nil);
          break;
        end;
      end;
    end;
    if Assigned(FPopulateThread) then
      if FPopulateThread.Terminated then Exit;
    if (TableList.Items.Count > 0) and (TableList.ItemIndex = -1) then
    begin
      TableList.ItemIndex := 0;
      TableListClick(nil);
    end;
  except
    Screen.Cursor := crDefault;
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
  SQLLabel.Left := SQLMemo.Left;
end;

procedure TSQLEditForm.PopulateFieldList;
begin
  if @GetFieldNames = nil then Exit;
  try
    GetFieldNames(TableList.Items[TableList.ItemIndex], FieldList.Items, chbSystemObjects.Checked);
    FieldList.Items.Insert(0, '*');
  except
  end;
end;

procedure TSQLEditForm.TableListClick(Sender: TObject);
begin
  PopulateFieldList;
end;

procedure TSQLEditForm.InsertText(Text: string; AddComma: Boolean = True);
var
  StartSave: Integer;
  S: string;
begin
  S := SQLMemo.Text;
  StartSave := SQLMemo.SelStart;
  if (S <> '') and (StartSave > 0) and not (S[StartSave] in [' ','(']) and
    not (Text[1] = ' ') then
  begin
    if AddComma and (S[StartSave] <> ',') then
      Text := ', '+Text else
      Text := ' ' + Text;
  end;
  System.Insert(Text, S, StartSave+1);
  SQLMemo.Text := S;
  SQLMemo.SelStart := StartSave + Length(Text);
  SQLMemo.Update;
  DrawCaretPosIndicator;
end;

procedure TSQLEditForm.AddTableButtonClick(Sender: TObject);
var
  TableName,
  SQLText: string;
  NeedsQuote, Blank: Boolean;
begin
  if TableList.ItemIndex > -1 then
  begin
    SQLText := SQLMemo.Text;
    TableName := TableList.Items[TableList.ItemIndex];
    if (QuoteChar <>'') and (QuoteChar <> ' ') then
    begin
      if Assigned(NameRequiresQuoteChar) then
        NeedsQuote := NameRequiresQuoteChar(TableName)
      else
        NeedsQuote := DefaultReqQuoteChar(TableName);
      if NeedsQuote then
        TableName := QuoteChar + TableName + QuoteChar;
    end;
    Blank := SQLText = '';
    if Blank or (Copy(SQLText, 1, 6) = SSelect) then
      InsertText(Format(' %s %s', [SFrom, TableName]), False)
    else
      InsertText(TableName, False);
    if Blank then
    begin
      SQLMemo.SelStart := 0;
      SQLMemo.Update;
      InsertText(SSelect+' ', False);
    end;
  end;
end;

procedure TSQLEditForm.AddFieldButtonClick(Sender: TObject);
var
  I: Integer;
  ColumnName: string;
  NeedsQuote: Boolean;
begin
  if FieldList.ItemIndex > -1 then
  begin
    { Help the user and assume this is a select if starting with nothing }
    if SQLMemo.Text = '' then
    begin
      SQLMemo.Text := SSelect;
      SQLMemo.SelStart := Length(SQLMemo.Text);
    end;
    for I := 0 to FieldList.Items.Count - 1 do
      if FieldList.Selected[I] then
      begin
        ColumnName := FieldList.Items[I];
        if (ColumnName <> '*') and (QuoteChar <> '') and (QuoteChar <> ' ') then
        begin
          if Assigned(NameRequiresQuoteChar) then
            NeedsQuote := NameRequiresQuoteChar(ColumnName)
          else
            NeedsQuote := DefaultReqQuoteChar(ColumnName);
          if NeedsQuote then
            ColumnName := QuoteChar + ColumnName + QuoteChar;
        end;
        InsertText(ColumnName, (SQLMemo.Text <> SSelect) and (ColumnName <> '*'));
      end;
  end;
end;

procedure TSQLEditForm.SQLMemoExit(Sender: TObject);
begin
  DrawCaretPosIndicator;
end;

procedure TSQLEditForm.SQLMemoEnter(Sender: TObject);
begin
  { Erase the CaretPos indicator }
  SQLMemo.Invalidate;
end;

procedure TSQLEditForm.DrawCaretPosIndicator;
var
  XPos, YPos: Integer;
begin
  with SQLMemo.CaretPos do
  begin
    YPos := (Y+1)*CharHeight;
    XPos := SQLCanvas.TextWidth(Copy(SQLMemo.Lines[Y], 1, X)) - 3 ;
    SQLCanvas.Draw(XPos ,YPos, Image1.Picture.Graphic);
  end;
end;

{ TPopulateThread }

constructor TPopulateThread.Create(ExecuteEvent: TExecuteEvent);
begin
  FExecuteEvent := ExecuteEvent;
  inherited Create(False);
end;

procedure TPopulateThread.Execute;
begin
  CoInitialize(nil);
  try
    FExecuteEvent;
  except
  end;
  CoUninitialize;
end;

function DefaultReqQuoteChar( Name: string): Boolean;
var
  p: PChar;
begin
  p := PChar(Name);
  Result := False;
  repeat 
    if not (p^ in ['A'..'Z', '0'..'9', '_']) then
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

end.
