{$I Psqldac.inc}
unit PSQLMigrator;

{SVN revision: $Id$}

interface
uses Classes, DB, PSQLDBTables, PSQLMacroQuery, Forms, SysUtils, PSQLTypes;

type

  PDataSetPair = ^TDataSetPair;
  TDataSetPair = record
    OldDataSet,
    NewDataSet: TDataSet;
  end;

  TConvertComponent = (convBDE, convADO, convDBX, convZeos, convMySQLDAC);
  TConvertComponents = set of TConvertComponent;


  TDataSetList = class(TList)
  private
    function GetDataSetPairs(Index: Integer): PDataSetPair;
    procedure SetDataSetPairs(Index: Integer; Item: PDataSetPair);
    function GetOldDataSets(Index: Integer): TDataSet;
    procedure SetOldDataSets(Index: Integer; Item: TDataSet);
    function GetNewDataSets(Index: Integer): TDataSet;
    procedure SetNewDataSets(Index: Integer; Item: TDataSet);
  public
    function GetPaired(aDataSet: TDataSet): TDataSet;
    function IndexOfOldDataSet(aDataSet: TDataSet): Integer;
    procedure DeletePair(Index: Integer);
    procedure ClearAll;
    destructor Destroy; override;
    property Pairs[Index: Integer]: PDataSetPair read GetDataSetPairs write SetDataSetPairs;
    property OldDataSets[Index: Integer]: TDataSet read GetOldDataSets write SetOldDataSets;
    property NewDataSets[Index: Integer]: TDataSet read GetNewDataSets write SetNewDataSets;
  end;

  TBDE2PSQLDAC = class(TComponent)
  private
    FAbout   : TPSQLDACAbout;
    FPSQLDatabase: TPSQLDatabase;
    FDataSets: TDataSetList;
    FFields: TList;
    FExecute: Boolean;
    FDeleteSourceComponents: Boolean;
    FConvertComponents: TConvertComponents;
    FSourceConnection: TCustomConnection;
    function GetDatabase: TPSQLDatabase;
    procedure SetDatabase(Value: TPSQLDatabase);
    procedure FillComponents(ACompList: TList; ACompClass: TClass);
    procedure SetDeleteSourceComponents(const Value: Boolean);
    procedure SetConvertComponents(const Value: TConvertComponents);
    procedure SetSourceConnection(const Value: TCustomConnection);
  protected
    { Common methods }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MigrateTables;
    procedure MigrateDataSources;
    procedure MigrateLookupFields;
    procedure UpdateDesignInfo(OldDataSet, NewComponent: TComponent);
    procedure AssignEvents(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure AssignFields(OldField, NewField: TField);
    procedure NewDataSetPair(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure MoveFields(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure CreatePSQLDataSet(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure CheckDataSet(OldDataSet: TDataSet);
    procedure GetNeededSQLs(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure GetCachedUpdates(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
    procedure GetNeededUpdateObject(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
  public
    procedure Migrate; virtual;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property Database: TPSQLDatabase read GetDatabase write SetDatabase;
    property SourceConnection: TCustomConnection read FSourceConnection write SetSourceConnection;
    property Execute: Boolean read FExecute write FExecute;
    property ConvertComponents: TConvertComponents read FConvertComponents write SetConvertComponents;
    property DeleteSourceComponents: Boolean read FDeleteSourceComponents
          write SetDeleteSourceComponents default True;
  end;

{$IFDEF DELPHI_5}
const
  S_OK = 0;
{$ENDIF}


implementation

uses Dialogs, Controls, ToolsAPI, TypInfo;

{ TDataSetList }
function TDataSetList.GetDataSetPairs(Index: Integer): PDataSetPair;
begin
  Result := PDataSetPair(Items[Index]);
end;

function TDataSetList.GetNewDataSets(Index: Integer): TDataSet;
begin
  Result := Pairs[Index].NewDataSet;
end;

function TDataSetList.GetOldDataSets(Index: Integer): TDataSet;
begin
  Result := Pairs[Index].OldDataSet;
end;

procedure TDataSetList.SetDataSetPairs(Index: Integer; Item: PDataSetPair);
begin
  Items[Index] := Item;
end;

procedure TDataSetList.SetNewDataSets(Index: Integer; Item: TDataSet);
begin
  Pairs[Index].NewDataSet := Item;
end;

procedure TDataSetList.SetOldDataSets(Index: Integer; Item: TDataSet);
begin
  Pairs[Index].OldDataSet := Item;
end;

function TDataSetList.GetPaired(aDataSet: TDataSet): TDataSet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count-1 do
    if OldDataSets[I] = aDataSet then
    begin
       Result := NewDataSets[I];
       exit;
    end;
end;

function TDataSetList.IndexOfOldDataSet(aDataSet: TDataSet): Integer;
var Index: Integer;
begin
  Result := -1;
  for Index := 0 to pred(Count) do
    if OldDataSets[Index] = aDataSet then begin
      Result := Index;
      exit;
    end;
end;

procedure TDataSetList.ClearAll;
var Index: Integer;
begin
  for Index := pred(Count) downto 0 do DeletePair(Index);
end;

procedure TDataSetList.DeletePair(Index: Integer);
begin
  FreeMem(PDataSetPair(Items[Index]));
  Delete(Index);
end;

destructor TDataSetList.Destroy;
begin
  ClearAll;
  inherited;
end;

{ TBDE2PSQLDAC }
function TBDE2PSQLDAC.GetDatabase: TPSQLDatabase;
begin
  Result := FPSQLDatabase;
end;

procedure TBDE2PSQLDAC.SetDatabase(Value: TPSQLDatabase);
begin
  FPSQLDatabase := Value;
  if Value <> nil then Value.FreeNotification(Self)
end;


procedure TBDE2PSQLDAC.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (aComponent = FPSQLDatabase) then
      FPSQLDatabase := nil
    else
      if (aComponent = FSourceConnection) then
        FSourceConnection := nil;
end;

procedure TBDE2PSQLDAC.AssignEvents(aDataSet: TPSQLDataSet;
  OldDataSet: TDataSet);
begin
  with OldDataSet do
  begin
    aDataSet.BeforeOpen     := BeforeOpen;
    aDataSet.AfterOpen      := AfterOpen ;
    aDataSet.BeforeClose    := BeforeClose;
    aDataSet.AfterClose     := AfterClose;
    aDataSet.BeforeInsert   := BeforeInsert;
    aDataSet.AfterInsert    := AfterInsert;
    aDataSet.BeforeEdit     := BeforeEdit;
    aDataSet.AfterEdit      := AfterEdit;
    aDataSet.BeforePost     := BeforePost;
    aDataSet.AfterPost      := AfterPost;
    aDataSet.BeforeCancel   := BeforeCancel;
    aDataSet.AfterCancel    := AfterCancel;
    aDataSet.BeforeDelete   := BeforeDelete;
    aDataSet.AfterDelete    := AfterDelete;
    aDataSet.BeforeScroll   := BeforeScroll;
    aDataSet.AfterScroll    := AfterScroll;
    aDataSet.OnCalcFields   := OnCalcFields;
    aDataSet.OnDeleteError  := OnDeleteError;
    aDataSet.OnEditError    := OnEditError;
    aDataSet.OnFilterRecord := OnFilterRecord;
    aDataSet.OnNewRecord    := OnNewRecord;
    aDataSet.OnPostError    := OnPostError;
  end;
end;

procedure TBDE2PSQLDAC.AssignFields(OldField, NewField: TField);
var
  s: string;
begin
  with OldField do
  begin
    NewField.FieldName    := FieldName;
    NewField.DisplayLabel := DisplayLabel;
    NewField.FieldKind    := FieldKind;
    NewField.EditMask     := EditMask;
    NewField.Alignment    := Alignment;
    NewField.DefaultExpression := DefaultExpression;
    NewField.DisplayWidth := DisplayWidth;
    NewField.Visible      := Visible;
    NewField.KeyFields := KeyFields;
    NewField.LookupCache := LookupCache;
    NewField.LookupDataSet := FDataSets.GetPaired(LookupDataSet);
    NewField.LookupKeyFields := LookupKeyFields;
    NewField.LookupResultField := LookupResultField;
    NewField.OnChange     := OnChange;
    NewField.OnGetText    := OnGetText;
    NewField.OnSetText    := OnSetText;
    NewField.OnValidate   := OnValidate;
    s := Name;
    Name := 'Die_' + Name;
    NewField.Name := s;
  end
end;

procedure TBDE2PSQLDAC.MoveFields(aDataSet: TPSQLDataSet;  OldDataSet: TDataSet);
begin
  if  OldDataSet.DefaultFields then Exit;
  with OldDataSet do
    while FieldCount > 0 do Fields[0].DataSet := aDataSet;
end;

procedure  TBDE2PSQLDAC.UpdateDesignInfo(OldDataSet, NewComponent: TComponent);
begin
  NewComponent.DesignInfo := OldDataSet.DesignInfo;
  OldDataSet.Owner.RemoveComponent(NewComponent);
  OldDataSet.Owner.InsertComponent(NewComponent);
end;

procedure  TBDE2PSQLDAC.CreatePSQLDataSet(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
var
   Index, NamePos: integer;
   rName1, rName2: string;
begin
    if Assigned(OldDataSet) then OldDataSet.Close; // PaGo 23.05.2007
    //UpdateDesignInfo(OldDataSet, aDataSet);
    aDataSet.DataBase := FPSQLDatabase;
    GetNeededSQLs(aDataSet, OldDataSet);
    AssignEvents(aDataSet, OldDataSet);
    MoveFields  (aDataSet, OldDataSet);
    GetCachedUpdates(aDataSet, OldDataSet);
    OldDataSet.Name := 'Die_' + OldDataSet.Name;
    aDataSet.Name := Copy(OldDataSet.Name, 5, length(OldDataSet.Name));
    for Index := 0 to pred(aDataSet.FieldCount) do
    begin
       rName1 := aDataSet.Fields[Index].Name;
       NamePos := Pos(aDataSet.Name, rName1);
       if NamePos = 1 then
       begin
          rName2 := Copy(rName1, length(aDataSet.Name) + 1, length(rName1));
          NamePos := Pos(aDataSet.Name, rName2);
          if NamePos = 1 then aDataSet.Fields[Index].Name := rName2;
       end;
    end;
    GetNeededUpdateObject(aDataSet, OldDataSet);
end;

procedure  TBDE2PSQLDAC.MigrateDataSources;
var
    List: TList;
    I: Integer;
    DataSource: TDataSource;
    DataSet: TDataSet;
begin
  List := TList.Create;
  try
   FillComponents(List,TDataSource);
   for I := 0 to List.Count-1 do
     begin
      DataSource := TDataSource(List[I]);
      DataSet := nil;
      if FDataSets.IndexOfOldDataSet(DataSource.DataSet) <> -1 then
         DataSet := FDataSets.GetPaired(DataSource.DataSet);
      if DataSet <> nil then
         DataSource.DataSet := DataSet;
     end;
  finally
   List.Free;
  end;
end;

procedure TBDE2PSQLDAC.Migrate;
var
  I: Integer;
begin
  if not Assigned(FPSQLDatabase) then
    DatabaseError('Database not assigned',Self);
  FDataSets.ClearAll;  // PaGo 23.05.2007
  Screen.Cursor := crHourGlass;
  try
    MigrateTables;
    MigrateDataSources;
    MigrateLookupFields;
    if DeleteSourceComponents then
     for I:=0 to FDataSets.Count-1 do
      begin
       if GetPropInfo(FDataSets.OldDataSets[I], 'UpdateObject') <> nil then
         GetObjectProp(FDataSets.OldDataSets[I], 'UpdateObject').Free;
       FDataSets.OldDataSets[I].Free;
      end;
    ShowMessage(Format('%d dataset descendants processed successfully',[FDataSets.Count]));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TBDE2PSQLDAC.MigrateTables;
var
  I: Integer;
  List: TList;
begin
  List := TList.Create;
  try
    FillComponents(List, TDataset);
    for i := List.Count-1 downto 0 do
     if SameText(TDataset(List[i]).ClassName, 'TNestedTable') then
       List.Delete(I)
     else
       CheckDataset(TDataset(List[i]));
  finally
    List.Free;
  end;
  for I := 0 to FDataSets.Count - 1 do
    CreatePSQLDataSet(TPSQLDataSet(FDataSets.NewDataSets[I]), FDataSets.OldDataSets[I]);
end;

constructor TBDE2PSQLDAC.Create(aOwner: TComponent);
begin
  inherited;
  FDataSets := TDataSetList.Create;
  FFields := TList.Create;
  FDeleteSourceComponents := True;
  FConvertComponents := [convBDE, convADO, convDBX, convZeos, convMySQLDAC];
end;

destructor TBDE2PSQLDAC.Destroy;
begin
  FDataSets.Free;
  FFields.Free;
  inherited;
end;

procedure TBDE2PSQLDAC.NewDataSetPair(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
var
  Pair: PDataSetPair;
begin
   New(Pair);
   Pair.OldDataSet := OldDataSet;
   Pair.NewDataSet := aDataSet;
   FDataSets.Add(Pair);
end;

procedure TBDE2PSQLDAC.CheckDataSet(OldDataSet: TDataSet);
var
  aDataSet: TPSQLDataSet;
  anObj: TObject;
  Propinfo: PPropInfo;
begin
  if Assigned(FSourceConnection) then
   begin
    Propinfo := GetPropInfo(PTypeInfo(OldDataSet.ClassInfo), 'Connection'); //ADO & Zeos
    if not Assigned(Propinfo) then
      Propinfo := GetPropInfo(PTypeInfo(OldDataSet.ClassInfo), 'SQLConnection'); //dbX
    if not Assigned(Propinfo) then
      Propinfo := GetPropInfo(PTypeInfo(OldDataSet.ClassInfo), 'Database'); //MySqlDAC
    if not Assigned(Propinfo) then Exit;
    anObj := GetObjectProp(OldDataSet, Propinfo);
    if anObj <> FSourceConnection then Exit;
   end;

  aDataSet := nil;

  if OldDataSet.ClassNameIs('TQuery') and (convBDE in FConvertComponents)  or
     OldDataSet.ClassNameIs('TZQuery') and (convZeos in FConvertComponents) or
     OldDataSet.ClassNameIs('TSQLQuery') and (convDBX in FConvertComponents) or
     OldDataSet.ClassNameIs('TSQLDataset') and (convDBX in FConvertComponents) or
     OldDataSet.ClassNameIs('TADOQuery') and (convADO in FConvertComponents) or
     OldDataSet.ClassNameIs('TADODataset') and (convADO in FConvertComponents) or
     OldDataSet.ClassNameIs('TMySQLQuery') and (convMySQLDAC in FConvertComponents) then
       {$WARNINGS OFF} //make D5 happy
            aDataSet := TPSQLQuery.Create(OldDataSet.Owner)
       {$WARNINGS ON} //make D5 happy

  else

  if OldDataSet.ClassNameIs('TTable') and (convBDE in FConvertComponents)  or
     OldDataSet.ClassNameIs('TZTable') and (convZeos in FConvertComponents) or
     OldDataSet.ClassNameIs('TSQLTable') and (convDBX in FConvertComponents) or
     OldDataSet.ClassNameIs('TADOTable') and (convADO in FConvertComponents) or
     OldDataSet.ClassNameIs('TMySQLTable') and (convMySQLDAC in FConvertComponents) then
       {$WARNINGS OFF} //make D5 happy
            aDataSet := TPSQLTable.Create(OldDataSet.Owner)
       {$WARNINGS ON} //make D5 happy

  else

  if OldDataSet.ClassNameIs('TStoredProc') and (convBDE in FConvertComponents)  or
     OldDataSet.ClassNameIs('TZStoredProc') and (convZeos in FConvertComponents) or
     OldDataSet.ClassNameIs('TSQLStoredProc') and (convDBX in FConvertComponents) or
     OldDataSet.ClassNameIs('TADOStoredProc') and (convADO in FConvertComponents) or
     OldDataSet.ClassNameIs('TMySQLStoredProc') and (convMySQLDAC in FConvertComponents)then
       {$WARNINGS OFF} //make D5 happy
            aDataSet := TPSQLStoredProc.Create(OldDataSet.Owner)
       {$WARNINGS ON} //make D5 happy
  else

  if OldDataSet.ClassNameIs('TMySQLMacroQuery') and (convMySQLDAC in FConvertComponents) then
    {$WARNINGS OFF} //make D5 happy
            aDataSet := TPSQLMacroQuery.Create(OldDataSet.Owner);
    {$WARNINGS ON} //make D5 happy

  if Assigned(aDataset) then
    begin
      UpdateDesignInfo(OldDataSet, aDataSet);
      NewDataSetPair(aDataSet, OldDataSet);
    end;
end;

procedure TBDE2PSQLDAC.GetCachedUpdates(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
begin
   aDataSet.CachedUpdates := False;
end;

procedure TBDE2PSQLDAC.GetNeededSQLs(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
var AObj: TObject;
begin
  if OldDataSet.ClassNameIs('TQuery') or OldDataSet.ClassNameIs('TZQuery') or
     OldDataSet.ClassNameIs('TSQLQuery') or OldDataSet.ClassNameIs('TADOQuery') or
     OldDataSet.ClassNameIs('TMySQLMacroQuery') then
      begin
       AObj := GetObjectProp(OldDataSet,'SQL');
       if Assigned(AObj) and (AObj is TStrings) then
         TPSQLQuery(aDataSet).SQL.Assign(AObj as TStrings)
      end
  else
   if OldDataSet.ClassNameIs('TSQLDataset') or OldDataSet.ClassNameIs('TADODataset') then
         TPSQLQuery(aDataSet).SQL.Text := GetStrProp(OldDataSet, 'CommandText')
  else
   if OldDataSet.ClassNameIs('TTable') or OldDataSet.ClassNameIs('TZTable') or
      OldDataSet.ClassNameIs('TSQLTable') or OldDataSet.ClassNameIs('TADOTable') then
       begin
        TPSQLTable(aDataSet).TableName := GetStrProp(OldDataSet, 'TableName');
        if GetPropInfo(OldDataset,'Filtered') <> nil then
           TPSQLTable(aDataSet).Filtered := Boolean(GetOrdProp(OldDataSet, 'Filtered'));
        if GetPropInfo(OldDataset,'Filter') <> nil then
           TPSQLTable(aDataSet).Filter := GetStrProp(OldDataSet, 'Filter');
       end
  else
   if OldDataSet.ClassNameIs('TStoredProc') or OldDataSet.ClassNameIs('TZStoredProc') or
      OldDataSet.ClassNameIs('TSQLStoredProc') then
       TPSQLStoredProc(aDataSet).StoredProcName := GetStrProp(OldDataSet, 'StoredProcName')
  else
   if OldDataSet.ClassNameIs('TADOStoredProc') then
       TPSQLStoredProc(aDataSet).StoredProcName := GetStrProp(OldDataSet, 'ProcedureName');
end;

procedure TBDE2PSQLDAC.GetNeededUpdateObject(aDataSet: TPSQLDataSet; OldDataSet: TDataSet);
var
    Obj: TObject;

   procedure AssignSQL(const aSQL: TStrings; const PropName: string);
    var AObj: TObject;
   begin
       try
        AObj := GetObjectProp(Obj,PropName);
        If Assigned(AObj) and (AObj is TStrings) then
         aSQL.Assign(AObj as TStrings);
       except
       end;
   end;

begin
    if GetPropInfo(OldDataset,'UpdateObject') <> nil then
      Obj := GetObjectProp(OldDataSet, 'UpdateObject')
    else
      Exit;
    if Assigned(Obj) and
       (Obj.ClassNameIs('TUpdateSQL') or Obj.ClassNameIs('TZUpdateSQL')) then
        begin
          aDataset.UpdateObject := TPSQLUpdateSQL.Create((Obj as TComponent).Owner);
          UpdateDesignInfo(Obj as TComponent,aDataset.UpdateObject);
          (Obj as TComponent).Name := 'Die_' + (Obj as TComponent).Name;
          aDataset.UpdateObject.Name := Copy((Obj as TComponent).Name,5,MaxInt);
          AssignSQL(TPSQLUpdateSQL(aDataSet.UpdateObject).ModifySQL, 'ModifySQL');
          AssignSQL(TPSQLUpdateSQL(aDataSet.UpdateObject).InsertSQL, 'InsertSQL');
          AssignSQL(TPSQLUpdateSQL(aDataSet.UpdateObject).DeleteSQL, 'DeleteSQL');
        end;
end;

procedure TBDE2PSQLDAC.FillComponents(ACompList: TList;
  ACompClass: TClass);
var
  I, J, K: Integer;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  Editor: IOTAEditor;
  FormEditor: IOTAFormEditor;
  RootComp: IOTAComponent;
  Comp: TComponent;
begin
  If not (Assigned(ACompList) and Assigned(ACompClass)) then
    Exit;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices = nil then Exit;
  for I := 0 to ModuleServices.ModuleCount - 1 do
  begin
    Module := ModuleServices.Modules[I];
    for J := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := Module.GetModuleFileEditor(J);
      if Editor.QueryInterface(IOTAFormEditor, FormEditor) = S_OK then
      begin
        FormEditor.Show;
        RootComp := FormEditor.GetRootComponent;
        if RootComp <> nil then
        begin
          Comp := (RootComp as INTAComponent).GetComponent;
          for K :=0 to Comp.ComponentCount-1 do
           if Comp.Components[K] is ACompClass then
             ACompList.Add(Comp.Components[K]);
        end;
      end;
    end;
  end;
end;

procedure TBDE2PSQLDAC.SetDeleteSourceComponents(const Value: Boolean);
begin
  FDeleteSourceComponents := Value;
end;

procedure TBDE2PSQLDAC.SetSourceConnection(const Value: TCustomConnection);
begin
  FSourceConnection := Value;
  if Value <> nil then Value.FreeNotification(Self)
end;

procedure TBDE2PSQLDAC.MigrateLookupFields;
var
    FieldList: TList;
    I: Integer;
begin
  FieldList := TList.Create;
  try
   FillComponents(FieldList,TField);
   for I := 0 to FieldList.Count-1 do
     with TField(FieldList[i]) do
      If Lookup then
        LookupDataSet := FDataSets.GetPaired(LookupDataSet);
  finally
   FieldList.Free;
  end;
end;

procedure TBDE2PSQLDAC.SetConvertComponents(
  const Value: TConvertComponents);
begin
  FConvertComponents := Value;
end;

end.



