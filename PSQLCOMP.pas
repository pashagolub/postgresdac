{$I PSQLdac.inc}
unit PSQLCOMP;

interface            

Uses Windows,Messages,SysUtils,Classes, Graphics, Controls,Forms, Dialogs,
     {$IFDEF DELPHI_6}DesignIntf,DesignEditors {$ELSE}DsgnIntf{$ENDIF},
     Db,PSQLFldLinks,PSQLDbTables,PSQLupdsqled,PSQLBatch,PSQLMacroQuery,
     PSQLMigrator, PSQLMonitor, PSQLTools, PSQLDump, PSQLCopy, PSQLMetaData,
     PSQLDirectQuery;

type
    TAboutProperty = class(TPropertyEditor)
    Public
      procedure Edit; override;
      function  GetAttributes: TPropertyAttributes; override;
      function  GetValue: string; override;
    end;

    TMigrateExecutePropertyEditor = class(TPropertyEditor)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function GetValue: string; override;
    end;

    TPSQLTableNamePropertyEditor =  Class(TStringProperty)
    Public
      Function  GetAttributes: TPropertyAttributes; Override;
      Procedure GetValueList(List: TStrings);
      Procedure GetValues(Proc: TGetStrProc); Override;
    end;

    TPSQLUserNamePropertyEditor =  Class(TStringProperty)
    Public
      Function  GetAttributes: TPropertyAttributes; Override;
      Procedure GetValueList(List: TStrings);
      Procedure GetValues(Proc: TGetStrProc); Override;
    end;

    TPSQLStoredProcNamePropertyEditor =  Class(TStringProperty)
    Public
      Function  GetAttributes: TPropertyAttributes; Override;
      Procedure GetValueList(List: TStrings);
      Procedure GetValues(Proc: TGetStrProc); Override;
    end;

    TPSQLIndexNamePropertyEditor =  Class(TStringProperty)
    Public
      Function  GetAttributes: TPropertyAttributes; Override;
      Procedure GetValueList(List: TStrings);
      Procedure GetValues(Proc: TGetStrProc); Override;
  end;


  TPSQLIndexFieldNamesPropertyEditor = Class(TStringProperty)
    Public
      Function  GetAttributes: TPropertyAttributes; Override;
      Procedure GetValueList(List: TStrings);
      Procedure GetValues(Proc: TGetStrProc); Override;
  end;

  { TPSQLTableFieldLinkProperty }
  TPSQLTableFieldLinkProperty = class(TPSQLFieldLinkProperty)
  private
    FTable: TPSQLTable;
  protected
    procedure GetFieldNamesForIndex(List: TStrings); override;
    function GetIndexBased: Boolean; override;
    function GetIndexDefs: TIndexDefs; override;
    function GetIndexFieldNames: string; override;
    function GetIndexName: string; override;
    function GetMasterFields: string; override;
    procedure SetIndexFieldNames(const Value: string); override;
    procedure SetIndexName(const Value: string); override;
    procedure SetMasterFields(const Value: string); override;
  public
    property IndexBased: Boolean read GetIndexBased;
    property IndexDefs: TIndexDefs read GetIndexDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    procedure Edit; override;
  end;

  TPSQLDataSourcePropertyEditor =  Class(TComponentProperty)
  Private
    FCheckProc: TGetStrProc;
    Procedure CheckComponent( const Value : string );
  Public
    Procedure GetValues(Proc: TGetStrProc); override;
  end;

  TPSQLDatabaseEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


  TPSQLStoredProcEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TPSQLUpdateSQLEditor = class(TComponentEditor)
   public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
   end;

  TPSQLDatabaseCharsetPropertyEditor =  Class(TStringProperty)
   Public
    Function  GetAttributes: TPropertyAttributes; Override;
    Procedure GetValueList(List: TStrings);
    Procedure GetValues(Proc: TGetStrProc); Override;
   end;


Procedure Register;
Procedure RegisterPropertyEditors;

implementation

Uses {$IFNDEF DELPHI_4}BDEConst{$ELSE}DsnDBCst{$ENDIF},TypInfo,PSQLAboutFrm,
PSQLConnFrm, PSQLStoredProcFrm;
{$R DBPRO.DCR}

function GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetPropertyValue(DataSet, 'IndexDefs') as TIndexDefs;
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

{About Property}
function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly]
end;

procedure TAboutProperty.Edit;
begin
 DAC4PSQLShowAbout(GetComponent(0).ClassName); //mi:2007-09-28
end;

function TAboutProperty.GetValue: string;
begin
  Result := 'About...';
end;

{PSQLTableName}
Function TPSQLTableNamePropertyEditor.GetAttributes : TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

Procedure TPSQLTableNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do  Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;

Procedure TPSQLTableNamePropertyEditor.GetValueList(List: TStrings);
var
  DB: TPSQLDatabase;
  Cursor: TCursor;
begin
  If GetComponent(0) is TPSQLTable then
    DB := (GetComponent(0) as TPSQLTable).Database
  else
    If GetComponent(0) is TPSQLCopy then
      DB := (GetComponent(0) as TPSQLCopy).Database
    else
      DB := nil;
   if Db = nil then raise EDatabaseError.Create('Database property is not set');
  Cursor := Screen.Cursor;
  try
   DB.GetTableNames('', False, List);
  finally
    Screen.Cursor := Cursor;
  end;
end;

{TPSQLIndexName}
Function TPSQLIndexNamePropertyEditor.GetAttributes : TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

Procedure TPSQLIndexNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList( Values );
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;

Procedure TPSQLIndexNamePropertyEditor.GetValueList(List : TStrings);
begin
  (GetComponent(0) as TPSQLTable).GetIndexNames(List);
end;

{TPSQLIndexFieldNamesPropertyEditor}
Function TPSQLIndexFieldNamesPropertyEditor.GetAttributes : TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

Procedure TPSQLIndexFieldNamesPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList(Values);
    for I := 0 to Values.Count-1 do Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;

Procedure TPSQLIndexFieldNamesPropertyEditor.GetValueList( List : TStrings );
var
  I: Integer;
  IndexDefs: TIndexDefs;
begin
  IndexDefs := GetIndexDefs(GetComponent(0));
  for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      if (Options * [ixExpression, ixDescending] = []) and (Fields <> '') then List.Add(Fields);
end;

{ TPSQLTableFieldLinkProperty }
procedure TPSQLTableFieldLinkProperty.Edit;
var
  Table: TPSQLTable;
begin
  Table := DataSet as TPSQLTable;
  FTable := TPSQLTable.Create(nil);
  try
    FTable.Database := Table.Database;
    FTable.TableName := Table.TableName;
    if Table.IndexFieldNames <> '' then
      FTable.IndexFieldNames := Table.IndexFieldNames else
      FTable.IndexName := Table.IndexName;
    FTable.MasterFields := Table.MasterFields;
    FTable.Open;
    inherited Edit;
    if Changed then
    begin
      Table.MasterFields := FTable.MasterFields;
      if FTable.IndexFieldNames <> '' then
        Table.IndexFieldNames := FTable.IndexFieldNames else
        Table.IndexName := FTable.IndexName;
    end;
  finally
    FTable.Free;
  end;
end;

procedure TPSQLTableFieldLinkProperty.GetFieldNamesForIndex(List: TStrings);
var
  i: Integer;
begin
  for i := 0 to FTable.IndexFieldCount - 1 do
    List.Add(FTable.IndexFields[i].FieldName);
end;

function TPSQLTableFieldLinkProperty.GetIndexBased: Boolean;
begin
  Result := {$IFDEF DELPHI_4} not True{$ELSE}not IProviderSupport(FTable).PSIsSQLBased{$ENDIF};
end;

function TPSQLTableFieldLinkProperty.GetIndexDefs: TIndexDefs;
begin
  Result := FTable.IndexDefs;
end;

function TPSQLTableFieldLinkProperty.GetIndexFieldNames: string;
begin
  Result := FTable.IndexFieldNames;
end;

function TPSQLTableFieldLinkProperty.GetIndexName: string;
begin
  Result := FTable.IndexName;
end;

function TPSQLTableFieldLinkProperty.GetMasterFields: string;
begin
  Result := FTable.MasterFields;
end;

procedure TPSQLTableFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TPSQLTableFieldLinkProperty.SetIndexName(const Value: string);
begin
  if Value = 'Primary' then
    FTable.IndexName := '' else
    FTable.IndexName := Value;
end;

procedure TPSQLTableFieldLinkProperty.SetMasterFields(const Value: string);
begin
  FTable.MasterFields := Value;
end;

{PSQLDataSource}
Procedure TPSQLDataSourcePropertyEditor.CheckComponent( const Value : string );
var
  J: Integer;
  DataSource: TDataSource;
begin
  DataSource := TDataSource( Designer.GetComponent(Value ) );
  for J := 0 to Pred( PropCount ) do
    if TDataSet( GetComponent( J ) ).IsLinkedTo( DataSource ) then
      Exit;
  FCheckProc( Value );
end;

Procedure TPSQLDataSourcePropertyEditor.GetValues( Proc : TGetStrProc );
begin
  FCheckProc := Proc;
  Inherited GetValues( CheckComponent );
end;

{PSQLDatabase Editor}
procedure TPSQLDatabaseEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditDatabase(TPSQLDatabase(Component)) then Designer.Modified;
  end;
end;

function TPSQLDatabaseEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'TPSQLDatabase Editor...';
  end;
end;

function TPSQLDatabaseEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TPSQLUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  if EditPSQLUpdateSQL(TPSQLUpdateSQL(Component)) then Designer.Modified;
end;

function TPSQLUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  Result := '&PSQLUpdateSQL Editor...';
end;

function TPSQLUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


Procedure RegisterPropertyEditors;
begin
    RegisterPropertyEditor(TypeInfo(string),TPSQLDatabase,'CharSet',TPSQLDatabaseCharsetPropertyEditor);
    RegisterPropertyEditor(TypeInfo(TFileName),TPSQLTable,'TableName',TPSQLTableNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(TFileName),TPSQLCopy,'TableName',TPSQLTableNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string),TPSQLTable,'IndexName',TPSQLIndexNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string),TPSQLStoredProc,'StoredProcName',TPSQLStoredProcNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string),TPSQLUser,'UserName',TPSQLUserNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string),TPSQLTable,'IndexFieldNames',TPSQLIndexFieldNamesPropertyEditor);
    RegisterPropertyEditor(TypeInfo(TDataSource),TPSQLTable,'MasterSource',TPSQLDataSourcePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string),TPSQLTable,'MasterFields',TPSQLTableFieldLinkProperty);
    RegisterPropertyEditor(TypeInfo(TPSQLDACAbout),nil,'',TAboutProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), TBDE2PSQLDAC, 'Execute', TMigrateExecutePropertyEditor);
end;

procedure Register;
begin
  RegisterComponents('PostgresDAC',
      [TPSQLDatabase,TPSQLTable,TPSQLQuery,TPSQLStoredProc,TPSQLUpdateSQL,TPSQLNotify,
      TPSQLBatchExecute, TPSQLMacroQuery, TPSQLMonitor, TPSQLDirectQuery,
      TPSQLTools, TPSQLCopy, TPSQLDump, TPSQLRestore, TPSQLUser, TBDE2PSQLDAC] );
  RegisterComponentEditor(TPSQLDatabase, TPSQLDatabaseEditor);
  RegisterComponentEditor(TPSQLUpdateSQL,TPSQLUpdateSQLEditor);
  RegisterComponentEditor(TPSQLStoredProc,TPSQLStoredProcEditor);
  RegisterPropertyEditors;
end;

{ TMigrateExecutePropertyEditor }

procedure TMigrateExecutePropertyEditor.Edit;
begin
   TBDE2PSQLDAC(GetComponent(0)).Migrate;
   Modified; // PaGo 23.05.2007
end;

function TMigrateExecutePropertyEditor.GetAttributes: TPropertyAttributes;
begin
   Result := [paDialog, paReadOnly];
end;

function TMigrateExecutePropertyEditor.GetValue: string;
begin
   Result := 'Press to Migrate...';
end;

{ TPSQLStoredProcNamePropertyEditor }

function TPSQLStoredProcNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TPSQLStoredProcNamePropertyEditor.GetValueList(List: TStrings);
var
  Proc: TPSQLStoredProc;
begin
  Proc := GetComponent(0) as TPSQLStoredProc;
  if Proc.Database = nil then raise EDatabaseError.Create('Database property is not set');
  Proc.Database.GetStoredProcNames('', List);
end;


procedure TPSQLStoredProcNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do  Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;
{ TPSQLDatabaseCharsetPropertyEditor }

function TPSQLDatabaseCharsetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TPSQLDatabaseCharsetPropertyEditor.GetValueList(List: TStrings);
var
  DB: TPSQLDatabase;
begin
  DB := GetComponent(0) as TPSQLDatabase;
  if not DB.Connected then
    List.CommaText := '<default>,EUC_JP,EUC_CN,EUC_KR,EUC_TW,JOHAB,UNICODE,MULE_INTERNAL,LATIN1,LATIN2,LATIN3,LATIN4,'+
                      'LATIN5,LATIN6,LATIN7,LATIN8,LATIN9,LATIN10,WIN1256,TCVN,WIN874,KOI8,WIN,ALT,ISO_8859_5,'+
                      'ISO_8859_6,ISO_8859_7,ISO_8859_8,WIN1250,SJIS,BIG5,GBK,UHC,GB18030'
  else
   begin
    DB.GetCharsets(List);
    List.Insert(0,'<default>');
   end;
end;

procedure TPSQLDatabaseCharsetPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do  Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;

{ TPSQLStoredProcEditor }

procedure TPSQLStoredProcEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: if EditStoredProc(TPSQLStoredProc(Component)) then Designer.Modified;
  end;
end;

function TPSQLStoredProcEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'TPSQLStoredProc Editor...';
  end;
end;

function TPSQLStoredProcEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TPSQLUserNamePropertyEditor }

function TPSQLUserNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TPSQLUserNamePropertyEditor.GetValueList(List: TStrings);
var
  User: TPSQLUser;
begin
  User := GetComponent(0) as TPSQLUser;
  if User.Database = nil then raise EDatabaseError.Create('Database property is not set');
  User.Database.GetUserNames('', List);
end;


procedure TPSQLUserNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I      : Integer;
  Values : TStringList;
begin
  Values := TStringList.Create;
  Try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do  Proc(Values[I]);
  Finally
    Values.Free;
  end;
end;

end.