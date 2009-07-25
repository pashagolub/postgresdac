{$I PSQLdac.inc}
unit PSQLCOMP;

{SVN revision: $Id$}

interface

Uses Windows,Messages,SysUtils,Classes, Graphics, Controls,Forms, Dialogs,
     {$IFDEF DELPHI_5}DsgnIntf{$ELSE}DesignIntf,DesignEditors{$ENDIF},
     Db, {$IFNDEF BCB}DsDesign,{$ENDIF} PSQLFldLinks, PSQLDbTables, PSQLupdsqled, PSQLBatch, PSQLMacroQuery,
     PSQLMigrator, PSQLMonitor, PSQLTools, PSQLDump, PSQLCopy, PSQLMetaData,
     PSQLDirectQuery, PSQLFields;

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

  TPSQLParamOidPropertyEditor =  Class(TIntegerProperty)
    Public
      function  GetAttributes: TPropertyAttributes; Override;
      procedure GetValueList(List: TStrings);
      procedure GetValues(Proc: TGetStrProc); Override;
      procedure SetValue(const Value: string); override;
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

{$IFNDEF BCB}
  TPSQLDSDesigner = class(TDSDesigner)
  public
    procedure EndCreateFields; override;
  end;

  TPSQLQueryEditor = class(TComponentEditor)
  private
    FConnection: TPSQLDatabase;
  protected
    function GetDSDesignerClass: TDSDesignerClass; virtual;
  public
    procedure GetTables(List: TStrings; SystemTables: Boolean);
    procedure GetFields(const TableName: string; List: TStrings; SystemFields: Boolean);
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}

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

Uses {BDEConst,}TypInfo,PSQLAboutFrm,
PSQLConnFrm, PSQLStoredProcFrm, PSQLEdit, PSQLTypes, DBCommon;

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
    RegisterPropertyEditor(TypeInfo(string), TPSQLDatabase, 'CharSet', TPSQLDatabaseCharsetPropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLDump, 'Encoding', TPSQLDatabaseCharsetPropertyEditor);
    RegisterPropertyEditor(TypeInfo(TFileName), TPSQLTable, 'TableName', TPSQLTableNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(cardinal), TPSQLParam, 'DataTypeOID', TPSQLParamOidPropertyEditor);
    RegisterPropertyEditor(TypeInfo(TFileName), TPSQLCopy, 'TableName', TPSQLTableNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLTable, 'IndexName', TPSQLIndexNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLStoredProc, 'StoredProcName', TPSQLStoredProcNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLUser, 'UserName', TPSQLUserNamePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLTable, 'IndexFieldNames', TPSQLIndexFieldNamesPropertyEditor);
    RegisterPropertyEditor(TypeInfo(TDataSource), TPSQLTable, 'MasterSource', TPSQLDataSourcePropertyEditor);
    RegisterPropertyEditor(TypeInfo(string), TPSQLTable, 'MasterFields', TPSQLTableFieldLinkProperty);
    RegisterPropertyEditor(TypeInfo(TPSQLDACAbout), nil, '', TAboutProperty);
    RegisterPropertyEditor(TypeInfo(Boolean), TBDE2PSQLDAC, 'Execute', TMigrateExecutePropertyEditor);
end;

procedure Register;
begin
  RegisterComponents('PostgresDAC',
      [TPSQLDatabase, TPSQLTable, TPSQLQuery, TPSQLStoredProc, TPSQLUpdateSQL, TPSQLNotify,
      TPSQLBatchExecute, TPSQLMacroQuery, TPSQLMonitor, TPSQLDirectQuery,
      TPSQLTools, TPSQLCopy, TPSQLDump, TPSQLRestore, TPSQLUser, TBDE2PSQLDAC] );
  RegisterComponentEditor(TPSQLDatabase, TPSQLDatabaseEditor);
  {$IFNDEF BCB}RegisterComponentEditor(TPSQLQuery, TPSQLQueryEditor);{$ENDIF}
  RegisterComponentEditor(TPSQLUpdateSQL,TPSQLUpdateSQLEditor);
  RegisterComponentEditor(TPSQLStoredProc,TPSQLStoredProcEditor);
  RegisterFields([TPSQLGuidField]);
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
  AComp: TPersistent;
begin
  DB := nil;
  AComp := GetComponent(0);
  if AComp is TPSQLDatabase then
    DB := AComp as TPSQLDatabase
  else
   if (AComp is TPSQLDump) and Assigned((AComp as TPSQLDump).Database) then
     DB := (AComp as TPSQLDump).Database;
  if not Assigned(DB) or not DB.Connected then
    List.CommaText := '<default>,BIG5,EUC_CN,EUC_JIS_2004,EUC_JP,EUC_KR,EUC_TW,'+
     'GB18030,GBK,ISO_8859_5,ISO_8859_6,ISO_8859_7,ISO_8859_8,JOHAB,KOI8,LATIN1,'+
     'LATIN10,LATIN2,LATIN3,LATIN4,LATIN5,LATIN6,LATIN7,LATIN8,LATIN9,MULE_INTERNAL,'+
     'SHIFT_JIS_2004,SJIS,SQL_ASCII,UHC,UTF8,WIN1250,WIN1251,WIN1252,WIN1253,'+
     'WIN1254,WIN1255,WIN1256,WIN1257,WIN1258,WIN866,WIN874'
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

{ TPSQLDataSetEditor }
{$IFNDEF BCB}
procedure TPSQLQueryEditor.ExecuteVerb(Index: Integer);
var
  SQL: string;
  TableName: string;
begin
  case Index of
   0: ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
   1:
    begin
      FConnection := TPSQLQuery(Component).Database;
      try
        SQL := TPSQLQuery(Component).SQL.Text;
        if SQL <> '' then
          TableName := GetTableNameFromSQL(SQL);
        if EditSQL(SQL, GetTables, GetFields,
           TableName) then
          TPSQLQuery(Component).SQL.Text := SQL;
      finally
        FConnection := nil;
      end;
   end;
 end;
 Designer.Modified;
end;

function TPSQLQueryEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TPSQLDSDesigner;
end;

procedure TPSQLQueryEditor.GetFields(const TableName: string;
  List: TStrings; SystemFields: Boolean);
var S: string;
begin
  List.Clear;
  S := Format('SELECT quote_ident(attname) FROM pg_attribute WHERE attrelid = %s::regclass',
                              [QuotedStr(Tablename)]);
  if not SystemFields then
   S := S + ' AND attnum > 0';
  if Assigned(FConnection) then
   FConnection.SelectStrings(S, List);
end;

procedure TPSQLQueryEditor.GetTables(List: TStrings;
  SystemTables: Boolean);
begin
  List.Clear;
  if Assigned(FConnection) then
    FConnection.GetTableNames('', SystemTables, List);
end;

function TPSQLQueryEditor.GetVerb(Index: Integer): string;
begin
  case Index of
   0: Result := 'Fields Editor...';
   1: Result := 'SQL Editor...';
  end;
end;

function TPSQLQueryEditor.GetVerbCount: Integer;
begin
 Result := 2;
end;

{ TPSQLDSDesigner }

procedure TPSQLDSDesigner.EndCreateFields;
var OldState: boolean;
begin
  inherited;
  if not (DataSet is TPSQLQuery) then Exit;
  if not (dsoPopulateFieldsOrigin in TPSQLQuery(DataSet).Options) then Exit;

  OldState := TPSQLQuery(DataSet).Active;
  try
    TPSQLQuery(DataSet).Active := True;
    TPSQLQuery(DataSet).PopulateFieldsOrigin();
  finally
    TPSQLQuery(DataSet).Active := OldState;
  end;
end;
{$ENDIF}

{ TPSQLParamOidPropertyEditor }

function TPSQLParamOidPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect];
end;

procedure TPSQLParamOidPropertyEditor.GetValueList(List: TStrings);
begin
  List.Text :=
    '"char"=18'#13'abstime=702'#13'aclitem=1033'#13'bit varying=1562'#13'bit=1560'#13'bool=16'#13'box=603'#13'bytea=17'#13'char=1042'#13''+
    'cid=29'#13'cidr=650'#13'circle=718'#13'date=1082'#13'float4=700'#13'float8=701'#13'inet=869'#13'int2=21'#13'int2vector=22'#13'int4=23'#13'int8=20'#13''+
    'interval=1186'#13'line=628'#13'lseg=601'#13'macaddr=829'#13'money=790'#13'name=19'#13'numeric=1700'#13'oid=26'#13'oidvector=30'#13'path=602'#13''+
    'point=600'#13'polygon=604'#13'refcursor=1790'#13'regclass=2205'#13'regoper=2203'#13'regoperator=2204'#13'regproc=24'#13'regprocedure=2202'#13''+
    'regtype=2206'#13'reltime=703'#13'smgr=210'#13'text=25'#13'tid=27'#13'time with time zone=1266'#13'time=1083'#13'timestamp with time zone=1184'#13''+
    'timestamp=1114'#13'tinterval=704'#13'tsquery=3615'#13'tsvector=3614'#13'uuid=2950'#13'varchar=1043'#13'xid=28'#13'xml=142'#13''+
    //arrays bellow
    'abstime[]=1023'#13'aclitem[]=1034'#13'bit varying[]=1563'#13'bit[]=1561'#13'bool[]=1000'#13'box[]=1020'#13'bpchar[]=1014'#13'bytea[]=1001'#13''+
    'char[]=1002'#13'cid[]=1012'#13'cidr[]=651'#13'circle[]=719'#13'date[]=1182'#13'float4[]=1021'#13'float8[]=1022'#13'inet[]=1041'#13'int2[]=1005'#13''+
    'int2vector[]=1006'#13'int4[]=1007'#13'int8[]=1016'#13'interval[]=1187'#13'line[]=629'#13'lseg[]=1018'#13'macaddr[]=1040'#13'money[]=791'#13''+
    'name[]=1003'#13'numeric[]=1231'#13'oid[]=1028'#13'oidvector[]=1013'#13'path[]=1019'#13'point[]=1017'#13'polygon[]=1027'#13'refcursor[]=2201'#13''+
    'regclass[]=2210'#13'regoper[]=2208'#13'regoperator[]=2209'#13'regproc[]=1008'#13'regprocedure[]=2207'#13'regtype[]=2211'#13'reltime[]=1024'#13''+
    'text[]=1009'#13'tid[]=1010'#13'time with time zone[]=1270'#13'time[]=1183'#13'timestamp with time zone[]=1185'#13'timestamp[]=1115'#13''+
    'tinterval[]=1025'#13'varchar[]=1015'#13'xid[]=1011';
end;

procedure TPSQLParamOidPropertyEditor.GetValues(Proc: TGetStrProc);
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

procedure TPSQLParamOidPropertyEditor.SetValue(const Value: string);
begin
  inherited SetValue(Copy(Value, Pos('=', Value) + 1, MaxInt));
end;

end.


