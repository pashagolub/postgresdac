
{******************************************}
{                                          }
{             FastReport v3.0              }
{         PSQL enduser components           }
{                                          }

// Created by: MicroOLAP Technologies LTD.
// E-mail: support@microolap.com

{                                          }
{******************************************}

unit frxPSQLComponents;

interface

{$I frx.inc}

uses
  Windows, Classes, frxClass, frxCustomDB, DB, PSQLDbTables, PSQLAccess, PSQLTypes
{$IFDEF Delphi6}
, Variants
{$ENDIF}
{$IFDEF QBUILDER}
, fqbClass
{$ENDIF};


type
  TfrxPSQLComponents = class(TfrxDBComponents)
  private
    FDefaultDatabase: TPSQLDatabase;
    FOldComponents: TfrxPSQLComponents;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDescription: String; override;
  published
    property DefaultDatabase: TPSQLDatabase read FDefaultDatabase write FDefaultDatabase;
  end;

  TfrxPSQLDatabase = class(TfrxCustomDatabase)
  private
    FDatabase: TPSQLDatabase;
    function GetUserName: string;
    function GetUserPassword: string;
    procedure SetUserName(const Value: string);
    procedure SetUserPassword(const Value: string);
  protected
    procedure SetConnected(Value: Boolean); override;
    procedure SetDatabaseName(const Value: String); override;
    procedure SetLoginPrompt(Value: Boolean); override;
    procedure SetParams(Value: TStrings); override;
    function GetConnected: Boolean; override;
    function GetDatabaseName: String; override;
    function GetLoginPrompt: Boolean; override;
    function GetParams: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function GetDescription: String; override;
    procedure SetLogin(const Login, Password: String); override;
    property Database: TPSQLDatabase read FDatabase;
  published
    property DatabaseName;
    property UserName: string read GetUserName write SetUserName;
    property UserPassword: string read GetUserPassword write SetUserPassword;
    property LoginPrompt;
    property Params;
    property Connected;
  end;

  TfrxPSQLTable = class(TfrxCustomTable)
  private
    FDatabase: TfrxPSQLDatabase;
    FTable: TPSQLTable;
    procedure SetDatabase(const Value: TfrxPSQLDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetMasterFields(const Value: String); override;
    procedure SetIndexFieldNames(const Value: String); override;
    procedure SetIndexName(const Value: String); override;
    procedure SetTableName(const Value: String); override;
    function GetIndexFieldNames: String; override;
    function GetIndexName: String; override;
    function GetTableName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    property Table: TPSQLTable read FTable;
  published
    property Database: TfrxPSQLDatabase read FDatabase write SetDatabase;
  end;

  TfrxPSQLQuery = class(TfrxCustomQuery)
  private
    FDatabase: TfrxPSQLDatabase;
    FQuery: TPSQLQuery;
    procedure SetDatabase(const Value: TfrxPSQLDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMaster(const Value: TDataSource); override;
    procedure SetSQL(Value: TStrings); override;
    function GetSQL: TStrings; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor DesignCreate(AOwner: TComponent; Flags: Word); override;
    class function GetDescription: String; override;
    procedure BeforeStartReport; override;
    procedure UpdateParams; override;
{$IFDEF QBUILDER}
    function QBEngine: TfqbEngine; override;
{$ENDIF}
    property Query: TPSQLQuery read FQuery;
  published
    property Database: TfrxPSQLDatabase read FDatabase write SetDatabase;
  end;

{$IFDEF QBUILDER}
  TfrxEnginePSQL = class(TfqbEngine)
  private
    FQuery: TPSQLQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReadTableList(ATableList: TStrings); override;
    procedure ReadFieldList(const ATableName: string; var AFieldList: TfqbFieldList); override;
    function ResultDataSet: TDataSet; override;
    procedure SetSQL(const Value: string); override;
  end;
{$ENDIF}


var
  PSQLComponents: TfrxPSQLComponents;


implementation

{$R frxpsqlbuttns.dcr}

uses
  Graphics,
  frxPSQLRTTI,
{$IFNDEF NO_EDITORS}
  frxPSQLEditor,
{$ENDIF}
  frxDsgnIntf, frxRes;


{ TfrxPSQLComponents }

constructor TfrxPSQLComponents.Create(AOwner: TComponent);
begin
  inherited;
  FOldComponents := PSQLComponents;
  PSQLComponents := Self;
end;

destructor TfrxPSQLComponents.Destroy;
begin
  if PSQLComponents = Self then
    PSQLComponents := FOldComponents;
  inherited;
end;

function TfrxPSQLComponents.GetDescription: String;
begin
  Result := 'PSQL';
end;

procedure TfrxPSQLComponents.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FDefaultDatabase) and (Operation = opRemove) then
    FDefaultDatabase := nil;
end;



{ TfrxPSQLDatabase }

constructor TfrxPSQLDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := TPSQLDatabase.Create(nil);
  Component := FDatabase;
end;

destructor TfrxPSQLDatabase.Destroy;
begin
  inherited;
end;

class function TfrxPSQLDatabase.GetDescription: String;
begin
  Result := 'PSQL Database';
end;

function TfrxPSQLDatabase.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

function TfrxPSQLDatabase.GetDatabaseName: String;
begin
  Result := FDatabase.DatabaseName;
end;

function TfrxPSQLDatabase.GetLoginPrompt: Boolean;
begin
  Result := FDatabase.LoginPrompt;
end;

function TfrxPSQLDatabase.GetParams: TStrings;
begin
  Result := FDatabase.Params;
end;

function TfrxPSQLDatabase.GetUserName: string;
begin
  Result := FDatabase.UserName;
end;

function TfrxPSQLDatabase.GetUserPassword: string;
begin
  Result := FDatabase.UserPassword;
end;

procedure TfrxPSQLDatabase.SetConnected(Value: Boolean);
begin
  BeforeConnect(Value);
  FDatabase.Connected := Value;
end;

procedure TfrxPSQLDatabase.SetDatabaseName(const Value: String);
begin
  FDatabase.DatabaseName := Value;
end;

procedure TfrxPSQLDatabase.SetLoginPrompt(Value: Boolean);
begin
  FDatabase.LoginPrompt := Value;
end;

procedure TfrxPSQLDatabase.SetParams(Value: TStrings);
begin
  FDatabase.Params := Value;
end;

procedure TfrxPSQLDatabase.SetUserName(const Value: string);
begin
  FDatabase.UserName := Value;
end;

procedure TfrxPSQLDatabase.SetUserPassword(const Value: string);
begin
  FDatabase.UserPassword := Value;
end;

procedure TfrxPSQLDatabase.SetLogin(const Login, Password: String);
begin
  SetUserName(Login);
  SetUserPassword(Password);
end;


{ TfrxPSQLTable }

constructor TfrxPSQLTable.Create(AOwner: TComponent);
begin
  FTable := TPSQLTable.Create(nil);
  DataSet := FTable;
  SetDatabase(nil);
  inherited;
end;

constructor TfrxPSQLTable.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxPSQLDatabase then
    begin
      SetDatabase(TfrxPSQLDatabase(l[i]));
      break;
    end;
end;

class function TfrxPSQLTable.GetDescription: String;
begin
  Result := 'PSQL Table';
end;

procedure TfrxPSQLTable.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

procedure TfrxPSQLTable.SetDatabase(const Value: TfrxPSQLDatabase);
begin
  FDatabase := Value;
  if Value <> nil then
    FTable.Database := Value.Database
  else if PSQLComponents <> nil then
    FTable.Database := PSQLComponents.DefaultDatabase
  else
    FTable.Database := nil;
end;

function TfrxPSQLTable.GetIndexFieldNames: String;
begin
  Result := FTable.IndexFieldNames;
end;

function TfrxPSQLTable.GetIndexName: String;
begin
  Result := FTable.IndexName;
end;

function TfrxPSQLTable.GetTableName: String;
begin
  Result := FTable.TableName;
end;

procedure TfrxPSQLTable.SetIndexFieldNames(const Value: String);
begin
  FTable.IndexFieldNames := Value;
end;

procedure TfrxPSQLTable.SetIndexName(const Value: String);
begin
  FTable.IndexName := Value;
end;

procedure TfrxPSQLTable.SetTableName(const Value: String);
begin
  FTable.TableName := Value;
end;

procedure TfrxPSQLTable.SetMaster(const Value: TDataSource);
begin
  FTable.MasterSource := Value;
end;

procedure TfrxPSQLTable.SetMasterFields(const Value: String);
begin
  FTable.MasterFields := Value;
end;

procedure TfrxPSQLTable.BeforeStartReport;
begin
  SetDatabase(FDatabase);
end;


{ TfrxPSQLQuery }

constructor TfrxPSQLQuery.Create(AOwner: TComponent);
begin
  FQuery := TPSQLQuery.Create(nil);
  Dataset := FQuery;
  SetDatabase(nil);
  inherited;
end;

constructor TfrxPSQLQuery.DesignCreate(AOwner: TComponent; Flags: Word);
var
  i: Integer;
  l: TList;
begin
  inherited;
  l := Report.AllObjects;
  for i := 0 to l.Count - 1 do
    if TObject(l[i]) is TfrxPSQLDatabase then
    begin
      SetDatabase(TfrxPSQLDatabase(l[i]));
      break;
    end;
end;

class function TfrxPSQLQuery.GetDescription: String;
begin
  Result := 'PSQL Query';
end;

procedure TfrxPSQLQuery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then
    SetDatabase(nil);
end;

procedure TfrxPSQLQuery.SetDatabase(const Value: TfrxPSQLDatabase);
begin
  FDatabase := Value;
  if Value <> nil then
    FQuery.Database := Value.Database
  else if PSQLComponents <> nil then
    FQuery.Database := PSQLComponents.DefaultDatabase
  else
    FQuery.Database := nil;
end;

function TfrxPSQLQuery.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

procedure TfrxPSQLQuery.SetSQL(Value: TStrings);
begin
  FQuery.SQL := Value;
end;

procedure TfrxPSQLQuery.SetMaster(const Value: TDataSource);
begin
  FQuery.DataSource := Value;
end;

procedure TfrxPSQLQuery.UpdateParams;
begin
  frxParamsToTParams(Self, FQuery.Params);
end;

procedure TfrxPSQLQuery.BeforeStartReport;
begin
  SetDatabase(FDatabase);
end;

{$IFDEF QBUILDER}
function TfrxPSQLQuery.QBEngine: TfqbEngine;
begin
  Result := TfrxEnginePSQL.Create(nil);
  TfrxEnginePSQL(Result).FQuery.Database := FQuery.Database;
end;
{$ENDIF}


{$IFDEF QBUILDER}
constructor TfrxEnginePSQL.Create(AOwner: TComponent);
begin
  inherited;
  FQuery := TPSQLQuery.Create(Self);
end;

destructor TfrxEnginePSQL.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TfrxEnginePSQL.ReadFieldList(const ATableName: string;
  var AFieldList: TfqbFieldList);
var
  TempTable: TPSQLTable;
  Fields: TFieldDefs;
  i: Integer;
  tmpField: TfqbField;
begin
  AFieldList.Clear;
  TempTable := TPSQLTable.Create(Self);
  TempTable.Database := FQuery.Database;
  TempTable.TableName := ATableName;
  Fields := TempTable.FieldDefs;
  try
    try
      TempTable.Active := True;
      tmpField:= TfqbField(AFieldList.Add);
      tmpField.FieldName := '*';
      for i := 0 to Fields.Count - 1 do
      begin
        tmpField := TfqbField(AFieldList.Add);
        tmpField.FieldName := Fields.Items[i].Name;
        tmpField.FieldType := Ord(Fields.Items[i].DataType)
      end;
    except
    end;
  finally
    TempTable.Free;
  end;
end;

procedure TfrxEnginePSQL.ReadTableList(ATableList: TStrings);
begin
  ATableList.Clear;
  FQuery.Database.GetTableNames('', False, ATableList);
end;

function TfrxEnginePSQL.ResultDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TfrxEnginePSQL.SetSQL(const Value: string);
begin
  FQuery.SQL.Text := Value;
end;
{$ENDIF}

var ButtonImage: TBitmap;
    Idx: integer;

initialization

  ButtonImage := TBitmap.Create;
  try

    ButtonImage.LoadFromResourceName(HInstance, 'TFRXPSQLDATABASE');
    Idx := frxResources.ObjectImages.Add(ButtonImage, nil);
    frxObjects.RegisterObject1(TfrxPSQLDataBase, nil, '', {$IFDEF DB_CAT}'DATABASES'{$ELSE}''{$ENDIF}, 0, Idx);

    ButtonImage.LoadFromResourceName(HInstance, 'TFRXPSQLTABLE');
    Idx := frxResources.ObjectImages.Add(ButtonImage, nil);
    frxObjects.RegisterObject1(TfrxPSQLTable, ButtonImage, '', {$IFDEF DB_CAT}'TABLES'{$ELSE}''{$ENDIF}, 0, Idx);

    ButtonImage.LoadFromResourceName(HInstance, 'TFRXPSQLQUERY');
    Idx := frxResources.ObjectImages.Add(ButtonImage, nil);
    frxObjects.RegisterObject1(TfrxPSQLQuery, ButtonImage, '', {$IFDEF DB_CAT}'QUERIES'{$ELSE}''{$ENDIF}, 0, Idx);

  finally
    ButtonImage.Free;
  end;

finalization
  frxObjects.UnRegister(TfrxPSQLDataBase);
  frxObjects.UnRegister(TfrxPSQLTable);
  frxObjects.UnRegister(TfrxPSQLQuery);


end.