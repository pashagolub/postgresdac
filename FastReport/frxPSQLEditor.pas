
{******************************************}
{                                          }
{             FastReport v3.0              }
{      PSQL components design editors       }
{                                          }

// Created by: MicroOLAP Technologies LTD.
// E-mail: support@microolap.com

{                                          }
{******************************************}

unit frxPSQLEditor;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, SysUtils, Forms, Dialogs, frxPSQLComponents, frxCustomDB,
  frxDsgnIntf, frxRes, PSQLDbTables, PSQLAccess, PSQLTypes, DbLogDlg
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TfrxDatabaseNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    function Edit: Boolean; override;
  end;

  TfrxDatabaseProperty = class(TfrxComponentProperty)
  public
    function GetValue: String; override;
  end;

  TfrxTableNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
    procedure SetValue(const Value: String); override;
  end;

  TfrxIndexNameProperty = class(TfrxStringProperty)
  public
    function GetAttributes: TfrxPropertyAttributes; override;
    procedure GetValues; override;
  end;


{ TfrxDatabaseNameProperty }

function TfrxDatabaseNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paDialog];
end;

function TfrxDatabaseNameProperty.Edit: Boolean;
var
  Login, Password: String;
begin
  with TfrxPSQLDatabase(Component) do
  begin
    Login := UserName;
    Password := UserPassword;
    Result := LoginDialogEx(Database.DatabaseName, Login, Password, FALSE);
    if not Result then Exit;
    UserName := Login;
    UserPassword := Password;
  end;
end;


{ TfrxDatabaseProperty }

function TfrxDatabaseProperty.GetValue: String;
var
  db: TfrxPSQLDatabase;
begin
  db := TfrxPSQLDatabase(GetOrdValue);
  if db = nil then
  begin
    if (PSQLComponents <> nil) and (PSQLComponents.DefaultDatabase <> nil) then
      Result := PSQLComponents.DefaultDatabase.Name
    else
      Result := frxResources.Get('prNotAssigned');
  end
  else
    Result := inherited GetValue;
end;


{ TfrxTableNameProperty }

function TfrxTableNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

procedure TfrxTableNameProperty.GetValues;
begin
  inherited;
  with TfrxPSQLTable(Component).Table do
    if Database <> nil then
      DataBase.GetTableNames('', False, Values);
end;

procedure TfrxTableNameProperty.SetValue(const Value: String);
begin
  inherited;
  Designer.UpdateDataTree;
end;


{ TfrxIndexProperty }

function TfrxIndexNameProperty.GetAttributes: TfrxPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

procedure TfrxIndexNameProperty.GetValues;
var
  i: Integer;
begin
  inherited;
  try
    with TfrxPSQLTable(Component).Table do
      if (TableName <> '') and (IndexDefs <> nil) then
      begin
        IndexDefs.Update;
        for i := 0 to IndexDefs.Count - 1 do
          if IndexDefs[i].Name <> '' then
            Values.Add(IndexDefs[i].Name);
      end;
  except
  end;
end;


initialization
  frxPropertyEditors.Register(TypeInfo(String), TfrxPSQLDataBase, 'UserName',
    TfrxDataBaseNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxPSQLDataBase, 'UserPassword',
    TfrxDataBaseNameProperty);
  frxPropertyEditors.Register(TypeInfo(TfrxPSQLDatabase), TfrxPSQLTable, 'Database',
    TfrxDatabaseProperty);
  frxPropertyEditors.Register(TypeInfo(TfrxPSQLDatabase), TfrxPSQLQuery, 'Database',
    TfrxDatabaseProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxPSQLTable, 'TableName',
    TfrxTableNameProperty);
  frxPropertyEditors.Register(TypeInfo(String), TfrxPSQLTable, 'IndexName',
    TfrxIndexNameProperty);

end.
