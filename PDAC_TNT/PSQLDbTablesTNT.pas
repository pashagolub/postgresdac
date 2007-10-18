unit PSQLDbTablesTNT;

interface

uses
  SysUtils, Classes, DB, PSQLDbTables;

type
  TPSQLQueryTNT = class(TPSQLQuery)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  end;

  TPSQLTableTNT = class(TPSQLTable)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  end;

  TPSQLStoredProcTNT = class(TPSQLStoredProc)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
  end;

procedure Register;

implementation

uses TntDB;
{$R DB_TNT.dcr}

procedure Register;
begin
  RegisterComponents('PostgresDAC TNT Wrappers', [TPSQLTableTNT, TPSQLQueryTNT, TPSQLStoredProcTNT]);
end;

{ TPSQLQueryTNT }
function TPSQLQueryTNT.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftString then
    Result := TTNTStringField
  else
    Result := inherited GetFieldClass(FieldType)
end;

{ TPSQLTableTNT }
function TPSQLTableTNT.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftString then
    Result := TTNTStringField
  else
    Result := inherited GetFieldClass(FieldType)
end;

{ TPSQLStoredProcTNT }
function TPSQLStoredProcTNT.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftString then
    Result := TTNTStringField
  else
    Result := inherited GetFieldClass(FieldType)
end;

end.
