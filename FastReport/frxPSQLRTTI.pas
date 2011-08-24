
{******************************************}
{                                          }
{             FastReport v3.0              }
{          PSQL components RTTI             }
{                                          }

// Created by: MicroOLAP Technologies LTD.
// E-mail: support@microolap.com

{                                          }
{******************************************}

unit frxPSQLRTTI;

interface

{$I frx.inc}

implementation

uses
  Windows, Classes, fs_iinterpreter, frxPSQLComponents
{$IFDEF Delphi6}
, Variants
{$ENDIF};


type
  TFunctions = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;


{ TFunctions }

constructor TFunctions.Create(AScript: TfsScript);
begin
  inherited Create(AScript);
  with AScript do
  begin
    AddClass(TfrxPSQLDatabase, 'TfrxCustomDatabase');
    AddClass(TfrxPSQLTable, 'TfrxCustomTable');
    with AddClass(TfrxPSQLQuery, 'TfrxCustomQuery') do
      AddMethod('procedure ExecSQL', CallMethod);
  end;
end;

function TFunctions.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  Result := 0;

  if ClassType = TfrxPSQLQuery then
  begin
    if MethodName = 'EXECSQL' then
      TfrxPSQLQuery(Instance).Query.ExecSQL
  end
end;


initialization
  fsRTTIModules.Add(TFunctions);

end.
