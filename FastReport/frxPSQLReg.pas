
{******************************************}
{                                          }
{             FastReport v3.0              }
{       PSQL components registration        }
{                                          }

// Created by: MicroOLAP Technologies LTD.
// E-mail: support@microolap.com

{                                          }
{******************************************}

unit frxPSQLReg;

interface

{$I frx.inc}

procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes
{$IFNDEF Delphi6}
, DsgnIntf
{$ELSE}
, DesignIntf, DesignEditors
{$ENDIF}
, frxPSQLComponents;

procedure Register;
begin
  RegisterComponents('FastReport 4.0', [TfrxPSQLComponents]);
end;

end.
