{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclPostgresDACL;

interface

uses
  PSQLCOMP, PSQLConnFrm, PSQLQueryEdit, PSQLfldlinks, PSQLStoredProcFrm, 
  PSQLupdsqled, psqlAboutFrm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PSQLCOMP', @PSQLCOMP.Register);
end;

initialization
  RegisterPackage('dclPostgresDACL', @Register);
end.
