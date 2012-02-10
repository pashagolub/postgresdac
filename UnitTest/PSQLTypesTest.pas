unit PSQLTypesTest;

interface

uses
  TestFramework, Classes, SysUtils, PSQLTypes, Math, Windows;

type

  // Test methods for PSQLTypes
  TestPSQLTypes = class(TTestCase)
  public
   procedure HiddenProc;
  published
   procedure CheckIsValidIP;
   procedure CheckMaskSearch;
  end;

implementation

{ TestPSQLTypes }

procedure TestPSQLTypes.CheckIsValidIP;
var SL: TStrings;
    S: string;
begin
 SL := TStringList.Create;
 try
   SL.LoadFromFile('TestData\correct_ips.txt');
   for S in SL do
     Check(IsValidIP(S), 'correct ' + S);
   SL.LoadFromFile('TestData\incorrect_ips.txt');
   for S in SL do
     Check(not IsValidIP(S), 'incorrect ' + S);
 finally
   SL.Free;
 end;
end;

procedure TestPSQLTypes.CheckMaskSearch;
begin
  Check(MaskSearch('mama washed rama', 'mama%'));
end;

procedure TestPSQLTypes.HiddenProc;
begin
  raise Exception.Create('');
end;

initialization

  RegisterTest(TestPSQLTypes.Suite);

end.

