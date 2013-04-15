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
    i: integer;
begin
 SL := TStringList.Create;
 try
   SL.LoadFromFile('TestData\correct_ips.txt');
   for i := 0 to SL.Count - 1 do
     Check(IsValidIP(SL[i]), 'correct ' + SL[i]);
   SL.LoadFromFile('TestData\incorrect_ips.txt');
   for i := 0 to SL.Count - 1 do
     Check(IsValidIP(SL[i]), 'incorrect ' + SL[i]);
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

