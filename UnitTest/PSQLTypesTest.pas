unit PSQLTypesTest;
{$IFDEF DUNITX}
  {$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, PSQLTypes, TestXHelper
  {$IFNDEF DUNITX}
  , TestFramework, Math, Windows
  {$ELSE}
  , DUnitX.TestFramework, Types
  {$ENDIF};

type

  // Test methods for PSQLTypes
  {$IFDEF DUNITX}[TestFixture]{$ENDIF}
  TestPSQLTypes = class({$IFNDEF DUNITX}TTestCase{$ELSE}TTestXCase{$ENDIF})
  private
  {$IFDEF DUNITX}
    FRSCorrectIP: TResourceStream;
    FRSInCorrectIP: TResourceStream;
  {$ENDIF}
  public
   procedure HiddenProc;
  published
   procedure CheckIsValidIP;
   procedure CheckMaskSearch;
    {$IFDEF DUNITX}
    [SetupFixture]
    procedure SetupFixture;
    [TearDownFixture]
    procedure TearDownFixture;
    {$ENDIF}
  end;

implementation

{ TestPSQLTypes }
procedure TestPSQLTypes.CheckIsValidIP;
var SL: TStrings;
    i: integer;
begin
 SL := TStringList.Create;
 try
   {$IFNDEF DUNITX}
   SL.LoadFromFile('TestData\correct_ips.txt');
   {$ELSE}
   SL.LoadFromStream(FRSCorrectIP);
   {$ENDIF}
   for i := 0 to SL.Count - 1 do
     Check(IsValidIP(SL[i]), 'correct ' + SL[i]);
   {$IFNDEF DUNITX}
   SL.LoadFromFile('TestData\incorrect_ips.txt');
   {$ELSE}
   SL.LoadFromStream(FRSInCorrectIP);
   {$ENDIF}
   for i := 0 to SL.Count - 1 do
     Check(not IsValidIP(SL[i]), 'incorrect ' + SL[i]);
 finally
  {$IFNDEF NEXTGEN}
   SL.Free;
  {$ELSE}
   SL.DisposeOf;
  {$ENDIF}
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

{$IFDEF DUNITX}
procedure TestPSQLTypes.SetupFixture;
begin
  FRSCorrectIP := TResourceStream.Create(HInstance, 'correct_ip', RT_RCDATA);
  FRSCorrectIP.Position := 0;
  FRSInCorrectIP := TResourceStream.Create(HInstance, 'incorrect_ip', RT_RCDATA);
  FRSInCorrectIP.Position := 0;
end;

procedure TestPSQLTypes.TearDownFixture;
begin
  FRSCorrectIP.DisposeOf;
  FRSInCorrectIP.DisposeOf;
end;
{$ENDIF}

initialization
 {$IFNDEF DUNITX}
  RegisterTest(TestPSQLTypes.Suite);
{$ELSE}
  TDUnitX.RegisterTestFixture(TestPSQLTypes);
{$ENDIF}

end.

