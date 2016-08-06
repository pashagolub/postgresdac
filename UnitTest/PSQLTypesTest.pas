unit PSQLTypesTest;
{$IFDEF DUNITX}
  {$M+}
{$ENDIF}

interface

uses
  Classes, SysUtils, PSQLTypes
  {$IFNDEF DUNITX}
  , TestFramework, Math, Windows
  {$ELSE}
  , DUnitX.TestFramework, Types
  {$ENDIF};

type

  // Test methods for PSQLTypes
  {$IFNDEF DUNITX}[TestFixture]{$ENDIF}
  TestPSQLTypes = class({$IFNDEF DUNITX}TTestCase{$ELSE}TObject{$ENDIF})
  private
    FRSCorrectIP: TResourceStream;
    FRSInCorrectIP: TResourceStream;
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

{$IFDEF DUNITX}
uses TestHelper;
{$ENDIF}
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
     DACCheck(IsValidIP(SL[i]), 'correct ' + SL[i]);
   {$IFNDEF DUNITX}
   SL.LoadFromFile('TestData\incorrect_ips.txt');
   {$ELSE}
   SL.LoadFromStream(FRSInCorrectIP);
   {$ENDIF}
   for i := 0 to SL.Count - 1 do
     DACCheck(not IsValidIP(SL[i]), 'incorrect ' + SL[i]);
 finally
   SL.Free;
 end;
end;

procedure TestPSQLTypes.CheckMaskSearch;
begin
  DACIsTrue(MaskSearch('mama washed rama', 'mama%'));
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
  FRSCorrectIP.Free;
  FRSInCorrectIP.Free;
end;
{$ENDIF}

initialization
 {$IFNDEF DUNITX}
  RegisterTest(TestPSQLTypes.Suite);
{$ELSE}
  TDUnitX.RegisterTestFixture(TestPSQLTypes);
{$ENDIF}

end.

