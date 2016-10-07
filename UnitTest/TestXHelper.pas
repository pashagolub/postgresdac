unit TestXHelper;

interface

uses Classes, SysUtils, PSQLDbTables, PSQLTypes, DUnitX.TestFramework;

type
  TTestXCase = class(TObject)
  public
    procedure Check(condition: Boolean; msg: string = ''); virtual;
  end;

  TTestDBSetup = class(TObject)
  public
    Database: TPSQLDatabase;
    Query: TPSQLQuery;
  end;

var
  TestDBSetup: TTestDBSetup;

procedure ComponentToFile(Component: TComponent; Name: string);
procedure FileToComponent(Name: string; Component: TComponent);
procedure SetUpTestDatabase(var DB: TPSQLDatabase; ConfFileName: string);

implementation

procedure SetUpTestDatabase(var DB: TPSQLDatabase; ConfFileName: string);
{$IFNDEF DUNITX}
var
  Frm: TPSQLConnForm;
{$ENDIF}
begin
  DB := TPSQLDatabase.Create(nil);
  if FileExists(ConfFileName) then
  try
    FileToComponent(ConfFileName, DB);
    DB.Close;
  except
    on E: EPSQLDatabaseError do //nothing, failed connection
  end;
{$IFNDEF DUNITX}
  Application.CreateForm(TPSQLConnForm, Frm);
  try
    with Frm do
     begin
      GetDatabaseProperty(DB);
      if ShowModal = mrOk then
        SetDatabaseProperty(DB)
      else
        begin
         FreeAndNil(DB);
         raise EInvalidOperation.Create('Test cancelled by operator!');
        end;
     end;
  finally
   Frm.Free;
  end;
  DB.Open;
{$ENDIF}
end;

procedure ComponentToFile(Component: TComponent; Name: string);

var
  BinStream: TMemoryStream;
  StrStream: TFileStream;
begin

  BinStream := TMemoryStream.Create;
  try
    StrStream := TFileStream.Create(Name, fmCreate);
    try
      BinStream.WriteComponent(Component);
      {$IFNDEF NEXTGEN}
      BinStream.Seek(0, soFromBeginning);
      {$ELSE}
      BinStream.Position := 0;
      {$ENDIF}
      ObjectBinaryToText(BinStream, StrStream);
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure FileToComponent(Name: string; Component: TComponent);
var
  StrStream:TFileStream;
  BinStream: TMemoryStream;
begin
  StrStream := TFileStream.Create(Name, fmOpenRead);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      {$IFNDEF NEXTGEN}
      BinStream.Seek(0, soFromBeginning);
      {$ELSE}
      BinStream.Position := 0;
      {$ENDIF}
      BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

{ TTestXCase }

procedure TTestXCase.Check(condition: Boolean; msg: string);
begin
  Assert.IsTrue(Condition, Msg);
end;

end.
