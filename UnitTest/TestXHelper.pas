unit TestXHelper;

interface

uses
  Classes, SysUtils, PSQLDbTables, PSQLTypes, DUnitX.TestFramework, PSQLFMXConnFrm, FMX.Forms,
  System.UITypes;

type
  TTestXCase = class(TObject)
  public
    procedure Check(condition: Boolean; msg: string = ''); virtual;
  end;

  TTestDBSetup = class(TObject)
  public
    Database: TPSQLDatabase;
    Query: TPSQLQuery;
    destructor Destroy; override;
    procedure SetUp;

  end;

var
  TestDBSetup: TTestDBSetup;

procedure ComponentToFile(Component: TComponent; Name: string);
procedure FileToComponent(Name: string; Component: TComponent);
procedure SetUpTestDatabase(DB: TPSQLDatabase; ConfFileName: string);

implementation

procedure SetUpTestDatabase(DB: TPSQLDatabase; ConfFileName: string);
var
  Frm: TPSQLConnForm;
begin
  if FileExists(ConfFileName) then
  try
    FileToComponent(ConfFileName, DB);
    DB.Close;
  except
    on E: EPSQLDatabaseError do //nothing, failed connection
  end;
  Frm := TPSQLConnForm.Create(nil);
  try
    with Frm do
     begin
      GetDatabaseProperty(DB);
      {$IFDEF MOBILE}
      ShowModal(
        procedure(ModalRes : TModalResult)
        begin
          if ModalRes = mrOk then
            SetDatabaseProperty(DB)
          else
            Application.Terminate;
        end
      );
      {$ELSE}
        if Frm.ShowModal = mrOk then
          SetDatabaseProperty(DB)
        else
          Application.Terminate;
      {$ENDIF};
     end;
  finally
   Frm.Free;
  end;
  DB.Open;
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

{ TTestDBSetup }

destructor TTestDBSetup.Destroy;
begin
  ComponentToFile(Database, 'PDACTestX.conf');
  Query.Free;
  Database.Free;
  inherited;
end;

procedure TTestDBSetup.SetUp;
var i: integer;
begin
  inherited;
  Database := TPSQLDatabase.Create(nil);
  SetUpTestDatabase(Database, 'PDACTest.conf');
  Query := TPSQLQuery.Create(nil);
  Query.Database := TestDBSetup.Database;
  Query.ParamCheck := False;
  TestDBSetup.Database.ErrorVerbosity := evVERBOSE;
  //TestDBSetup.Database.Execute('SET TimeZone to ''America/Caracas'''); // for the complex timezone -04:30
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS requestlive_test ' +
                '(' +
                '  id serial NOT NULL PRIMARY KEY,' + //Serial will create Sequence -> not Required
                '  intf integer NOT NULL,' + //NotNull ->Required
                '  string character varying(100) NOT NULL DEFAULT ''abc'',' + //NotNull + Default -> not Required
                '  datum timestamp without time zone,' + //not Required etc.
                '  notes text,' +
                '  graphic oid,' +
                '  b_graphic bytea,' +
                '  b boolean,' +
                '  floatf real,' +
                '  datef date,' +
                '  timef time,' +
                '  numf numeric' +
                ')');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS required_test ' +
                '(' +
                '  id serial NOT NULL PRIMARY KEY,' + //Serial will create Sequence -> not Required
                '  intf integer NOT NULL,' + //NotNull ->Required
                '  string character varying(100) NOT NULL DEFAULT ''abc'',' + //NotNull + Default -> not Required
                '  datum timestamp without time zone)'); //not Required.
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS testtable' +
                '(  col1 character varying(10) NOT NULL,' +
                '  col2 character varying(10) NOT NULL,' +
                '  CONSTRAINT pk_testtable PRIMARY KEY (col1, col2) )');
  TestDBSetup.Database.Execute('INSERT INTO testtable VALUES (''10'', ''20''), (''11'', ''21'')' );
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS uuid_test_case_table(uuidf uuid NOT NULL PRIMARY KEY)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS geometry_test_case_table(id int4 PRIMARY KEY, p point, c circle, b box, l lseg)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS range_test_case_table('+
                'id int4 PRIMARY KEY, numr numrange, '+
                'intr int4range, dater daterange, '+
                'tsr tsrange, tstzr tstzrange)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE tools_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'sfield TEXT DEFAULT now()::text,' +
                'tfield timestamp DEFAULT now(),'  +
                'rfield real DEFAULT random())');
  TestDBSetup.Database.Execute('CREATE INDEX rfield_idx ON tools_test_case_table (rfield)');
  for i := 0 to 100 do
    TestDBSetup.Database.Execute('INSERT INTO tools_test_case_table DEFAULT VALUES');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE IF NOT EXISTS blobs_test_case_table(' +
                'id SERIAL NOT NULL PRIMARY KEY,'  +
                'byteaf bytea,' +
                'oidf oid,'  +
                'memof text)');
  TestDBSetup.Database.Execute('CREATE TEMP TABLE server_tasks(process text, PID int4, session varchar, session_num int4, memory varchar)');
end;

initialization



finalization

  TestDBSetup.DisposeOf;

end.
