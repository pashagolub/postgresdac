unit PSQLDirectQuery;

interface

uses
  SysUtils, Classes, PSQLDbTables, PSQLTypes;

type
  EPSQLDirectQueryException = class(Exception);

  TPSQLCustomDirectQuery = class(TComponent)
  private
    { Private declarations }
    FDatabase: TPSQLDatabase;
    FSQL : TStrings;
    FStatement : PPGresult;
    FAbout : TPSQLDACAbout;
    FRecNo: integer;
    FEOF: boolean;
    FBOF: boolean;

    procedure FreeHandle();
    function GetActive(): boolean;
    procedure SetActive(const Value: boolean);
    procedure CheckOpen();//raise an exception if dataset is not active
    function GetRecNo: integer;
    procedure SetRecNo(const Value: integer);
    function GetRecordCount: integer;
    function GetIsEmpty: boolean;
    procedure SetSQL(const Value: TStrings);
    function GetFieldValue(aIndex: integer): string;
    function GetFieldsCount : integer;
    function GetFieldName(aIndex: integer): string;
  protected
    { Protected declarations }
    procedure SetDatabase(Value : TPSQLDatabase);
    function GetDatabase : TPSQLDatabase;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent);override;
    destructor Destroy();override;

    procedure Open();
    procedure Close();
    procedure Refresh();

    procedure First();
    procedure Last();
    procedure Next();
    procedure Prior();
    function MoveBy(aDistance : integer) : integer;

    function FieldValueByFieldName(aFieldName : string) : string;
    function FieldIndexByName(aFieldName : string) : integer;
    function FieldIsNull(aFieldIndex : integer) : boolean;overload;
    function FieldIsNull(aFieldName : string) : boolean;overload;

    property Database : TPSQLDatabase read GetDatabase write SetDatabase;
    property Active : boolean read GetActive write SetActive;
    property SQL : TStrings read FSQL write SetSQL;
    property RecNo : integer read GetRecNo write SetRecNo;//current cursor position
    property RecordCount : integer read GetRecordCount;
    property IsEmpty : boolean read GetIsEmpty;
    property Eof : boolean read FEOF;
    property Bof : boolean read FBOF;
    property FieldsCount : integer read GetFieldsCount; 
    property FieldValues[aIndex : integer]: string read GetFieldValue;
    property FieldNames[aIndex : integer]: string read GetFieldName;
  published
    { Published declarations }
    property About : TPSQLDACAbout read FAbout;
  end;

  TPSQLDirectQuery = class(TPSQLCustomDirectQuery)
  published
    property Database;
    property SQL;
  end;

implementation

uses
  DB, bdeconst, PSQLAccess, DBConsts;

{ TPSQLCustomDirectQuery }

procedure TPSQLCustomDirectQuery.CheckOpen();
begin
  if FStatement = nil then
    raise EPSQLDirectQueryException.Create(SDataSetClosed);
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Close();
begin
  FreeHandle();
end;
//----------------------------------------------------------------------------------------------------------------------
constructor TPSQLCustomDirectQuery.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);

  FStatement := nil;
  FSQL := TStringList.Create();
end;
//----------------------------------------------------------------------------------------------------------------------
destructor TPSQLCustomDirectQuery.Destroy();
begin
  SetDatabase(nil);  //03.04.2008
  FreeHandle();
  FSQL.Free();

  inherited;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.FieldIndexByName(aFieldName: string): integer;
begin
  CheckOpen();

  Result := PQfnumber(FStatement, TNativeConnect(FDatabase.Handle).StringToRaw(aFieldName));
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.FieldIsNull(aFieldIndex: integer): boolean;
begin
  if GetRecordCount() <= 0 then
    raise EPSQLDirectQueryException.Create(SDataSetEmpty);

  if aFieldIndex >= GetFieldsCount() then
    raise EPSQLDirectQueryException.Create(SFieldIndexError);

  Result := PQgetisnull(FStatement, FRecNo, aFieldIndex) = 1;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.FieldIsNull(aFieldName: string): boolean;
var
  i : integer;
begin
  i := FieldIndexByName(aFieldName);
  if i = -1 then
    raise EPSQLDirectQueryException.Create(Format(SFieldNotFound, [aFieldName]));

  Result := FieldIsNull(i);
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.FieldValueByFieldName(aFieldName: string): string;
var
  i : integer;
begin
  i := FieldIndexByName(aFieldName);
  if i = -1 then
    raise EPSQLDirectQueryException.Create(Format(SFieldNotFound, [aFieldName]));

  Result := FieldValues[i];
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.First;
begin
  CheckOpen();

  RecNo := 0;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.FreeHandle;
begin
  if FStatement <> nil then
  begin
    PQclear(FStatement);
    FStatement := nil;
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetActive: boolean;
begin
  Result := FStatement <> nil;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetDatabase: TPSQLDatabase;
begin
  Result := FDatabase;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetFieldName(aIndex: integer): string;
begin
  if aIndex >= GetFieldsCount then
    raise EPSQLDirectQueryException.Create(SFieldIndexError);
   Result := TNativeConnect(FDatabase.Handle).RawToString(PQfname(FStatement, aIndex));
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetFieldsCount: integer;
begin
  CheckOpen();
  Result := PQnfields(FStatement);
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetFieldValue(aIndex: integer): string;
begin
  if GetRecordCount() = 0 then
    raise EPSQLDirectQueryException.Create(SDataSetEmpty);

  if aIndex >= GetFieldsCount() then
    raise EPSQLDirectQueryException.Create(SFieldIndexError);

  Result := TNativeConnect(FDatabase.Handle).RawToString(PQgetvalue(FStatement, FRecNo, aIndex));
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetIsEmpty: boolean;
begin
  Result := GetRecordCount() = 0;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetRecNo: integer;
begin
  Result := FRecNo;
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.GetRecordCount: integer;
begin
  CheckOpen();
  Result := PQntuples(FStatement);
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Last();
begin
  CheckOpen();
  FRecNo := Pred(GetRecordCount());
end;
//----------------------------------------------------------------------------------------------------------------------
function TPSQLCustomDirectQuery.MoveBy(aDistance: integer): integer;
var
  NewRecNo: integer;
begin
  CheckOpen();

  NewRecNo := FRecNo + aDistance;

  if NewRecNo >= GetRecordCount() then
    FRecNo := Pred(GetRecordCount())
  else if NewRecNo < 0 then
    FRecNo := 0;

  Result := NewRecNo - FRecNo;

  RecNo := NewRecNo;

  if EOF or BOF then
    Result := 0;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Next;
begin
  CheckOpen();

  RecNo := RecNo + 1;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Open();
var NC: TNativeConnect;
begin
  if FStatement <> nil then
    exit;//already opened, use Refresh() if you want to re-read data

  if FDatabase = nil then
    raise EPSQLDirectQueryException.Create(SDatabaseNameMissing);

  if Trim(FSQL.Text) = EmptyStr then
    raise EPSQLDirectQueryException.Create(SEmptySQLStatement);

  NC := TNativeConnect(FDatabase.Handle);

  FStatement := {$IFDEF M_DEBUG}PSQLAccess.{$ENDIF}PQexec(NC.Handle, NC.StringToRaw(FSQL.Text));
  if PQresultStatus(FStatement) <> PGRES_TUPLES_OK then
  begin
    FreeHandle();
    raise EPSQLDatabaseError.Create(FDatabase.Engine(), 0);
  end;

  RecNo := 0;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Prior;
begin
  CheckOpen();

  RecNo := RecNo - 1;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.Refresh();
//mi:2007-04-27 nice method :)
begin
  Close();
  Open();
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.SetActive(const Value: boolean);
begin
  if Value then
    Open()
  else
    Close();
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.SetDatabase(Value: TPSQLDatabase);
begin
  if Active then
    Close();

  if Assigned(FDatabase) then
    FDatabase.UnregisterDirectQuery(Self);

  FDatabase := Value;

  if Assigned(FDatabase) then
    FDatabase.RegisterDirectQuery(Self);
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.SetRecNo(const Value: integer);
begin
  CheckOpen();

  FEOF := Value >= GetRecordCount();
  FBOF := Value < 0;

  if FEOF or FBOF then
    Exit;

  FRecNo := Value;
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TPSQLCustomDirectQuery.SetSQL(const Value: TStrings);
begin
  if FSQL.Text <> Value.Text then
  begin
    Close();
    FSQL.BeginUpdate();
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate();
    end;
  end;
end;
//----------------------------------------------------------------------------------------------------------------------

end.
