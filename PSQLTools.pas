unit PSQLTools;


interface

Uses Windows, Messages, SysUtils, Classes, Graphics, Controls,
     PSQLDbTables, PSQLAccess, Dialogs;

type
  EPSQLToolsException = class(Exception);

  TPSQLOperation = (poANALYZE, poCLUSTER, poVACUUM, poREINDEX);

  TVacuumOption = (voFULL, voFREEZE, voANALYZE);
  TVacuumOptions = set of TVacuumOption;

  TPSQLTools = class;

  TToolsEvent = procedure(Sender: TPSQLTools; const Operation:TPSQLOperation) of object;

  TPSQLTools = class(TComponent)
   private
    FAbout   : TPSQLDACAbout;
    FDatabase : TPSQLDatabase;
    FQuery    : TPSQLQuery;
    FColumnList : TStrings;
    FOnError : TToolsEvent;
    FOnSuccess : TToolsEvent;
    FPSQLOperation : TPSQLOperation;
    FVacuumOptions : TVacuumOptions;
    FVerbose : boolean;
    FIndexName: string;
    FTableName: string;
    procedure SetDatabase(const Value: TPSQLDatabase);
    procedure SetColumnList(const Value: TStrings);
    function AnalyzeStmt: string;
    function ClusterStmt: string;
    function ReindexStmt: string;
    function VacuumStmt: string;
   protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); Override;
   public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
   published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property ColumnList : TStrings read FColumnList write SetColumnList;
    property Database  : TPSQLDatabase read FDatabase write SetDatabase;
    property IndexName : string read FIndexName write FIndexName;
    property Operation : TPSQLOperation read FPSQLOperation write FPSQLOperation default poANALYZE;
    property TableName : string read FTableName write FTableName;
    property VacuumOptions : TVacuumOptions read FVacuumOptions write FVacuumOptions;
    property Verbose : boolean read FVerbose write FVerbose default False;
    property OnError : TToolsEvent read FOnError write FOnError;
    property OnSuccess : TToolsEvent read FOnSuccess write FOnSuccess;
  end;


implementation

Uses PSQLTypes;

{ TPSQLTools }

function TPSQLTools.AnalyzeStmt: string;
var Target: string;
    i: integer;
begin
 If FTableName > '' then
     Target := FTableName;
 If (FColumnList.Count > 0) and (Target > '') then
  begin
   Target := Target + ' (' + FColumnList[0];
   for i := 1 to FColumnList.Count-1 do
    Target := Target + ', ' + FColumnList[i];
   Target := Target + ')';
  end;
 If FVerbose then
   Result := Format('ANALYZE VERBOSE %s',[Target])
 else
   Result := Format('ANALYZE %s',[Target]);
end;

function TPSQLTools.ClusterStmt: string;
var Target: string;
begin
 If FTableName > '' then
     Target := FTableName;
 If (FIndexName > '') and (Target > '') then
   Target := FIndexName + ' ON ' + Target;
 Result := Format('CLUSTER %s',[Target]);
end;

constructor TPSQLTools.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FQuery := TPSQLQuery.Create(nil);
  FColumnList := TStringList.Create;
  FVacuumOptions := [];
end;

destructor TPSQLTools.Destroy;
begin
  FQuery.Free;
  ColumnList.Free;
  inherited;
end;

function TPSQLTools.Execute: Boolean;
begin
  Result := False;
  case FPSQLOperation of
   poANALYZE: FQuery.SQL.Text := AnalyzeStmt;
   poCLUSTER: FQuery.SQL.Text := ClusterStmt;
   poVACUUM:  with FQuery.SQL  do
       begin
         if FDatabase.TransactionStatus in [trstINTRANS, trstINERROR] then
           FDatabase.Commit;
         FQuery.SQL.Text := VacuumStmt;
       end;
   poREINDEX: FQuery.SQL.Text := ReindexStmt;
  end;
  try
   FQuery.ExecSQL;
   Result := True;
   If Assigned(OnSuccess) then
       FOnSuccess(Self,FPSQLOperation);
  except
   If Assigned(OnError) then
     FOnError(Self,FPSQLOperation);
  end;
end;

procedure TPSQLTools.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  Inherited Notification( AComponent, Operation );
  if (Operation = opRemove) and (AComponent = FDatabase) then
     FDatabase := nil;
end;

function TPSQLTools.ReindexStmt: string;
var Target: string;
begin
 If FIndexName > '' then
  Target := 'INDEX ' + FIndexName
 else
   If FTableName > '' then
     Target := 'TABLE ' + TableName
   else
     IF Assigned(FDatabase) and (FDatabase.DatabaseName > '') then
       Target := 'DATABASE ' + AnsiQuotedStr(FDatabase.DatabaseName,'"');
 If Target = '' then
   raise EPSQLToolsException.Create('Reindex target not assigned')
 else
   Result := Format('REINDEX %s',[Target]);
end;

procedure TPSQLTools.SetColumnList(const Value: TStrings);
begin
  FColumnList.Assign(Value);
end;

procedure TPSQLTools.SetDatabase(const Value: TPSQLDatabase);
begin
  if Value <> FDatabase then
   begin
    FDatabase := Value;
    FQuery.Database := FDatabase;
   end;
end;

function TPSQLTools.VacuumStmt: string;
var Target: string;
begin
 If voFULL in FVacuumOptions  then
   Target := 'FULL';
 If voFREEZE in FVacuumOptions  then
   Target := Target + ' FREEZE';
 If FVerbose then
   Target := Target + ' VERBOSE';
 If voAnalyze in FVacuumOptions then
   Target := Target + ' ANALYZE';
 Result := Format('VACUUM %s %s',[Target, FTableName]);
end;

end.
