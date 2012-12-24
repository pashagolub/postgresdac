unit PSQLBatch;

interface

{SVN revision: $Id$}

Uses  {$IFDEF FPC}LCLIntf,{$ENDIF}{$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils, Classes, Db,
      {$IFDEF DELPHI_6}Variants,{$ENDIF}{StdVCL,} PSQLDbTables, PSQLTypes;

type
  {TPSQLBatchExecute}
  TpsqlBatchAction = (baFail, baAbort, baIgnore, baContinue);

  TPSQLBatchErrorEvent = procedure(Sender: TObject; E: EPSQLDatabaseError; SQLText : String; StatementNo : Integer) of object;

  TPSQLBatchExecute = class(TComponent)
  private
    FAbout   : TPSQLDACAbout;
    FDatabase: TPSQLDatabase;
    FAffectedRows: LongInt;
    FSql: TStringList;
    FDelimiter : Char;
    FAction    : TPSQLBatchAction;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FOnBatchError: TPSQLBatchErrorEvent;
    procedure SetSql(Value: TStringList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function BatchExecSql(Sql: WideString): LongInt;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure ExecSQL;
    property RowsAffected: LongInt read FAffectedRows;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property Action : TPSQLBatchAction read FAction write FAction default baFail;
    property Database: TPSQLDatabase read FDatabase write FDatabase;
    property SQL: TStringList read FSql write SetSql;
    property Delimiter : Char read FDelimiter write FDelimiter default ';';
    property OnBeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
    property OnBatchError: TPSQLBatchErrorEvent read FOnBatchError write FOnBatchError;
  end;

implementation

{TPSQLBatchExecute}
constructor TPSQLBatchExecute.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSql := TStringList.Create;
  FDelimiter := ';';
  FAction := baFail;
end;

destructor  TPSQLBatchExecute.Destroy;
begin
  FSql.Free;
  inherited Destroy;
end;

procedure TPSQLBatchExecute.SetSql(Value: TStringList);
begin
  FSql.Assign(Value);
end;

procedure TPSQLBatchExecute.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase) and (Operation = opRemove) then
    FDatabase   := nil;
end;

function TPSQLBatchExecute.BatchExecSql(Sql: WideString): LongInt;
var
  Buffer, Token, Text: {$IFDEF FPC}ansistring{$ELSE}string{$ENDIF};
  StmtNo : Integer;
begin
  Buffer := Sql;
  Text   := '';
  Result := 0;
  StmtNo := 0;
  while Buffer <> '' do
  begin
    if (Pos(Buffer[1], ' '#9#10#13) <> 0) and (Text <> '') then
      Text := Text + ' ';
    GetToken(Buffer, Token);
    if (Token = FDelimiter) and (Text <> '') then
    begin
      try
        Inc(StmtNo);
        Text := Trim(Text);
        Result := Result + FDatabase.Execute(Text);
        Text := '';
      except
        on E: EPSQLDatabaseError do
        begin
           if E.Message <> '' then E.Message := E.Message + '. ';
           if Assigned(FOnBatchError) then
              FOnBatchError(Self, E,Text,StmtNo);
           case Action of
             baFail:     raise;
             baAbort:    SysUtils.Abort;
             baContinue: if Assigned(FDatabase.OnException) then FDatabase.OnException(Self, E);
             baIgnore:   ;
           end;
           Text :='';
        end;
      end;
    end else
      Text := Text + Token;
  end;
  Text := Trim(Text);
  if Text <> '' then
  try
    Inc(StmtNo);
    Result := Result + FDatabase.Execute(Text);
  except
    on E: EPSQLDatabaseError do
    begin
       if E.Message <> '' then E.Message := E.Message + '. ';
       if Assigned(FOnBatchError) then
          FOnBatchError(Self, E,Text,StmtNo);
       case Action of
         baFail:     raise;
         baAbort:    SysUtils.Abort;
         baContinue: if Assigned(FDatabase.OnException) then FDatabase.OnException(Self, E);
         baIgnore:   ;
       end;
    end;
  end;
end;

procedure TPSQLBatchExecute.ExecSql;
begin
  if Assigned(FDatabase) then
  begin
    if Assigned(FBeforeExecute) then FBeforeExecute(Self);
    FDatabase.Connected := True;
    FAffectedRows := BatchExecSql(FSql.Text);
    if Assigned(FAfterExecute) then FAfterExecute(Self);
  end else
    DatabaseError('Property Database not set!');
end;


end.
