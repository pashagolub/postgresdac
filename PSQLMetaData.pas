{$I PSQLdac.inc}
unit PSQLMetaData;

{SVN revision: $Id$}

interface

Uses Classes, SysUtils, Db, PSQLTypes, Math,
     {$IFDEF DELPHI_6}Variants,{$ENDIF} PSQLDbTables,
     PSQLConsts, DBConsts, PSQLAccess;


type

  TTablePriv = (tpSELECT, tpINSERT, tpUPDATE,
                tpDELETE, tpRULE, tpREFERENCES,
                tpTRIGGER);
  TTablePrivs = set of TTablePriv;

  TDBPriv    = (dbpCREATE, dbpTEMP);
  TDBPrivs   = set of TDBPriv;

  TFuncPriv  = (fpEXECUTE);
  TFuncPrivs = set of TFuncPriv;

  TLangPriv  = (lpUSAGE);
  TLangPrivs = set of TLangPriv;

  TSchemaPriv = (spCREATE, spUSAGE);
  TSchemaPrivs = set of TSchemaPriv;

  TTblSpacePriv = (tspCREATE);
  TTblSpacePrivs = set of TTblSpacePriv;

  TPSQLObjectsType = (otDatabases, otTables, otFunctions, otTablespaces,
                 otLanguages, otSchemas);

  EPSQLUserException = class(Exception);

  TPSQLUser = class(TPSQLDataSet)
   private
    FAbout: TPSQLDACAbout;
    FUserName: string;
    FShowSystemObjects: boolean;
    FObjectsType: TPSQLObjectsType;
    FPrepared: Boolean;
    FCanCreateDB: boolean;
    FSuperUser: boolean;
    FCanUpdateSysCatalogs: boolean;
    FUserID: integer;
    FAccountValidUntil: string;
    function CreateCursor(GenHandle: Boolean): HDBICur;
    function GetCursor(GenHandle: Boolean): HDBICur;
    procedure FreeStatement;            
    procedure SetUserName(const Value: string);
    procedure SetShowSystemObjects(const Value: boolean);
    procedure SetObjectsType(const Value: TPSQLObjectsType);
    procedure SetPrepare(const Value: Boolean);
    procedure SetPrepared(Value: Boolean);
    procedure PrepareStmt;
    procedure SetDummyInt(const Value: integer);
    procedure SetDummyBool(const Value: boolean);
    procedure SetDummyStr(const Value: string);
   protected
    function CreateHandle: HDBICur; override;
    procedure Disconnect; Override;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AffectedRows: integer;
    function Engine: TPSQLEngine; override;
    procedure Prepare;
    procedure UnPrepare;
    property Handle;
    property StmtHandle;
    property Prepared: Boolean read FPrepared write SetPrepare;
   published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property ShowSystemObjects: boolean read FShowSystemObjects write SetShowSystemObjects;
    property UserName: string read FUserName write SetUserName;
    property ObjectsType: TPSQLObjectsType read FObjectsType write SetObjectsType;
    property SuperUser: boolean read FSuperUser write SetDummyBool stored False;
    property UserID: integer read FUserID write SetDummyInt stored False;
    property CanCreateDB: boolean read FCanCreateDB write SetDummyBool stored False;
    property CanUpdateSysCatalogs: boolean read FCanUpdateSysCatalogs
                                write SetDummyBool stored False;
    property AccountValidUntil: string read FAccountValidUntil
                                write SetDummyStr stored False;
   end;

Const
   sqlPrivStmts: array[TPSQLObjectsType] of string =
        (sqlShowDatabasePrivelegesWithGrantOptsEx,  //  otDatabases,
         sqlShowTablesPrivelegesWithGrantOptsEx,    //  otTables,
         sqlShowFunctionsPrivilegesWithGrantOptsEx, //  otFuntions,
         sqlShowTablespacePrivelegesWithGrantOptsEx,//  otTablespaces,
         sqlShowLanguagesPrivilegesWithGrantOptsEx, //  otLanguages,
         sqlShowSchemaPrivelegesWithGrantOptsEx     //  otSchemas
         );


implementation



{ TPSQLMetaData }

constructor TPSQLUser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSuperUser := False;
  FCanCreateDB := False;
  FCanUpdateSysCatalogs := False;
  FUserID := -1;
  FAccountValidUntil := '';
end;

function TPSQLUser.CreateCursor(GenHandle: Boolean): HDBICur;
begin
  if (FUserName <> '') then
  begin
    Prepare;
    Result := GetCursor(GenHandle);
  end else
    raise EPSQLUserException.Create('Username property is empty!');
end;

function TPSQLUser.CreateHandle: HDBICur;
begin
  Result := CreateCursor(True);
end;

destructor TPSQLUser.Destroy;
begin
  Destroying;
  Disconnect;
  inherited Destroy;
end;

type
  THackDatabase = class(TPSQLDatabase);

function TPSQLUser.Engine: TPSQLEngine;
begin
  Result := THackDatabase(DataBase).Engine;
end;

procedure TPSQLUser.FreeStatement;
var
   S: HDBIStmt;
begin
  S:= StmtHandle;
  if StmtHandle <> nil then Engine.QFree(S);
  FSuperUser := False;
  FCanCreateDB := False;
  FCanUpdateSysCatalogs := False;
  FUserID := -1;
  FAccountValidUntil := '';
  FPrepared := False;
end;

function TPSQLUser.GetCursor(GenHandle: Boolean): HDBICur;
var
  PCursor: phDBICur;
  AffRows: integer;
begin
  Result := NIL;
  if GenHandle then
    PCursor := @Result else
    PCursor := NIL;
  Check(Engine, Engine.GetUserProps(Database.Handle, FUserName, FSuperUser,
                  FCanCreateDB, FCanUpdateSysCatalogs,
                  FUserID, FAccountValidUntil));
  Check(Engine, Engine.QExec(StmtHandle, PCursor, AffRows));
end;

procedure TPSQLUser.Prepare;
begin
  SetPrepared(True);
end;

procedure TPSQLUser.PrepareStmt;
Var Q:string;
    DBh : HDBIDB;
//    hStmt: hDBIStmt;
begin
 DBh := DBHandle;
// hStmt := StmtHandle;
 Check(Engine,Engine.QAlloc(DBh, hDBIStmt(FHandle)));
// StmtHandle := hStmt;
 try
  Q := StringReplace(sqlPrivStmts[FObjectsType],'%s',FUserName,[rfReplaceAll]);
  If not FShowSystemObjects then
    Q:= StringReplace(Q,'%NoSys',sqlNoSysObjects,[rfReplaceAll])
  else
    Q:= StringReplace(Q,'%NoSys','',[rfReplaceAll]);
  Q := Q + 'ORDER BY 1';
  Check(Engine, Engine.QPrepare(StmtHandle,Q));
 except
  Engine.QFree(hDBIStmt(FHandle));
//  StmtHandle := NIL;
  raise;
 end;
end;

procedure TPSQLUser.SetShowSystemObjects(const Value: boolean);
begin
 if FShowSystemObjects <> Value then
  begin
   Disconnect;
   FShowSystemObjects := Value;
  end;
end;

procedure TPSQLUser.SetObjectsType(const Value: TPSQLObjectsType);
begin
 if ObjectsType <> Value then
  begin
   Disconnect;
   FObjectsType := Value;
  end;
end;

procedure TPSQLUser.SetPrepare(const Value: Boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

procedure TPSQLUser.SetPrepared(Value: Boolean);
begin
  if Handle <> nil then DatabaseError(SDataSetOpen, Self);
  if FPrepared <> Value then
  begin
    if Value then
      try
        PrepareStmt;
        FPrepared := True;
      except
        FreeStatement;
        raise;
      end
    else FreeStatement;
  end;
end;

procedure TPSQLUser.SetUserName(const Value: string);
begin
 if FUserName <> Value then
  begin
   Disconnect;
   FUserName := Value;
  end;
end;

procedure TPSQLUser.UnPrepare;
begin
  SetPrepared(False);
end;

function TPSQLUser.AffectedRows: integer;
begin
  Result := GetRecordCount;
end;

procedure TPSQLUser.Disconnect;
begin
  inherited Disconnect;
  UnPrepare;
end;

procedure TPSQLUser.SetDummyInt(const Value: integer);
begin
//dummy method for published properties;
end;


procedure TPSQLUser.SetDummyBool(const Value: boolean);
begin
//dummy method for published properties;
end;

procedure TPSQLUser.SetDummyStr(const Value: string);
begin
//dummy method for published properties;
end;

end.
