{$I pSQLDAC.inc}
                                      
unit PSQLAccess;

{SVN revision: $Id$}

{$T-}

interface

uses Classes, {$IFDEF FPC}LCLIntf,{$ENDIF} Db, PSQLTypes, Math,
     {$IFDEF DELPHI_9}DbCommon,{$ELSE}PSQLCommon,{$ENDIF}
     {$IFDEF DELPHI_6}Variants,{$ENDIF}
     {$IFDEF FPC}Variants,{$ENDIF}
     SysUtils;
type
  {Forward declaration}
  TNativeConnect = class;

{****************************************************************************}
{                        Error handler                                       }
{****************************************************************************}
  EPSQLException =  Class(EAbort)
    private
      FPSQL : TNativeConnect;
      FPSQLErrorCode : Word;
      FBDEErrorCode : Word;                
      FBDE          : Boolean;
      FPSQLErrorMsg : String;
      FPSQLErrorPos : String;
      FPSQLErrorContext: String;
      FPSQLErrorseverity:string;
      FPSQLErrorsqlstate:string;
      FPSQLErrordetail:string;
      FPSQLErrorprimary:string;
      FPSQLErrorhint:string;
      FPSQLErrorinternalpos:string;
      FPSQLErrorinternalquery:string;
      FPSQLErrorsourcefile:string;
      FPSQLErrorsourceline:string;
      FPSQLErrorsourcefunc:string;

      function GetNativeErrorMsg : String;
      function GetNativeErrorPos : String;
      function GetNativeErrorContext : String;
      function GetNativeErrorseverity : String;
      function GetNativeErrorsqlstate  : String;
      function GetNativeErrordetail    : String;
      function GetNativeErrorprimary   : String;
      function GetNativeErrorhint      : String;
      function GetNativeErrorinternalpos     : String;
      function GetNativeErrorinternalquery   : String;
      function GetNativeErrorsourcefile      : String;
      function GetNativeErrorsourceline      : String;
      function GetNativeErrorsourcefunc      : String;

    public
      Constructor CreateBDE(ECode : Word);
      constructor CreateBDEMsg(ECode : Word; Const EMessage : String);
      Constructor Create(PSQL : TNativeConnect);
      Constructor CreateMsg(PSQL : TNativeConnect; Const ErrorMsg : String );
      property PSQLErrorCode : word read FPSQLErrorCode;
      property PSQLErrorMsg : String read GetNativeErrorMsg;
      property BDEErrorCode : Word read FBDEErrorCode;
      property BDEErrors : Boolean read FBDE;
      //error field properties added by Tony Caduto 5/17/2006
      property PSQLErrorPos : String read GetNativeErrorPos;
      property PSQLErrorContext : String read GetNativeErrorContext;
      property PSQLErrorseverity: String read GetNativeErrorseverity;
      property PSQLErrorsqlstate: String read GetNativeErrorsqlstate;
      property PSQLErrordetail: String read GetNativeErrordetail;
      property PSQLErrorprimary: String read GetNativeErrorprimary;
      property PSQLErrorhint: String read GetNativeErrorhint;
      property PSQLErrorinternalpos: String read GetNativeErrorinternalpos;
      property PSQLErrorinternalquery: String read GetNativeErrorinternalquery;
      property PSQLErrorsourcefile: String read GetNativeErrorsourcefile;
      property PSQLErrorsourceline: String read GetNativeErrorsourceline;
      property PSQLErrorsourcefunc: String read GetNativeErrorsourcefunc;

  end;

{****************************************************************************}
{                       TNativeConnect                                       }
{****************************************************************************}
  TNativeConnect = Class(TObject)
  private
    FHandle                   : PPGconn;
    FSystem                   : Boolean;
    FLastOperationTime        : cardinal;
    FBlobTransactionInProgress: boolean;
    FServerVersion            : string;
    FIntServerVersion         : integer;
    FNativeByteaFormat        : TNativeByteaFormat;
    FErrorpos                 : string;
    FErrorContext             : string;
    FErrorseverity            : string;
    FErrorsqlstate            : string;
    FErrordetail              : string;
    FErrorprimary             : string;
    FErrorhint                : string;
    FErrorinternalpos         : string;
    FErrorinternalquery       : string;
    FErrorsourcefile          : string;
    FErrorsourceline          : string;
    FErrorsourcefunc          : string;
    FConnectString            : string;
    FDirectConnectString      : string;
    function GetBackendPID : integer;
    function IsTransactionActive: boolean;
    function GetTransactionStatus: TTransactionStatusType;
  protected
    FTransState : eXState;  { Transaction end control xsActive, xsInactive }
    FTransLevel : eXILType;  { Transaction isolation levels }
    function GetCommitOperation: Boolean; {Get commit operation}
  public
    Tables : TContainer; {List of Tables}
    FLoggin : Boolean; {Loggin flag}
    DBOptions : TDBOptions; {Connection parameters}

    constructor Create;
    destructor  Destroy; Override;

    class function Ping(Params: TStrings): TPingStatus;

    procedure DirectExecute(SQL: String);
    procedure ProcessDBParams(Params : TStrings);
    procedure InternalConnect(ConnParams: TStrings = nil); {Login to database}
    procedure InternalDisconnect; {Logout from database}
    procedure Reset; {reset connection to server}
    function Rollback: boolean; {Rollback transaction}
    function Commit: boolean; {Commit transaction}
    procedure CancelBackend(PID: Integer);
    procedure CheckResult; overload;{Check result last operation}
    procedure CheckResult(FStatement: PPGresult); overload;
    function GetErrorText: String; {Get Error text}
    function Success: Boolean;
    procedure StoredProcParams(pszPName: string; ProcOID: cardinal; List:TList);
    procedure StoredProcList(pszWild : string; List : TStrings);
    procedure TableList(pszWild : string; SystemTables: Boolean; List : TStrings);
    procedure UserList(pszWild : string; List : TStrings);
    procedure SchemaList(pszWild : string; SystemSchemas: Boolean; List : TStrings);
    procedure TablespaceList(pszWild : string; List : TStrings);
    procedure DatabaseList(pszWild : string; List : TStrings);
    procedure OpenTable(pszTableName: string; pszIndexName: string; iIndexId: Word;
                        eOpenMode: DBIOpenMode;eShareMode: DBIShareMode;var hCursor: hDBICur;
                        AnOptions: TPSQLDatasetOptions;
                        Limit, Offset : Integer);
    procedure QueryAlloc(var hStmt: hDBIStmt);
    procedure QueryPrepare(var hStmt: hDBIStmt;Query : String);
    procedure BeginTran(eXIL: eXILType; var hXact: hDBIXact);
    procedure BeginBLOBTran;
    procedure RollbackBLOBTran;
    procedure CommitBLOBTran;
    procedure EndTran(hXact : hDBIXact; eEnd : eXEnd);
    procedure GetTranInfo(hXact : hDBIXact; pxInfo : pXInfo);
    procedure QExecDirect(pszQuery : String; phCur: phDBICur; var AffectedRows : LongInt);
    procedure OpenFieldList(pszTableName: string; pszDriverType: string; bPhyTypes: Boolean; var hCur: hDBICur);
    procedure OpenIndexList(pszTableName: string; pszDriverType: string; var hCur: hDBICur);
    function GetCharSet: string;
    procedure GetCharSetList(var List: TStrings);
    procedure SetCharSet(var ACharSet: string);
    function GetTimeout: cardinal;
    function SetTimeout(const Timeout: cardinal): cardinal;
    procedure SetErrorVerbosity(const ErrorVerbosity: TErrorVerbosity);
    function GetServerVersion: string;
    function GetserverVersionAsInt: integer;
    procedure GetUserProps(const UserName: string; var SuperUser, CanCreateDB,
                        CanUpdateSysCatalogs: boolean; var UserID: integer;
                        var ValidUntil: string);
    procedure GetDBProps(const DB: string;
                        var Owner, Tablespace: string;
                        var IsTemplate: boolean;
                        var DBOid: cardinal; var Comment: string);
    procedure GetTableProps(const TableName: string;
                        var Owner, Comment, Tablespace: string;
                        var HasOIDs: boolean;
                        var TableOid: cardinal);
//    procedure ResetConnection; for future use
    procedure EmptyTable(hCursor : hDBICur; pszTableName : string);
    procedure TableExists(pszTableName : string);
    procedure AddIndex(hCursor: hDBICur; pszTableName: string; pszDriverType: string; var IdxDesc: IDXDesc; pszKeyviolName: string);
    procedure DeleteIndex(hCursor: hDBICur; pszTableName: string; pszDriverType: string; pszIndexName: string; pszIndexTagName: string; iIndexId: Word);
    procedure CreateTable(bOverWrite: Boolean; var crTblDsc: CRTblDesc);

    property IsolationLevel : eXILType Read FTransLevel;
    property Handle : PPGconn read FHandle write FHandle;
    property BackendPID : Integer read GetBackendPID;
    property LastOperationTime: cardinal read FLastOperationTime;
    property InTransaction: boolean read IsTransactionActive;
    property TransactionStatus: TTransactionStatusType read GetTransactionStatus;
    property BlobTransactionInProgress: boolean read FBlobTransactionInProgress;
    property NativeByteaFormat: TNativeByteaFormat read FNativeByteaFormat;

    function SelectStringDirect(pszQuery : string; var IsOk : boolean; aFieldNumber : integer):string; overload;
    function SelectStringDirect(pszQuery : string; var IsOk : boolean; pszFieldName : string):string; overload;
    function SelectStringsDirect(pszQuery : string; aList : TStrings; aFieldNumber : integer):string; overload;
    function SelectStringsDirect(pszQuery : string; aList : TStrings; pszFieldName : string):string; overload;


    function IsUnicodeUsed: boolean;
    function IsSSLUsed: boolean;

    function RawToString(S: PAnsiChar): string;
    function StringToRaw(S: string): PAnsiChar; //need to be free by StrDispose
    function StringToRawS(S: string): AnsiString;
  end;

  {Postgres Engine}
  TPSQLEngine =  Class(TBaseObject)
    private
      FDatabase                : hDBIDb;
      FNativeStatus            : Integer;
      FNativeMsg               : string;
      FNativeErrorPos          : string;
      FNativeErrorContext      : string;
      FNativeErrorseverity     : string;
      FNativeErrorsqlstate     : string;
      FNativeErrordetail       : string;
      FNativeErrorprimary      : string;
      FNativeErrorhint         : string;
      FNativeErrorinternalpos  : string;
      FNativeErrorinternalquery: string;
      FNativeErrorsourcefile   : string;
      FNativeErrorsourceline   : string;
      FNativeErrorsourcefunc   : string;
      function GetDatabase: hDBIDb;
      procedure SetDatabase(H : hDBIDb);
    Public
      Constructor Create(P : TObject; Container : TContainer);
      Destructor Destroy; Override;
      Property Status: Integer Read  FNativeStatus;
      Property MessageStatus : String read FNativeMsg;
      property errorpos:string read FnativeErrorPos;
      property errorcontext:string read FnativeErrorContext;

      property Errorseverity:string read FnativeErrorseverity;
      property Errorsqlstate:string read FnativeErrorsqlstate;
      property Errordetail:string read FnativeErrordetail;
      property Errorprimary:string read FnativeErrorprimary;
      property Errorhint:string read FnativeErrorhint;
      property Errorinternalpos:string read FnativeErrorinternalpos;
      property Errorinternalquery:string read FnativeErrorinternalquery;
      property Errorsourcefile:string read FnativeErrorsourcefile;
      property Errorsourceline:string read FnativeErrorsourceline;
      property Errorsourcefunc:string read FnativeErrorsourcefunc;

      Property Database: hDBIDb Read  GetDatabase Write SetDatabase;
      function IsSqlBased(hDb: hDBIDB): Boolean;
      function Ping(Params: TStrings; var PingResult: TPingStatus): DBIResult;
      function OpenDatabase(Params : TStrings; UseSinleLineConnInfo: boolean; var hDb: hDBIDb): DBIResult;
      function CloseDatabase(var hDb : hDBIDb) : DBIResult;
      function OpenTable(hDb: hDBIDb; pszTableName: string; pszDriverType: string; pszIndexName: string; pszIndexTagName: string;
               iIndexId: Word; eOpenMode: DBIOpenMode; eShareMode: DBIShareMode; exltMode: XLTMode; bUniDirectional : Boolean;
               pOptParams: Pointer; var hCursor: hDBICur; AnOptions: TPSQLDatasetOptions; Limit, Offset : Integer): DBIResult;
      function OpenStoredProcParams(hDb: hDBIDb;pszPName: string; ProcOID:cardinal; List : TList): DBIResult;
      function OpenStoredProcList(hDb: hDBIDb; pszWild: string; List : TStrings): DBIResult;
      function OpenTableList(hDb: hDBIDb; pszWild: string; SystemTables: Boolean; List : TStrings): DBIResult;
      function OpenUserList(hDb: hDBIDb; pszWild: string; List : TStrings): DBIResult;
      function OpenSchemaList(hDb: hDBIDb; pszWild: string; SystemSchemas: Boolean; List : TStrings): DBIResult;
      function OpenTablespaceList(hDb: hDBIDb; pszWild: string; List : TStrings): DBIResult;
      function SetToBookMark(hCur: hDBICur; pBookMark : Pointer) : DBIResult;
      function CompareBookMarks(hCur: hDBICur; pBookMark1, pBookMark2 : Pointer;var CmpBkmkResult : CmpBkmkRslt): DBIResult;
      function GetNextRecord(hCursor: hDBICur;eLock: DBILockType;pRecBuff: Pointer;pRecProps: pRECProps): DBIResult;
      function CloseCursor(hCursor: hDBICur): DBIResult;
      function PutField(hCursor: hDBICur;FieldNo: Word;PRecord: Pointer;pSrc: Pointer): DBIResult;
      function OpenBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;eOpenMode: DBIOpenMode): DBIResult;
      function GetBlobSize(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;var iSize: Longint): DBIResult;
      function GetBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;iOffSet: Longint;iLen: Longint;pDest: Pointer;var iRead: Longint): DBIResult;
      function PutBlob(hCursor : hDBICur; PRecord : Pointer; FieldNo : Word; iOffSet : Longint; iLen : Longint; pSrc : Pointer): DBIResult;
      function TruncateBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;iLen: Longint): DBIResult;
      function FreeBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word): DBIResult;
      function CloseBlob(hCursor: hDBICur; FieldNo: Word): DBIResult;
      function BeginTran(hDb: hDBIDb; eXIL: eXILType; var hXact: hDBIXact): DBIResult;
      function EndTran(hDb: hDBIDb; hXact: hDBIXact; eEnd : eXEnd): DBIResult;
      function GetTranInfo(hDb: hDBIDb;hXact: hDBIXact; pxInfo: pXInfo): DBIResult;
      function GetTranStatus(hDb: hDBIDb; var TranStatus: TTransactionStatusType): DBIResult;
      function GetEngProp(hObj: hDBIObj;iProp: Longint;PropValue: Pointer;iMaxLen: Word;var iLen: Word): DBIResult;
      function SetEngProp(hObj: hDBIObj;iProp: Longint;PropValue: Longint): DBIResult;
      function GetVchkDesc(hCursor: hDBICur;iValSeqNo: Word; var pvalDesc: VCHKDesc): DBIResult;
      function GetCursorProps(hCursor: hDBICur;var curProps: CURProps): DBIResult;
      function GetObjFromObj(Source: hDBIObj; eObjType: DBIOBJType; var hObj: hDBIObj): DBIResult;
      function GetFieldDescs(hCursor: hDBICur; var pfldDesc: TFLDDescList): DBIResult;
      function SetToBegin(hCursor: hDBICur): DBIResult;
      function SetToEnd(hCursor: hDBICur): DBIResult;
      function RelRecordLock(hCursor: hDBICur;bAll: Boolean): DBIResult;
      function ReadBlock(hCursor: hDBICur; var iRecords: Integer;  pBuf: Pointer): DBIResult;
      function InitRecord(hCursor: hDBICur;PRecord: Pointer ): DBIResult;
      function InsertRecord(hCursor: hDBICur;eLock: DBILockType;PRecord: Pointer): DBIResult;
      function AppendRecord(hCursor: hDBICur;PRecord:Pointer): DBIResult;
      function ModifyRecord(hCursor: hDBICur;OldRecord,PRecord:Pointer;bFreeLock: Boolean;ARecno: LongInt): DBIResult;
      function DeleteRecord(hCursor: hDBICur;PRecord:Pointer): DBIResult;
      function SettoSeqNo(hCursor: hDBICur;iSeqNo: Longint): DBIResult;
      function GetPriorRecord(hCursor: hDBICur;eLock:DBILockType;PRecord: Pointer;pRecProps: pRECProps): DBIResult;
      function GetRecord(hCursor: hDBICur;eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps): DBIResult;
      function GetBookMark(hCur: hDBICur;pBookMark: Pointer): DBIResult;
      function GetRecordCount(hCursor: hDBICur;Var iRecCount: Longint): DBIResult;
      function ForceReread(hCursor: hDBICur): DBIResult;
      function GetField(hCursor: hDBICur;FieldNo: Word;PRecord: Pointer;pDest: Pointer;var bBlank: Boolean): DBIResult;
      function AddFilter(hCursor: hDBICur;iClientData: Longint;iPriority: Word;bCanAbort: Boolean;pcanExpr: pCANExpr;pfFilter: pfGENFilter;var hFilter: hDBIFilter): DBIResult;
      function DropFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
      function ActivateFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
      function DeactivateFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
      function GetErrorEntry(uEntry: Word;var ulNativeError: Longint;pszError: PChar): DBIResult;
      function GetErrorString(rslt: DBIResult;ErrorMsg: String): DBIResult;
      function QExecDirect(hDb: hDBIDb; pszQuery: String;phCur: phDBICur; var AffectedRows : LongInt): DBIResult;
      function QAlloc(hDb: hDBIDb;eQryLang: DBIQryLang;var hStmt: hDBIStmt): DBIResult;
      function QPrepare(hStmt: hDBIStmt;pszQuery: String): DBIResult;
      function QExec(hStmt: hDBIStmt; phCur: phDBICur; var AffectedRows: integer): DBIResult;
      function QPrepareExt(hDb: hDBIDb;eQryLang: DBIQryLang;pszQuery: PChar;propBits: Word;var hStmt: hDBIStmt): DBIResult;
      function QFree(var hStmt: hDBIStmt): DBIResult;
      function QPrepareProc (hDb: hDBIDb; pszProc: PChar; hParams: pointer; var hStmt: hDBIStmt): DBIResult;
      function QSetProcParams (hStmt: hDBIStmt; Params: TParams): DBIResult;
      function QGetProcParams (hStmt: hDBIStmt; Params: TParams): DBIResult;
      function QuerySetParams(hStmt: hDBIStmt;Params : TParams; SQLText : String): DBIResult;
      function CheckError : DBIResult;
      function GetDatabases(hDb: hDBIdb; pszWild: string; List : TStrings):DBIResult;
      function GetCharacterSet(hDb : hDBIDb; var CharSet : string):DBIResult;
      function GetCharacterSets(hDb : hDBIDb; List: TStrings):DBIResult;
      function SetCharacterSet(hDb : hDBIDb; var CharSet : string): DBIResult;
      function SetErrorVerbosity(hDb : hDBIDb; const ErrorVerbosity: TErrorVerbosity): DBIResult;
      function GetCommandTimeout(hDb : hDBIDb; var Timeout : cardinal): DBIResult;
      function SetCommandTimeout(hDb : hDBIDb; var Timeout : cardinal): DBIResult;
      function OpenFieldList(hDb: hDBIDb; pszTableName: string; pszDriverType: string; bPhyTypes: Boolean; var hCur: hDBICur): DBIResult;
      function OpenIndexList(hDb: hDBIDb; pszTableName: string; pszDriverType: string; var hCur: hDBICur): DBIResult;
      function EmptyTable(hDb: hDBIDb; hCursor : hDBICur; pszTableName : string; pszDriverType : string): DBIResult;
      function SetRange(hCursor : hDBICur;bKeyItself: Boolean;iFields1: Word;iLen1: Word;pKey1: Pointer;bKey1Incl: Boolean;
                        iFields2: Word;iLen2: Word;pKey2: Pointer;bKey2Incl: Boolean): DBIResult;
      function ResetRange(hCursor : hDBICur) : DBIResult;
      function SwitchToIndex(hCursor : hDBICur; pszIndexName, pszTagName : string; iIndexId : Word; bCurrRec : Boolean) : DBIResult;
      function ExtractKey(hCursor: hDBICur;PRecord: Pointer;pKeyBuf: Pointer): DBIResult;
      function GetRecordForKey(hCursor: hDBICur; bDirectKey: Boolean; iFields: Word; iLen: Word; pKey: Pointer; pRecBuff: Pointer; AStrictConformity: boolean = False): DBIResult;
      function AddIndex(hDb: hDBIDb;hCursor: hDBICur;pszTableName: string;pszDriverType: string;var IdxDesc: IDXDesc;pszKeyviolName: string): DBIResult;
      function DeleteIndex(hDb: hDBIDb;hCursor: hDBICur;pszTableName: string;pszDriverType: string;pszIndexName: string;pszIndexTagName: string;iIndexId: Word): DBIResult;
      function GetIndexDesc(hCursor: hDBICur;iIndexSeqNo: Word;var idxDesc: IDXDesc): DBIResult;
      function GetIndexDescs(hCursor: hDBICur; idxDescs: TIDXDescList): DBIResult;
      function TranslateRecordStructure(pszSrcDriverType: PChar; iFlds: Word; pfldsSrc: pFLDDesc; pszDstDriverType: PChar; pszLangDriver: PChar;pfldsDst: pFLDDesc; bCreatable: Boolean): DBIResult;
      function TableExists(hDb: hDBIDb; pszTableName: string): DBIResult;
      function CreateTable(hDb: hDBIDb; bOverWrite: Boolean; var crTblDsc: CRTblDesc): DBIResult;
      function AcqTableLock(hCursor: hDBICur; eLockType: word; bNoWait: boolean): DBIResult;
      function SetToKey(hCursor: hDBICur;eSearchCond: DBISearchCond;bDirectKey: Boolean;iFields: Word;iLen: Word;pBuff: Pointer): DBIResult;
      function CloneCursor(hCurSrc: hDBICur;bReadOnly: Boolean;bUniDirectional: Boolean;var   hCurNew: hDBICur): DBIResult;
      function SetToCursor(hDest, hSrc : hDBICur) : DBIResult;
      function OpenPGNotify(hDb: hDBIDb; var hNotify: hDBIObj): DBIResult;
      function ClosePGNotify(var hNotify : hDBIObj) : DBIResult;
      function ListenTo(hNotify : hDBIObj; pszEvent: string) : DBIResult;
      function UnlistenTo(hNotify : hDBIObj; pszEvent: string) : DBIResult;
      function DoNotify(hNotify : hDBIObj; pszEvent: string) : DBIResult;
      function DoNotifyEx(hNotify : hDBIObj; pszChannel: string; pszPayload: string) : DBIResult;
      function CheckEvents(hNotify : hDBIObj; var Pid : Integer; var pszOutPut, pszPayload : String)  : DBIResult;
      function GetBackendPID(hDb: hDBIDb; var PID: Integer): DBIResult;
      function GetServerVersion(hDb: hDBIDb; var ServerVersion: string): DBIResult;
      function GetUserProps(hDb: hDBIDb; const UserName: string;
                var SuperUser, CanCreateDB, CanUpdateSysCatalogs: boolean;
                var UserID: integer; var ValidUntil: string):DBIResult;
      function GetDBProps(hDB: hDBIDB; const DB: string;
                        var Owner, Tablespace: string;
                        var IsTemplate: boolean;
                        var DBOid: cardinal; var Comment: string):DBIResult;
      function GetTableProps(hDB: hDBIDB; const TableName: string; var Owner,
                        Comment, Tablespace: string; var HasOIDs: boolean;
                        var TableOid: cardinal):DBIResult;
      function GetFieldOldValue(hCursor: hDBICur; AFieldName: string; AParam: TParam): DBIResult;
      function GetFieldValueFromBuffer(hCursor: hDBICur; PRecord: Pointer; AFieldName: string; AParam: TParam; const UnchangedAsNull: boolean): DBIResult;
      function GetLastInsertId(hCursor: hDBICur; const FieldNum: integer; var ID: integer): DBIResult;
      function GetFieldTypeOID(hCursor: hDBICur; const FieldNum: integer): cardinal;
      function GetFieldOrigin(hCursor: hDBICur; const FieldNum: integer): string;

      function CheckBuffer(hCursor: hDBICur; PRecord: Pointer): DBIResult;
      function Reset(hDb: hDBIDb): DBIResult;
      function CancelBackend(hDb: hDBIdb; PID: Integer): DBIResult;
      function SelectStringDirect(hDb: hDBIDb;
                                  pszQuery : PChar;
                                  var IsOk : boolean;
                                  var aResult : string;
                                  aFieldNumber : integer):DBIResult;overload;
      function SelectStringDirect(hDb: hDBIDb;
                                  pszQuery : PChar;
                                  var IsOk : boolean;
                                  var aResult : string;
                                  aFieldName : string):DBIResult;overload;
      function SelectStringsDirect(hDb: hDBIDb;
                                  pszQuery : PChar;
                                  aList : TStrings;
                                  aFieldNumber : integer):DBIResult;overload;
      function SelectStringsDirect(hDb: hDBIDb;
                                  pszQuery : PChar;
                                  aList : TStrings;
                                  aFieldName : string):DBIResult;overload;

     end;

  /////////////////////////////////////////////////////////
  //               Forward declaration                   //
  /////////////////////////////////////////////////////////
  TNativeDataSet = Class;
  //////////////////////////////////////////////////////////
  //Class       : TPSQLField
  //Description : PSQL Field Description
  //////////////////////////////////////////////////////////



  TPSQLField = Class(TCollectionItem)
    private
      FDesc      : FldDesc;
      FValCheck  : VCHKDesc;
      FBuffer    : Pointer;
      FData      : Pointer;
      FStatus    : PFieldStatus;
      FArray     : Boolean;
      FNativeBLOBType: TNativeBLOBType;
      function GetLocalSize : Word;
      procedure SetLocalSize(S : Word);
      function GetLocalType : Word;
      procedure SetLocalType(S : Word);
      function GetFieldName : String;
      procedure SetFieldName(Const Value : String);
      procedure SetBuffer(PRecord : Pointer);
      function GetChanged : Boolean;
      procedure SetChanged(Flag : Boolean);
      function GetNull : Boolean;
      procedure SetNull(Flag : Boolean);
      function GetFieldDefault : string;//mi
      procedure SetFieldDefault(aStr : string);
      function GetNativeDataset: TNativeDataSet;
      function GetNativeConnect: TNativeConnect;
    public
      constructor CreateField(Owner : TCollection; P : FldDesc; P1 :VCHKDesc; FNum, LType, LSize : Word; isArray : Boolean);

      function FieldValue: PAnsiChar;
      function FieldValueAsStr: string; //this will be used in SQLs;

      Property Buffer : Pointer Read FBuffer Write SetBuffer;
      Property Data : Pointer Read FData;
      Property DataOffset : Word Read  FDesc.iOffset Write  FDesc.iOffset;
      Property Description : FLDDesc Read FDesc Write FDesc;
      Property ValCheck : VCHKDesc Read FValCheck Write FValCheck;
      Property FieldChanged : Boolean Read GetChanged Write SetChanged;
      Property FieldNull : Boolean Read GetNull Write SetNull;
      Property FieldStatus : PFieldStatus Read FStatus;
      Property NullOffset : Word Read FDesc.iNullOffset Write FDesc.iNullOffset;
      Property FieldNumber : Word Read FDesc.iFldNum Write FDesc.iFldNum;
      Property FieldName : String Read GetFieldName Write SetFieldName;
      Property FieldType : Word Read   FDesc.iFldType Write  FDesc.iFldType;
      Property FieldSubType : Word Read   FDesc.iSubType Write  FDesc.iSubType;
      Property FieldUnits1 : integer Read   FDesc.iUnits1 Write  FDesc.iUnits1;
      Property FieldUnits2 : integer Read   FDesc.iUnits2 Write  FDesc.iUnits2;
      Property FieldLength : Word Read   FDesc.iLen Write  FDesc.iLen;
      property FieldDefault: string read GetFieldDefault write SetFieldDefault;//mi
      property NativeType : Word Read   GetLocalType Write  SetLocalType;
      property NativeSize : Word Read   GetLocalSize Write  SetLocalSize;
      property FieldArray : Boolean Read  FArray write FArray;
      property NativeBLOBType: TNativeBLOBType read FNativeBLOBType
                write FNativeBlobType;
      property NativeDataset : TNativeDataSet read GetNativeDataset;
      property NativeConnect : TNativeConnect read GetNativeConnect;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLFields
  //Description : List PSQL Fields for current cursor
  //////////////////////////////////////////////////////////
   TPSQLFields = Class(TCollection)
    private
      FTable : TNativeDataSet;
      function GetField(Index : Integer) : TPSQLField;
    function GetNativeConnect: TNativeConnect;
    Public
      Constructor Create(Table : TNativeDataSet);
      function AddField(P : FldDesc; P1 :VCHKDesc; FNum, LType, LSize : Word; isArray : Boolean): TPSQLField;
      Property Field[Index : Integer] : TPSQLField Read  GetField; Default;
      procedure SetFields(PRecord : Pointer);
      function FieldNumberFromName(SearchName : PChar) : Integer;

      property NativeDataset : TNativeDataSet read FTable;
      property NativeConnect : TNativeConnect read GetNativeConnect;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLIndex
  //Description : PSQL Index Description
  //////////////////////////////////////////////////////////
  TPSQLIndex = Class(TCollectionItem)
    private
      FDesc      : IDXDesc;
    function GetIndexName: string;
    procedure SetIndexName(const Value: string);
    Public
      Constructor CreateIndex(Owner : TCollection; P : pIDXDesc);
      Property Description : IDXDesc Read FDesc Write FDesc;
    Published
      Property IndexNumber : Word Read FDesc.iIndexID Write FDesc.iIndexID;
      Property IndexName   : string Read GetIndexName Write SetIndexName;
      Property Primary     : WordBool Read FDesc.bPrimary Write FDesc.bPrimary;
      Property Unique      : WordBool Read FDesc.bUnique Write FDesc.bUnique;
      Property Descending  : WordBool Read FDesc.bDescending Write FDesc.bDescending;
      Property FldsInKey   : Word Read FDesc.iFldsInKey Write  FDesc.iFldsInKey;
      Property KeyLen      : Word Read FDesc.iKeyLen Write FDesc.iKeyLen;
      Property BlockSize   : Word Read FDesc.iBlockSize Write FDesc.iBlockSize;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLIndexes
  //Description : List PSQL Indexes for current cursor
  //////////////////////////////////////////////////////////
   TPSQLIndexes = Class(TCollection)
    private
      FTable : TNativeDataSet;
      FUpdated: boolean;
      function GetIndex(Index : Integer) : TPSQLIndex;
      function FindByName(Name :String): TPSQLIndex;
      procedure SetNeedUpdate(const Value: boolean);
    Public
      Constructor Create(Table : TNativeDataSet);
      Property mIndex[Index : Integer] : TPSQLIndex Read  GetIndex; Default;
      function SetIndex(Name,Fields : String;aPrimary,aUnique,aDesc : Boolean): integer;
      function FieldNumberFromName(SearchName : PChar) : Integer;
      property Updated: boolean read FUpdated write SetNeedUpdate;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLFilter
  //Description : Filtered object
  //////////////////////////////////////////////////////////
  TPSQLFilter = class(TObject)
  protected
    function PerformCANOp(AOperator : CANOp; AOp1, AOp2 : Variant) : Variant;
    function PerformCanConst(ANode : PCANConst; ValuesStart : Pointer; Var FldType : TFldType) : Variant;
    function TimeOf(const ADateTime: TDateTime): TDateTime;
  private
    FDataSet    : TNativeDataSet;
    FExpression : pCANExpr;
    FActive     : Boolean;
    FExprSize   : Word;
    FRecBuff    : Pointer;
    FPfFilter   : pfGENFilter;
    FClientData : Longint;
    function GetNodeStart : Integer;
    function GetLiteralPtr(AOffset: Word):Pointer;
    function GetNodeByOffset(AOffSet : Integer) : PCanNode;
    function UnaryNode(ANode : PCANUnary) : Variant;
    function BinaryNode(ANode : PCANBinary) : Variant;
    function CompareNode(ANode : PCANCompare) : Variant;
    function FieldNode(ANode : pCANField) : Variant;
    function GetNodeValue(AOffSet : Integer) : Variant;
    function CalcExpression(ANode : PCanNode) : Variant;
    function ListOfValues(ANode : pCANListElem): Variant;
    function PerformLikeCompare(Const Value, Mask : String; CaseSen : Boolean) : Boolean;
    function PerformInCompare(AOp1, AOp2 : Variant) : Boolean;
    Property NodeStart : Integer  Read GetNodeStart;
  public
    Constructor Create(Owner : TNativeDataSet; AClientData : Longint; Exp : pCANExpr; pfFilt : pfGENFilter);
    Destructor Destroy; Override;
    function GetFilterResult(PRecord : Pointer) : Variant;
    Property Active : Boolean Read  FActive  Write FActive;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLNative
  //Description : PSQL Native Field Description
  //////////////////////////////////////////////////////////
  TPSQLNative = Class(TCollectionItem)
    private
      FDesc      : TPGField_Info;
    Public
      Constructor CreateNative(Owner : TCollection; P : PPGField_Info);
      Property Description : TPGField_Info Read FDesc Write FDesc;
    Published
      Property NativeNumber  : Integer Read FDesc.FieldIndex Write FDesc.FieldIndex;
      Property NativeName    : String Read FDesc.FieldName Write FDesc.FieldName;
      Property NativeType    : cardinal Read FDesc.FieldType Write FDesc.FieldType;
      Property NativeSize    : Integer Read FDesc.FieldSize Write FDesc.FieldSize;
      Property NativeMaxSize : Integer Read FDesc.FieldMaxSize Write FDesc.FieldMaxSize;
      Property NativeDefault : String Read FDesc.FieldDefault Write  FDesc.FieldDefault;
  end;

  //////////////////////////////////////////////////////////
  //Class       : TPSQLNatives
  //Description : List PSQL Native Fields for current cursor
  //////////////////////////////////////////////////////////
   TPSQLNatives = Class(TCollection)
    private
      FTable : TNativeDataSet;
      function GetNative(Index : Integer) : TPSQLNative;
    Public
      Constructor Create(Table : TNativeDataSet);
      Property Field_Info[Index : Integer] : TPSQLNative Read  GetNative; Default;
      procedure SetNative(aIndex : Integer; aName : String; aType,aSize,aMaxSize : Integer; aDefault : String);
  end;

  //////////////////////////////////////////////////////////
  //Class       : TNativeDataSet
  //Description : Base class for All Objects
  //////////////////////////////////////////////////////////
    TNativeDataSet = Class(TObject)
    private
      function GetFieldTypType(Index: integer): AnsiChar;
    protected
      RecNo         : LongInt; {Record Nomber}
      FOMode        : DBIOpenMode;  {Open mode}
      FFilteredRecordCount  : LongInt; {Record count}
      FStatement    : PPGresult; {Handle PSQL Cursor }
      FFilters      : TContainer; {Filters list}
      FFilterActive : Boolean;  {is Active filter for Query }
      FReFetch      : Boolean;  {Batch Insert allows}
      FFieldDescs   : TPSQLFields;
      FIndexDescs   : TPSQLIndexes;
      FNativeDescs  : TPSQLNatives; {Native field Description}
      FLastOperationTime: cardinal;
      FKeyNumber    : SmallInt;
      FIndexName    : string;
      FPrimaryKeyNumber: SmallInt;
      FGetKeyDesc   : Boolean;
      FKeyDesc      : IDXDesc;
      FHasOIDs      : boolean;
      Ranges        : Boolean;
      FRecSize      : Integer;
      FConnect      : TNativeConnect;
      FOpen         : Boolean; {is Active Query}
      FAffectedRows : LongInt; {Affected Rows}
      FBookOfs        : Integer;
      FRecordState    : TRecordState;
      FLastDir        : TDir;
      FCurrentBuffer  : Pointer;
      FInternalBuffer : Pointer;
      FIsLocked       : Boolean;
      FReRead         : Boolean;
      OrderClause     : TStrings;
      RangeClause     : TStrings;
      StandartClause  : TStrings;
      LimitClause     : TStrings;
      AutoReExec      : Boolean;
      FLimit          : Integer;
      FOffset         : Integer;
      MasterCursor    : Pointer;
      FBlobHandle     : cardinal;
      FlocalBHandle   : Integer;
      FBlobOpen       : Boolean;
      FOIDTable       : TList;
      FSystemNeed     : Boolean;
      Ferror_pos      : integer;
      Ferror_line     : integer;
      FFieldMinSizes  : array of integer; //to decrease FieldMinSize routine access
      FFieldTypType   : AnsiString; //to store pg_type.typtype
      FSortingIndex   : array of integer; //filled with SortBy method
      FSortingFields  : string; //"fieldname" ASC|DESC, ...
      FOptions        : TPSQLDatasetOptions;
      //////////////////////////////////////////////////////////
      //            PROTECTED METHODS                         //
      //////////////////////////////////////////////////////////
      procedure SetInternalBuffer(Buffer : Pointer);
      function GetInternalBuffer: Pointer;
      function GetCurrentBuffer: Pointer;
      procedure SetCurrentBuffer(PRecord : Pointer);
      procedure SetBufferAddress(P : Pointer);
      procedure SetKeyNumber(newValue: SmallInt);
      function FieldOffset(iField: Integer): Word;
      function GetBookMarkSize: Integer;
      function GetIndexCount: Integer;
      procedure SetBufBookmark;
      procedure SetRecordNumber(RecNom : Longint);
      function GetRecordNumber : Longint;
      function GetRecCount: LongInt;
      procedure FillDefs(SL: TStrings);
      procedure InitFieldDescs;
      procedure CheckFilter(PRecord : Pointer);
      procedure InternalCommit;
      procedure FirstRecord; virtual;
      procedure LastRecord;
      procedure NextRecord();
      procedure PrevRecord();
      procedure CurrentRecord(ARecNo : LongInt);
      procedure GetWorkRecord(eLock : DBILockType; PRecord : Pointer);
      procedure GetRecordNo(var iRecNo : Longint);
      procedure LockRecord(eLock : DBILockType);
      function FilteredRecord(PRecord : Pointer) :  Boolean;
      procedure UpdateFilterStatus;
      function FieldCount : Integer;
    	procedure InternalSortBy(const Fields: array of Integer; const IsReverseOrder : array of boolean);
      function GetRecNo: integer;
      procedure InternalReadBuffer;
      function GetTableName: string;
      procedure SetTableName(Name : string);
      function CheckUniqueKey(var KeyNumber : integer): Boolean;
      procedure GetKeys(Unique: Boolean;var FieldList: TFieldArray; var FieldCount: Integer);
      function GetLOUnlinkSQL(ObjOID: cardinal): string; overload;
      function GetLOUnlinkSQL(ObjOID: string): string; overload;
      function GetDeleteSQL(Table: string; PRecord: Pointer): string;
      function GetInsertSQL(Table: string; PRecord: Pointer; ReturnUpdated: boolean = False): string;
      function GetUpdateSQL(Table: string; OldRecord, PRecord: Pointer; ReturnUpdated: boolean = False): String;
      procedure FreeBlobStreams(PRecord: Pointer);
      function FieldVal(FieldNo: Integer; FieldPtr : Pointer):String;
      //////////////////////////////////////////////////////////
      //            PSQL FIELD PARAMS                        //
      //////////////////////////////////////////////////////////
      function FieldName(FieldNum: Integer): String;
      function FieldIndex(FieldName: String): Integer;
      function FieldSize(FieldNum: Integer): Integer;
      function FieldMaxSize(FieldNum: Integer): Integer;
      function FieldMaxSizeInBytes(FieldNum: Integer): Integer;
      function FieldMinSize(FieldNum: Integer): Integer;
      function FieldType(FieldNum: Integer): cardinal;
      function FieldTable(FieldNum: integer): cardinal;
      function FieldOrigin(FieldNum: integer): string;
      function FieldPosInTable(FieldNum: integer): Integer;
      function FieldIsNull(FieldNum: Integer): Boolean;
      function Field(FieldNum: Integer): string;
      function FieldBuffer(FieldNum: Integer): PAnsiChar;
      function FieldByName(FieldName: String): string;
      function  GetSQLClause: string;
      function GetBufferSize : Word; Virtual;
      function GetWorkBufferSize : Word; virtual;
      procedure GetNativeDesc(FieldNo : Integer; var P : FldDesc; var P1: VCHKDesc; Var LocType, LocSize : Word; var LocArray: Boolean);
      procedure NativeToDelphi(P: TPSQLField;PRecord: Pointer; pDest: Pointer; var bBlank: Boolean);
      procedure DelphiToNative(P: TPSQLField;PRecord: Pointer;pSrc: Pointer);
      procedure CheckParam(Exp : Boolean;BDECODE : Word);
      function GetRecordSize: Integer;
      function GetFieldInfo(Index : Integer) : TPGFIELD_INFO;
      procedure ReOpenTable;
      procedure ClearIndexInfo;
     private
      FTableName: string;
      Property KeyNumber: SmallInt Read FKeyNumber Write SetKeyNumber;
      property RecordCount : LongInt Read GetRecCount;
      Property Fields : TPSQLFields Read  FFieldDescs;
      Property RecordSize : Integer read GetRecordSize;
      Property FieldInfo[Index: Integer]:TPGFIELD_INFO Read GetFieldInfo;
      Property BookMarkSize : Integer Read  GetBookMarkSize;
      Property BufferAddress : Pointer Write SetBufferAddress;
      Property CurrentBuffer : Pointer Read  GetCurrentBuffer Write SetCurrentBuffer;
      Property InternalBuffer : Pointer Read  GetInternalBuffer Write SetInternalBuffer;
      Property IndexCount : Integer Read  GetIndexCount;
      //insert, update, delete stuff
      function UuidValue(P : Pointer; NeedQuote: boolean = True): string;
      function StrValue(P : Pointer; NeedQuote: boolean = True): string;
      function MemoValue(P : Pointer; NeedQuote: boolean = True): string;
      function BlobValue(P : Pointer; Fld: TPSQLField; NeedEscape: boolean = True): string; overload;
      function BlobValue(MS: TStream; isBytea: boolean; NeedEscape: Boolean = True): string; overload;
      procedure ReadBlock(var iRecords: Integer; pBuf: Pointer);
    Public
      SQLQuery : String;
      ROWID    : OID;
      isQuery  : boolean;
      Constructor Create(PSQL : TNativeConnect;
                         Container : TContainer;
                         AnOptions: TPSQLDatasetOptions;
                         AName, IndexName : string;
                         Index : Word;
                         Limit, Offset : Integer;
                         ASystem: Boolean = False);
      Destructor Destroy; Override;
      procedure CompareBookMarks(pBookMark1, pBookMark2 : Pointer; var CmpBkmkResult : CmpBkmkRslt);
      procedure GetBookMark(P : Pointer);
      function GetLastInsertID(const KeyNumber: integer):integer;
      procedure Execute;
      procedure OpenTable;
      procedure GetField(FieldNo : Word; PRecord : Pointer; pDest : Pointer; var bBlank : Boolean);
      procedure PutField(FieldNo: Word;PRecord : Pointer; PSrc:Pointer);
      procedure CloseTable;
      procedure GetVchkDesc(iValSeqNo: Word; var pvalDesc: VCHKDesc);
      procedure GetCursorProps(var curProps : CURProps);
      procedure GetFieldDescs(var pFDesc : TFLDDescList);
      procedure GetRecordCount(var iRecCount : Longint); virtual;
      procedure GetNextRecord(eLock : DBILockType; PRecord : Pointer; pRecProps : pRECProps); Virtual;
      procedure SetToRecord(RecNo : LongInt);
      procedure SetToBookmark(P : Pointer); virtual;
      procedure GetRecord(eLock : DBILockType; PRecord : Pointer; pRecProps : pRECProps);
      procedure GetPriorRecord(eLock : DBILockType; PRecord : Pointer; pRecProps : pRECProps);
      procedure AddFilter(iClientData: Longint;iPriority: Word;bCanAbort: Boolean;pcanExpr: pCANExpr;pfFilter: pfGENFilter; var hFilter : hDBIFilter);
      procedure DropFilter(hFilter: hDBIFilter);
      procedure ActivateFilter(hFilter : hDBIFilter);
      procedure DeactivateFilter(hFilter : hDBIFilter);
      procedure GetProp(iProp: Longint;PropValue: Pointer;iMaxLen: Word;var iLen: Word);
      procedure SetProp(iProp: Longint; PropValue : Longint);
      procedure SetToBegin; Virtual;
      procedure SetToEnd;
      procedure ForceReread;
      procedure InitRecord(PRecord : Pointer);
      procedure InsertRecord(eLock : DBILockType; PRecord : Pointer);
      procedure AppendRecord(PRecord : Pointer);
      procedure ModifyRecord(OldRecord,PRecord : Pointer; bFreeLock : Boolean;ARecNo : LongInt);
      procedure DeleteRecord(PRecord : Pointer);
      //-->blob stuff
      procedure OpenBlob(PRecord: Pointer;FieldNo: Word;eOpenMode: DBIOpenMode);
      procedure FreeBlob(PRecord: Pointer;FieldNo: Word);
      procedure CloseBlob(FieldNo: Word);
      procedure GetBlobSize(PRecord : Pointer; FieldNo : Word; var iSize : Longint);
      procedure GetBlob(PRecord : Pointer; FieldNo : Word; iOffSet : Longint; iLen : Longint; pDest : Pointer; var iRead : Longint);
      procedure PutBlob(PRecord: Pointer;FieldNo: Word;iOffSet: Longint;iLen: Longint; pSrc : Pointer);
      procedure TruncateBlob(PRecord : Pointer; FieldNo : Word; iLen : Longint);
      //<--blob stuff
      procedure QuerySetParams(Params : TParams; SQLText : String);
      procedure StoredProcSetParams(Params: TParams);
      procedure StoredProcGetParams(Params: TParams);
      procedure RelRecordLock(bAll: Boolean);
      procedure ExtractKey(PRecord: Pointer;pKeyBuf: Pointer);
      procedure GetRecordForKey(bDirectKey: Boolean; iFields: Word; iLen: Word; pKey: Pointer; pRecBuff: Pointer; AStrictConformity: boolean = False);
      function findrows(const Fields: array of Integer; const SearchFields:array of String; ACaseSen : Boolean; APartLen : Integer; AStrictConformity: boolean = False):int64;
      function SetRowPosition(iFields : Integer; LID : Int64; pRecBuffer : Pointer):Boolean;
      procedure GetIndexDesc(iIndexSeqNo : Word; var idxDesc : IDXDesc);
      procedure GetIndexDescs(Descs : TIDXDescList);
      procedure SetRange(bKeyItself : Boolean; iFields1 : Word; iLen1 : Word; pKey1 : Pointer;
                bKey1Incl : Boolean; iFields2 : Word; iLen2 : Word; pKey2 : Pointer; bKey2Incl : Boolean);
      procedure ResetRange;
      procedure SwitchToIndex(pszIndexName : string; pszTagName : string; iIndexId : Word; bCurrRec : Boolean);
      procedure SettoSeqNo(iSeqNo: Longint);
      procedure EmptyTable;
      procedure AddIndex(var IdxDesc: IDXDesc; pszKeyviolName : string);
      procedure DeleteIndex(pszIndexName: string; pszIndexTagName: string; iIndexId: Word);
      procedure AcqTableLock(eLockType: word; bNoWait: boolean);
      procedure SetToKey(eSearchCond: DBISearchCond; bDirectKey: Boolean;iFields: Word;iLen: Word;pBuff: Pointer);
      procedure Clone(bReadOnly: Boolean;bUniDirectional: Boolean;var hCurNew: hDBICur);
      procedure SetToCursor(hDest : hDBICur);

      property RecordNumber : LongInt Read GetRecordNumber Write SetRecordNumber;
      property RecordState: TRecordState  Read  FRecordState Write FRecordState;
      property TableName : string Read  GetTableName Write SetTableName;

      property Options: TPSQLDatasetOptions read FOptions write FOptions;

      property FieldTypTypes[Index: integer]: AnsiChar read GetFieldTypType;

      procedure FieldOldValue(AFieldName: string; var AParam: TParam);
      procedure FieldValueFromBuffer(PRecord: Pointer; AFieldName: string; var AParam: TParam; const UnchangedAsNull: boolean);

      property IsLocked: boolean read FIsLocked write FIsLocked;
      property LastOperationTime: cardinal read FLastOperationTime;
      function CheckCanLive : boolean; //pasha_golub 14.07.06
      function HasFieldTimeZone(const FldNum: integer):boolean;
 		  procedure SortBy(FieldNames : string);
      function IsSortedLocally: boolean;

    //mi:2008-08-27 flag to prevent record buffer storing while reading BLOB field data
    private
      FPreventRememberBuffer : boolean;
    public
      property PreventRememberBuffer : boolean read FPreventRememberBuffer write FPreventRememberBuffer;
      property Connect: TNativeConnect read FConnect;
end;

 TIndexList = Class(TNativeDataSet)
 private
    Descs     : TIDXDescList;
    Items     : Word;
    Position  : Word;
 Public
    constructor Create(PSQL : TNativeConnect; D : TIDXDescList; TotalCount : Word);
    destructor Destroy; Override;
    procedure SetToBegin; Override;
    procedure GetNextRecord(eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps); override;
    procedure GetIdxDesc(Precord: PIdxDesc);
    function GetBufferSize : Word; Override;
    function GetWorkBufferSize : Word; Override;
    procedure SetToBookmark(P : Pointer); override;
    procedure GetRecordCount(Var iRecCount : Longint); override;
 end;

 TFieldList = Class(TNativeDataSet)
 private
    Descs   : TFLDDescList;
    Items     : Word;
    Position  : Word;
 public
    constructor Create(PSQL : TNativeConnect; D : TFLDDescList; TotalCount : Word);
    destructor Destroy; Override;
    procedure SetToBegin; Override;
    function GetBufferSize : Word; Override;
    procedure GetNextRecord(eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps); override;
    procedure GetFLDDesc(PRecord: pFLDDesc);
    function GetWorkBufferSize : Word; Override;
    procedure SetToBookmark(P : Pointer); override;
    procedure GetRecordCount(Var iRecCount : Longint); override;
 end;

  TNativePGNotify = class
  protected
    FConnect : TNativeConnect;
    FHandle  : PPGnotify;
    procedure InternalExecute(Sql: string);
  public
    constructor Create(AConnect: TNativeConnect);
    destructor Destroy; override;
    procedure ListenTo(Event: string);
    procedure UnlistenTo(Event: string);
    procedure DoNotify(Event: string);
    procedure DoNotifyEx(Channel: string; Payload: string);
    function CheckEvents(var PID : Integer; var Payload: string): string;
    property Handle: PPGnotify read fHandle;
  end;

function AdjustNativeField(iField : TPSQLField; Src, Dest : Pointer; Var Blank : Boolean) : Word;
function AdjustDelphiField(iField : TPSQLField; Src, Dest : Pointer) : Word;
procedure PSQLException(PSQL : TNativeConnect);
procedure PSQLExceptionMsg(PSQL : TNativeConnect; Const ErrorMsg : String );


function BDETOPSQLStr(Field : TFieldDef): String;
function SQLCreateIdxStr(Index : TPSQLIndex;TableName : String;Flds : TPSQLFields): String;
function QuoteIdentifier(IdentifierName: string): string;

function _PQExecute(AConnection: TNativeConnect; AQuery: string): PPGResult;
function _PQExecuteParams(AConnection: TNativeConnect; AQuery: string; AParams: TParams): PPGResult;

{$IFDEF M_DEBUG}
function PQExec(Handle: PPGconn; AQuery: PAnsiChar): PPGresult;
procedure LogDebugMessage(const MsgType, Msg: string);

var SessionStart: cardinal;
{$ENDIF}

{$IFDEF UNDER_DELPHI_6}
function StrToFloat(const S: string;
  const FormatSettings: TFormatSettings): Extended;
function FloatToStr(Value: Extended;
  const FormatSettings: TFormatSettings): string;
function FormatDateTime(const Format: string; DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime; const FormatSettings: TFormatSettings);
{$ENDIF}

implementation

uses PSQLDbTables, PSQLMonitor,
     {$IFNDEF DELPHI_5}StrUtils,{$ENDIF}
     {$IFDEF MSWINDOWS}Windows,{$ENDIF}
     {$IFNDEF FPC}DbConsts,{$ENDIF}
     PSQLExtMask, PSQLFields;

{**************************************************************************}
{                     Utility Objects                                      }
{**************************************************************************}

{$IFDEF TRIAL}
function PQntuples(Res: PPGresult): Integer;
begin
  Result := Min(PSQLTypes.PQntuples(Res), 25);
end;
{$ENDIF}

{$IFDEF M_DEBUG}
var F: TextFile;
    DebugFileOpened: boolean = False;

procedure LogDebugMessage(const MsgType, Msg: string);
begin
 if DebugFileOpened and (Msg > EmptyStr) then
  WriteLn(F,'<TR><TD>',GetTickCount() - SessionStart,'&nbsp;ms</TD><TD><b>',MsgType,'</b></TD><TD><PRE>',Msg,'</PRE></TD><TR>');
end;

function PQConnectDB(ConnInfo: PAnsiChar): PPGconn;
begin
 Result := PSQLTypes.PQConnectDB(ConnInfo);
 LogDebugMessage('CONN', String(ConnInfo));
end;

function PQExec(Handle: PPGconn; AQuery: PAnsiChar): PPGresult;
begin
 Result := PSQLTypes.PQexec(Handle,AQuery);
 LogDebugMessage('EXEC', String(AQuery));
end;

function lo_creat(Handle: PPGconn; mode: Integer): Oid;
begin
 Result := PSQLTypes.lo_creat(Handle,mode);
 LogDebugMessage('loCr', 'LO OID = '+inttostr(Result));
end;

function lo_open(Handle: PPGconn; lobjId: Oid; mode: Integer): Integer;
begin
 Result := PSQLTypes.lo_open(Handle,lobjId,mode);
 LogDebugMessage('loOp', 'oid = '+inttostr(lobjId)+'; fd = '+inttostr(Result));
end;

function lo_close(Handle: PPGconn; fd: Integer): Integer;
begin
 Result := PSQLTypes.lo_close(Handle,fd);
 LogDebugMessage('loCl', 'fd = '+inttostr(fd));
end;

function PQerrorMessage(Handle: PPGconn): PAnsiChar;
begin
  Result := PSQLTypes.PQerrorMessage(Handle);
  LogDebugMessage('ERR ', string(Result));
end;

procedure OpenDebugFile;
var Name, Time: string;
begin
 SessionStart := GetTickCount();
 DateTimeToString(Time, 'dd.mm.yy_hh.nn.ss', Now());
 Name := '_' + Time;
 Name := ChangeFileExt(GetModuleName(HInstance), Name + '_log.html');
 AssignFile(F, Name);
 {$I-}
 if FileExists(Name) then
  Append(F)
 else
  Rewrite(F);
 {$I+}
 DebugFileOpened := IOResult = 0;
 if not DebugFileOpened then Exit;
 WriteLn(F,'<HR>','<TABLE BORDER="1">');
 LogDebugMessage('INFO',Format('<b>----- Session started at %s -----</b>', [Time]));
end;

procedure CloseDebugFile;
begin
 if not DebugFileOpened then Exit;
 LogDebugMessage('INFO','----- Session closed -----');
 WriteLn(F,'</TABLE>');
 CloseFile(F);
end;
{$ENDIF}

{$IFDEF UNDER_DELPHI_6}
const
// 8087 status word masks
  mIE = $0001;
  mDE = $0002;
  mZE = $0004;
  mOE = $0008;
  mUE = $0010;
  mPE = $0020;
  mC0 = $0100;
  mC1 = $0200;
  mC2 = $0400;
  mC3 = $4000;
const
  // 1E18 as a 64-bit integer
  Const1E18Lo = $0A7640000;
  Const1E18Hi = $00DE0B6B3;
  DCon10: Integer = 10;

procedure GetLocaleFormatSettings(LCID: Integer;
  var FormatSettings: TFormatSettings);
begin
  with FormatSettings do
    DecimalSeparator := '.';
end;

function TextToFloat(Buffer: PChar; var Value;
  ValueType: TFloatValue; const FormatSettings: TFormatSettings): Boolean;

const
// 8087 control word
// Infinity control  = 1 Affine
// Rounding Control  = 0 Round to nearest or even
// Precision Control = 3 64 bits
// All interrupts masked
  CWNear: Word = $133F;

var
  Temp: Integer;
  CtrlWord: Word;
  DecimalSep: Char;
  SaveGOT: Integer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     EBX
        MOV     SaveGOT,EAX
{$ELSE}
        MOV     SaveGOT,0
        MOV     EBX,ECX
{$ENDIF}
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.DecimalSeparator
        MOV     DecimalSep,AL
        FSTCW   CtrlWord
        FCLEX
{$IFDEF PIC}
        FLDCW   [EAX].CWNear
{$ELSE}
        FLDCW   CWNear
{$ENDIF}
        FLDZ
        CALL    @@SkipBlanks
        MOV     BH, byte ptr [ESI]
        CMP     BH,'+'
        JE      @@1
        CMP     BH,'-'
        JNE     @@2
@@1:    INC     ESI
@@2:    MOV     ECX,ESI
        CALL    @@GetDigitStr
        XOR     EDX,EDX
        MOV     AL,[ESI]
        CMP     AL,DecimalSep
        JNE     @@3
        INC     ESI
        CALL    @@GetDigitStr
        NEG     EDX
@@3:    CMP     ECX,ESI
        JE      @@9
        MOV     AL, byte ptr [ESI]
        AND     AL,0DFH
        CMP     AL,'E'
        JNE     @@4
        INC     ESI
        PUSH    EDX
        CALL    @@GetExponent
        POP     EAX
        ADD     EDX,EAX
@@4:    CALL    @@SkipBlanks
        CMP     BYTE PTR [ESI],0
        JNE     @@9
        MOV     EAX,EDX
        CMP     BL,fvCurrency
        JNE     @@5
        ADD     EAX,4
@@5:    PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
        CMP     BH,'-'
        JNE     @@6
        FCHS
@@6:    CMP     BL,fvExtended
        JE      @@7
        FISTP   QWORD PTR [EDI]
        JMP     @@8
@@7:    FSTP    TBYTE PTR [EDI]
@@8:    FSTSW   AX
        TEST    AX,mIE+mOE
        JNE     @@10
        MOV     AL,1
        JMP     @@11
@@9:    FSTP    ST(0)
@@10:   XOR     EAX,EAX
@@11:   FCLEX
        FLDCW   CtrlWord
        FWAIT
        JMP     @@Exit

@@SkipBlanks:

@@21:   LODSB
        OR      AL,AL
        JE      @@22
        CMP     AL,' '
        JE      @@21
@@22:   DEC     ESI
        RET

// Process string of digits
// Out EDX = Digit count

@@GetDigitStr:

        XOR     EAX,EAX
        XOR     EDX,EDX
@@31:   LODSB
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@32
{$IFDEF PIC}
        XCHG    SaveGOT,EBX
        FIMUL   [EBX].DCon10
        XCHG    SaveGOT,EBX
{$ELSE}
        FIMUL   DCon10
{$ENDIF}
        MOV     Temp,EAX
        FIADD   Temp
        INC     EDX
        JMP     @@31
@@32:   DEC     ESI
        RET

// Get exponent
// Out EDX = Exponent (-4999..4999)

@@GetExponent:

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     CL, byte ptr [ESI]
        CMP     CL,'+'
        JE      @@41
        CMP     CL,'-'
        JNE     @@42
@@41:   INC     ESI
@@42:   MOV     AL, byte ptr [ESI]
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@43
        INC     ESI
        IMUL    EDX,10
        ADD     EDX,EAX
        CMP     EDX,500
        JB      @@42
@@43:   CMP     CL,'-'
        JNE     @@44
        NEG     EDX
@@44:   RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;

procedure PutExponent;
// Store exponent
// In   AL  = Exponent character ('E' or 'e')
//      AH  = Positive sign character ('+' or 0)
//      BL  = Zero indicator
//      ECX = Minimum number of digits (0..4)
//      EDX = Exponent
//      EDI = Destination buffer
asm
        PUSH    ESI
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     ESI,EAX
        POP     ECX
        POP     EAX
{$ELSE}
        XOR     ESI,ESI
{$ENDIF}
        STOSB
        OR      BL,BL
        JNE     @@0
        XOR     EDX,EDX
        JMP     @@1
@@0:    OR      EDX,EDX
        JGE     @@1
        MOV     AL,'-'
        NEG     EDX
        JMP     @@2
@@1:    OR      AH,AH
        JE      @@3
        MOV     AL,AH
@@2:    STOSB
@@3:    XCHG    EAX,EDX
        PUSH    EAX
        MOV     EBX,ESP
@@4:    XOR     EDX,EDX
        DIV     [ESI].DCon10
        ADD     DL,'0'
        MOV     [EBX],DL
        INC     EBX
        DEC     ECX
        OR      EAX,EAX
        JNE     @@4
        OR      ECX,ECX
        JG      @@4
@@5:    DEC     EBX
        MOV     AL,[EBX]
        STOSB
        CMP     EBX,ESP
        JNE     @@5
        POP     EAX
        POP     ESI
end;


function FloatToText(BufferArg: PChar; const Value; ValueType: TFloatValue;
  Format: TFloatFormat; Precision, Digits: Integer;
  const FormatSettings: TFormatSettings): Integer;
var
  Buffer: Cardinal;
  FloatRec: TFloatRec;
  SaveGOT: Integer;
  DecimalSep: Char;
  ThousandSep: Char;
  CurrencyStr: Pointer;
  CurrFmt: Byte;
  NegCurrFmt: Byte;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     Buffer,EAX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        MOV     SaveGOT,EAX
        POP     ECX
{$ENDIF}
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.DecimalSeparator
        MOV     DecimalSep,AL
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.ThousandSeparator
        MOV     ThousandSep,AL
        MOV     EAX,FormatSettings
        MOV     EAX,[EAX].TFormatSettings.CurrencyString
        MOV     CurrencyStr,EAX
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.CurrencyFormat
        MOV     CurrFmt,AL
        MOV     EAX,FormatSettings
        MOV     AL,[EAX].TFormatSettings.NegCurrFormat
        MOV     NegCurrFmt,AL
        MOV     SaveGOT,0
        MOV     EAX,19
        CMP     CL,fvExtended
        JNE     @@2
        MOV     EAX,Precision
        CMP     EAX,2
        JGE     @@1
        MOV     EAX,2
@@1:    CMP     EAX,18
        JLE     @@2
        MOV     EAX,18
@@2:    MOV     Precision,EAX
        PUSH    EAX
        MOV     EAX,9999
        CMP     Format,ffFixed
        JB      @@3
        MOV     EAX,Digits
@@3:    PUSH    EAX
        LEA     EAX,FloatRec
        CALL    FloatToDecimal
        MOV     EDI,Buffer
        MOVZX   EAX,FloatRec.Exponent
        SUB     EAX,7FFFH
        CMP     EAX,2
        JAE     @@4
        MOV     ECX, EAX
        CALL    @@PutSign
        LEA     ESI,@@INFNAN[ECX+ECX*2]
        ADD     ESI,SaveGOT
        MOV     ECX,3
        REP     MOVSB
        JMP     @@7
@@4:    LEA     ESI,FloatRec.Digits
        MOVZX   EBX,Format
        CMP     BL,ffExponent
        JE      @@6
        CMP     BL,ffCurrency
        JA      @@5
        MOVSX   EAX,FloatRec.Exponent
        CMP     EAX,Precision
        JLE     @@6
@@5:    MOV     BL,ffGeneral
@@6:    LEA     EBX,@@FormatVector[EBX*4]
        ADD     EBX,SaveGOT
        MOV     EBX,[EBX]
        ADD     EBX,SaveGOT
        CALL    EBX
@@7:    MOV     EAX,EDI
        SUB     EAX,Buffer
        POP     EBX
        POP     ESI
        POP     EDI
        JMP     @@Exit

@@FormatVector:
        DD      @@PutFGeneral
        DD      @@PutFExponent
        DD      @@PutFFixed
        DD      @@PutFNumber
        DD      @@PutFCurrency

@@INFNAN: DB 'INFNAN'

// Get digit or '0' if at end of digit string

@@GetDigit:

        LODSB
        OR      AL,AL
        JNE     @@a1
        MOV     AL,'0'
        DEC     ESI
@@a1:   RET

// Store '-' if number is negative

@@PutSign:

        CMP     FloatRec.Negative,0
        JE      @@b1
        MOV     AL,'-'
        STOSB
@@b1:   RET

// Convert number using ffGeneral format

@@PutFGeneral:

        CALL    @@PutSign
        MOVSX   ECX,FloatRec.Exponent
        XOR     EDX,EDX
        CMP     ECX,Precision
        JG      @@c1
        CMP     ECX,-3
        JL      @@c1
        OR      ECX,ECX
        JG      @@c2
        MOV     AL,'0'
        STOSB
        CMP     BYTE PTR [ESI],0
        JE      @@c6
        MOV     AL,DecimalSep
        STOSB
        NEG     ECX
        MOV     AL,'0'
        REP     STOSB
        JMP     @@c3
@@c1:   MOV     ECX,1
        INC     EDX
@@c2:   LODSB
        OR      AL,AL
        JE      @@c4
        STOSB
        LOOP    @@c2
        LODSB
        OR      AL,AL
        JE      @@c5
        MOV     AH,AL
        MOV     AL,DecimalSep
        STOSW
@@c3:   LODSB
        OR      AL,AL
        JE      @@c5
        STOSB
        JMP     @@c3
@@c4:   MOV     AL,'0'
        REP     STOSB
@@c5:   OR      EDX,EDX
        JE      @@c6
        XOR     EAX,EAX
        JMP     @@PutFloatExpWithDigits
@@c6:   RET

// Convert number using ffExponent format

@@PutFExponent:

        CALL    @@PutSign
        CALL    @@GetDigit
        MOV     AH,DecimalSep
        STOSW
        MOV     ECX,Precision
        DEC     ECX
@@d1:   CALL    @@GetDigit
        STOSB
        LOOP    @@d1
        MOV     AH,'+'

@@PutFloatExpWithDigits:

        MOV     ECX,Digits
        CMP     ECX,4
        JBE     @@PutFloatExp
        XOR     ECX,ECX

// Store exponent
// In   AH  = Positive sign character ('+' or 0)
//      ECX = Minimum number of digits (0..4)

@@PutFloatExp:

        MOV     AL,'E'
        MOV     BL, FloatRec.Digits.Byte
        MOVSX   EDX,FloatRec.Exponent
        DEC     EDX
        CALL    PutExponent
        RET

// Convert number using ffFixed or ffNumber format

@@PutFFixed:
@@PutFNumber:

        CALL    @@PutSign

// Store number in fixed point format

@@PutNumber:

        MOV     EDX,Digits
        CMP     EDX,18
        JB      @@f1
        MOV     EDX,18
@@f1:   MOVSX   ECX,FloatRec.Exponent
        OR      ECX,ECX
        JG      @@f2
        MOV     AL,'0'
        STOSB
        JMP     @@f4
@@f2:   XOR     EBX,EBX
        CMP     Format,ffFixed
        JE      @@f3
        MOV     EAX,ECX
        DEC     EAX
        MOV     BL,3
        DIV     BL
        MOV     BL,AH
        INC     EBX
@@f3:   CALL    @@GetDigit
        STOSB
        DEC     ECX
        JE      @@f4
        DEC     EBX
        JNE     @@f3
        MOV     AL,ThousandSep
        TEST    AL,AL
        JZ      @@f3
        STOSB
        MOV     BL,3
        JMP     @@f3
@@f4:   OR      EDX,EDX
        JE      @@f7
        MOV     AL,DecimalSep
        TEST    AL,AL
        JZ      @@f4b
        STOSB
@@f4b:  JECXZ   @@f6
        MOV     AL,'0'
@@f5:   STOSB
        DEC     EDX
        JE      @@f7
        INC     ECX
        JNE     @@f5
@@f6:   CALL    @@GetDigit
        STOSB
        DEC     EDX
        JNE     @@f6
@@f7:   RET

// Convert number using ffCurrency format

@@PutFCurrency:

        XOR     EBX,EBX
        MOV     BL,CurrFmt.Byte
        MOV     ECX,0003H
        CMP     FloatRec.Negative,0
        JE      @@g1
        MOV     BL,NegCurrFmt.Byte
        MOV     ECX,040FH
@@g1:   CMP     BL,CL
        JBE     @@g2
        MOV     BL,CL
@@g2:   ADD     BL,CH
        LEA     EBX,@@MoneyFormats[EBX+EBX*4]
        ADD     EBX,SaveGOT
        MOV     ECX,5
@@g10:  MOV     AL,[EBX]
        CMP     AL,'@'
        JE      @@g14
        PUSH    ECX
        PUSH    EBX
        CMP     AL,'$'
        JE      @@g11
        CMP     AL,'*'
        JE      @@g12
        STOSB
        JMP     @@g13
@@g11:  CALL    @@PutCurSym
        JMP     @@g13
@@g12:  CALL    @@PutNumber
@@g13:  POP     EBX
        POP     ECX
        INC     EBX
        LOOP    @@g10
@@g14:  RET

// Store currency symbol string

@@PutCurSym:

        PUSH    ESI
        MOV     ESI,CurrencyStr
        TEST    ESI,ESI
        JE      @@h1
        MOV     ECX,[ESI-4]
        REP     MOVSB
@@h1:   POP     ESI
        RET

// Currency formatting templates

@@MoneyFormats:
        DB      '$*@@@'
        DB      '*$@@@'
        DB      '$ *@@'
        DB      '* $@@'
        DB      '($*)@'
        DB      '-$*@@'
        DB      '$-*@@'
        DB      '$*-@@'
        DB      '(*$)@'
        DB      '-*$@@'
        DB      '*-$@@'
        DB      '*$-@@'
        DB      '-* $@'
        DB      '-$ *@'
        DB      '* $-@'
        DB      '$ *-@'
        DB      '$ -*@'
        DB      '*- $@'
        DB      '($ *)'
        DB      '(* $)'

@@Exit:
end;


resourcestring
  SInvalidFloat = '''%s'' is not a valid floating point value';

function StrToFloat(const S: string;
  const FormatSettings: TFormatSettings): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended, FormatSettings) then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [S]);
end;

function FloatToStr(Value: Extended;
  const FormatSettings: TFormatSettings): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, fvExtended,
    ffGeneral, 15, 0, FormatSettings));
end;

function FormatDateTime(const Format: string; DateTime: TDateTime;
  const FormatSettings: TFormatSettings): string;
begin
  DateTimeToString(Result, Format, DateTime, FormatSettings);
end;

procedure DateTimeToString(var Result: string; const Format: string;
  DateTime: TDateTime; const FormatSettings: TFormatSettings);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N: Integer;
  begin
    N := SizeOf(Buffer) - BufPos;
    if N > Count then N := Count;
    if N <> 0 then Move(P[0], Buffer[BufPos], N);
    Inc(BufPos, N);
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, SizeOf(NumBuf), Format,
      SizeOf(Format), [Digits, Number]));
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, TimeDecoded, Use12HourClock,
    BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    Year, Month, Day, Hour, Min, Sec, MSec, H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    procedure GetDate;
    begin
      if not DateDecoded then
      begin
        DecodeDate(DateTime, Year, Month, Day);
        DateDecoded := True;
      end;
    end;

    procedure GetTime;
    begin
      if not TimeDecoded then
      begin
        DecodeTime(DateTime, Hour, Min, Sec, MSec);
        TimeDecoded := True;
      end;
    end;

    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToBytelen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ByteToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToByteIndex(Result, 3) - 1;
                SetString(Result, P, CharToByteLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      with SystemTime do
      begin
        wYear  := Year;
        wMonth := Month;
        wDay   := Day;
      end;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;

    function StrCharLength(const Str: PChar): Integer;
    begin
      if SysLocale.FarEast then
        Result := Integer(CharNext(Str)) - Integer(Str)
      else
        Result := 1;
    end;

    function StrNextChar(const Str: PChar): PChar;
    begin
      Result := CharNext(Str);
    end;

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      TimeDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        if Starter in LeadBytes then
        begin
          AppendChars(Format, StrCharLength(Format));
          Format := StrNextChar(Format);
          LastToken := ' ';
          Continue;
        end;
        Format := StrNextChar(Format);
        Token := Starter;
        if Token in ['a'..'z'] then Dec(Token, 32);
        if Token in ['A'..'Z'] then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              GetDate;
              if Count <= 2 then
                AppendNumber(Year mod 100, 2) else
                AppendNumber(Year, 4);
            end;
          'G':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              GetDate;
              AppendString(ConvertYearString(Count));
            end;
          'M':
            begin
              GetCount;
              GetDate;
              case Count of
                1, 2: AppendNumber(Month, Count);
                3: AppendString(FormatSettings.ShortMonthNames[Month]);
              else
                AppendString(FormatSettings.LongMonthNames[Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  begin
                    GetDate;
                    AppendNumber(Day, Count);
                  end;
                3: AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                4: AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                5: AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              else
                AppendFormat(Pointer(FormatSettings.LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              GetTime;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if P^ in LeadBytes then
                begin
                  P := StrNextChar(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Min, Count);
            end;
          'S':
            begin
              GetCount;
              GetTime;
              if Count > 2 then Count := 2;
              AppendNumber(Sec, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(FormatSettings.ShortTimeFormat)) else
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              GetTime;
              if Count > 3 then Count := 3;
              AppendNumber(MSec, Count);
            end;
          'A':
            begin
              GetTime;
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if Hour < 12 then
                  AppendString(FormatSettings.TimeAMString) else
                  AppendString(FormatSettings.TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.LongDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                GetDate;
                AppendString(FormatSettings.ShortDayNames[DayOfWeek(DateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(FormatSettings.ShortDateFormat));
              GetTime;
              if (Hour <> 0) or (Min <> 0) or (Sec <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(FormatSettings.LongTimeFormat));
              end;
            end;
          '/':
            if DateSeparator <> #0 then
              AppendChars(@FormatSettings.DateSeparator, 1);
          ':':
            if TimeSeparator <> #0 then
              AppendChars(@FormatSettings.TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if Format^ in LeadBytes then
                  Format := StrNextChar(Format)
                else
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then AppendFormat(Pointer(Format)) else AppendFormat('C');
  SetString(Result, Buffer, BufPos);
end;
{$ENDIF}

function _PQExecute(AConnection: TNativeConnect; AQuery: string): PPGResult;
var Q: PAnsiChar;
    S: AnsiString;
begin
  if AConnection.IsUnicodeUsed then
    S := UTF8Encode(AQuery)
  else
    S := AnsiString(AQuery);
  GetMem(Q, Length(S) + 1);
  try
    StrPCopy(Q, S);
    Result := PQExec(AConnection.Handle, Q);
  finally
   FreeMem(Q);
  end;
end;

function _PQExecuteParams(AConnection: TNativeConnect; AQuery: string; AParams: TParams): PPGResult;
var Q: PAnsiChar;
    S: AnsiString;
    paramValues: array of PAnsiChar;
    i: integer;
begin
  if AConnection.IsUnicodeUsed then
    S := UTF8Encode(AQuery)
  else
    S := AnsiString(AQuery);
  GetMem(Q, Length(S) + 1);
  try
    StrPCopy(Q, S);
    SetLength(paramValues, AParams.Count);
    for i := 0 to AParams.Count - 1 do
     begin
      if AConnection.IsUnicodeUsed then
        S := UTF8Encode(AParams[i].AsString)
      else
        S := AnsiString(AParams[i].AsString);
      GetMem(paramValues[i], Length(S) + 1);
      StrPCopy(paramValues[i], S);
     end;
    try
      Result := PQexecParams(AConnection.Handle, Q, AParams.Count, nil, @paramValues[0], nil, nil, 0);
    finally
     for i := 0 to AParams.Count - 1 do
       FreeMem(paramValues[i]);
    end;
  finally
   FreeMem(Q);
  end;
end;

function _PQConnectDBParams(AParams: TStrings; ExpandDbName: boolean = False): PPGConn;
var
  ConnKeywords, ConnValues: array of PAnsiChar;
  K,V: AnsiString;
  i: integer;
begin
  SetLength(ConnKeywords, AParams.Count);
  SetLength(ConnValues, AParams.Count);
  for i := 0 to AParams.Count - 1 do
   begin
     K := UTF8Encode(AParams.Names[i]); //since this is connection assume we'll use UTF8
     GetMem(ConnKeywords[i], Length(K) + 1);
     StrPCopy(ConnKeywords[i], K);
     {$IFDEF DELPHI_7}
     V := UTF8Encode(AParams.ValueFromIndex[i]);
     {$ELSE}
     V := UTF8Encode(Copy(AParams[I], Length(K) + 2, MaxInt));
     {$ENDIF}
     GetMem(ConnValues[i], Length(V) + 1);
     StrPCopy(ConnValues[i], V);
   end;
   try
     {$IFDEF M_DEBUG}
     LogDebugMessage('CONN', AParams.CommaText);
     {$ENDIF}
     Result := PQconnectdbParams(@ConnKeywords[0], @ConnValues[0], ord(ExpandDbName));
   finally
     for i := 0 to AParams.Count - 1 do
      begin
       FreeMem(ConnValues[i]);
       FreeMem(ConnKeywords[i]);
      end;
   end;
end;

function TimeOf(const ADateTime: TDateTime): TDateTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(ADateTime, Hour, Min, Sec, MSec);
  Result := EncodeTime(Hour, Min, Sec, MSec);
end;

function QuoteIdentifier(IdentifierName: string): string;
var
   I: integer;
begin
  Result := IdentifierName;
  if IdentifierName = '' then
    Exit;
  if Result[1] <> '"' then
    Result := '"' + Result;
  if Result[length(Result)] <> '"' then
    Result := Result + '"';
  I := 2;
  while I <= length(Result)-1 do
   begin
   if Result[i]='.' then
    begin
     if Result[i-1] <> '"' then
      begin
       Result := Copy(Result,1,I-1) + '"' + Copy(Result,I,MaxInt);
       inc(i);
      end;
     if Result[i+1] <> '"' then
      begin
       Result := Copy(Result,1,I) + '"' + Copy(Result,I+1,MaxInt);
       inc(i,2);
      end;
    end;
   inc(i);
   end;
end;

function ELSEIF(Exp: Boolean; TrueValue, FalseValue: Integer): Integer;
begin
  if Exp then Result := TrueValue else Result := FalseValue;
end;

function AdjustNativeField(iField :TPSQLField; Src,Dest: Pointer; Var Blank : Boolean): Word;
//var l: integer;
begin
  ZeroMemory(Dest, iField.NativeSize);
  Result := 0;
  if PAnsiChar(Src)^ = #0 then
  begin
    Blank  := True;
    Exit;
  end;

  Inc(PAnsiChar(Src));
  Case iField.NativeType of
    FIELD_TYPE_BOOL:     SmallInt(Dest^) := SmallInt(Src^);
    FIELD_TYPE_INT2:     SmallInt(Dest^) := SmallInt(Src^);
    FIELD_TYPE_INT4:     LongInt(Dest^) := LongInt(Src^);
    FIELD_TYPE_INT8:     Int64(Dest^) := Int64(Src^);
    FIELD_TYPE_DATE:   begin
                          try
                            LongInt(Dest^) := DateTimeToTimeStamp(TDateTime(Src^)).Date;
                          except
                            Result := 1;
                          end;
                       end;
    FIELD_TYPE_TIME:   begin
                          try
                            LongInt(Dest^) := MSecsToTimeStamp(TDateTime(Src^)* MSecsPerDay).Time;
                          except
                            Result := 1;
                          end;
                       end;
    FIELD_TYPE_TIMESTAMP:
                          begin
                            try
                              TDateTime(Dest^):= TimeStampToMSecs(DateTimeToTimeStamp(TDateTime(Src^)));
                            except
                              Result:=1;
                            end;
                         end;
    FIELD_TYPE_FLOAT4,
    FIELD_TYPE_FLOAT8,
    FIELD_TYPE_NUMERIC: Double(Dest^) := Double(Src^);

    FIELD_TYPE_POINT: TPSQLPoint(Dest^) := TPSQLPoint(Src^);
    FIELD_TYPE_CIRCLE: TPSQLCircle(Dest^) := TPSQLCircle(Src^);
    FIELD_TYPE_BOX: TPSQLBox(Dest^) := TPSQLBox(Src^);
    FIELD_TYPE_LSEG: TPSQLLSeg(Dest^) := TPSQLLSeg(Src^);

    FIELD_TYPE_BYTEA,
    FIELD_TYPE_OID,
    FIELD_TYPE_TEXT: Result := 1; //29.09.2008
  else
(*    {$IFDEF DELPHI_12}
    if iField.NativeDataset.FConnect.IsUnicodeUsed then
      CopyMemory(Dest, Src, iField.NativeSize)
    else
    {$ENDIF}
      StrLCopy(PAnsiChar(Dest), PAnsiChar(Src), iField.NativeSize); *)
//   l := StrLen(PChar(Src));
   StrLCopy(PChar(Dest), PChar(Src), iField.FieldLength - 1); //minus null byte
  end;

  Blank := Result <> 0;
end;

function AdjustDelphiField(iField: TPSQLField; Src, Dest: Pointer): Word;
var
     TimeStamp: TTimeStamp;
begin
  ZeroMemory(Dest, iField.NativeSize);
  PAnsiChar(Dest)^:=#1;
  Inc(PAnsiChar(Dest),1);
  Result:=0;

  case iField.NativeType of
      FIELD_TYPE_BOOL:     SmallInt(Dest^) := SmallInt(Src^);
      FIELD_TYPE_INT2:     SmallInt(Dest^) := SmallInt(Src^);
      FIELD_TYPE_INT4:     LongInt(Dest^) := LongInt(Src^);
      FIELD_TYPE_INT8:     Int64(Dest^) := Int64(Src^);
      FIELD_TYPE_DATE:     begin
                             try
                                TimeStamp.Date := LongInt(Src^);
                                TimeStamp.Time := 0;
                                TDateTime(Dest^) := TimeStampToDateTime(TimeStamp);
                             except
                                Result := 1;
                             end;
                           end;
      FIELD_TYPE_TIME:     begin
                             try
                               TimeStamp.Date := DateDelta;
                               TimeStamp.Time := LongInt(Src^);
                               TDateTime(Dest^) := TimeStampToDateTime(TimeStamp);
                             except
                               Result := 1;
                             end;
                           end;

      FIELD_TYPE_TIMESTAMP: begin
                              try
                                TDateTime(Dest^):= TimeStampToDateTime(MSecsToTimeStamp(Double(Src^)));
                              except
                                Result:=1;
                              end;
                           end;
      FIELD_TYPE_FLOAT4,
      FIELD_TYPE_FLOAT8,
      FIELD_TYPE_NUMERIC: Double(Dest^) := Double(Src^);

      FIELD_TYPE_POINT: TPSQLPoint(Dest^) := TPSQLPoint(Src^);
      FIELD_TYPE_CIRCLE: TPSQLCircle(Dest^) := TPSQLCircle(Src^);
      FIELD_TYPE_BOX: TPSQLBox(Dest^) := TPSQLBox(Src^);
      FIELD_TYPE_LSEG: TPSQLLSeg(Dest^) := TPSQLLSeg(Src^);

      FIELD_TYPE_OID,
      FIELD_TYPE_BYTEA,
      FIELD_TYPE_TEXT: Result := 1;
  else
    {$IFDEF DELPHI_12}
    if iField.NativeDataset.FConnect.IsUnicodeUsed then
      CopyMemory(Dest, Src, iField.NativeSize)
    else
    {$ENDIF}
      StrLCopy(PAnsiChar(Dest), PAnsiChar(Src), iField.NativeSize);
  end;

 if Result = 1 then
 begin
    ZeroMemory(Dest, iField.NativeSize);
    Result := 0;
 end;
end;

procedure PSQLException(PSQL : TNativeConnect);
begin
  Raise EPSQLException.Create(PSQL);
end;

procedure PSQLExceptionMsg(PSQL : TNativeConnect; Const ErrorMsg : String );
begin
  Raise EPSQLException.CreateMsg(PSQL, ErrorMsg );
end;

function BDETOPSQLStr(Field : TFieldDef): String;
const
  _IsVarChar: array[boolean] of string = ('CHAR','VARCHAR');
  _IntNames: array[boolean,boolean] of string = (('INT4','SERIAL'),('INT8','BIGSERIAL'));
var
  isAutoInc: Boolean;
  isInt8: boolean;

  ColName: string;
begin
    Result :='';
    ColName := AnsiQuotedStr(Field.Name,'"');
    case Field.DataType of
      ftString,
      ftFixedChar : begin
                      Result := Format('%s %s',[ColName,_IsVarChar[(Field.DataType = ftFixedChar) or (faFixed in Field.Attributes)]]);
                      if Field.Size > 0 then
                        Result := Result + Format('(%s)',[IntToStr(Field.Size)]);
                    end;

      ftDate     : Result := Format('%s DATE',[ColName]);
      ftBlob,
      ftBytes,
      ftVarBytes : Result := Format('%s BYTEA',[ColName]);
      ftMemo     : Result := Format('%s TEXT',[ColName]);
      ftBoolean  : Result := Format('%s BOOL',[ColName]);
      ftSmallint,
      ftWord
      {$IFDEF DELPHI_15}
      ,ftShortInt
      {$ENDIF}
       : Result := Format('%s INT2',[ColName]);

      ftInteger,
      {$IFDEF DELPHI_15}
      ftLongWord,
      {$ENDIF}
      ftLargeint,
      ftAutoInc : begin
                       isAutoInc := ftAutoInc = Field.DataType;
                       isInt8 := Field.DataType = ftLargeint;
                       Result := Format('%s %s',[ColName,_IntNames[isInt8,isAutoInc]]);
                    end;

      ftFloat,
      ftBCD     : Result := Format('%s NUMERIC(%s,%s)',[ColName,IntToStr(Field.Size),IntToStr(Field.Precision)]);

      ftTime    : Result := Format('%s TIME',[ColName]);

      {$IFDEF DELPHI_15}
      ftTimeStamp,
      {$ENDIF}
      ftDateTime: Result := Format('%s DATETIME',[ColName]);
    end;
    if Field.Required then
       Result := Result + ' NOT NULL';
end;

function SQLCreateIdxStr(Index : TPSQLIndex;TableName : String;Flds : TPSQLFields): String;

    function GetFieldList:String;
    var
      I : Integer;
      S : String;
    begin
      S :='';
      for I :=0 to Index.FldsInKey-1 do
      begin
         S := S+ AnsiQuotedStr(Flds.Field[Index.Description.aiKeyFld[I]].FieldName,'"');
         if I < Index.FldsInKey-1 then S := S+',';
      end;
      Result := S;
    end;

var IdxName: string;
    Tbl: PChar;

begin
    result := '';

    if Index.IndexName = '' then
     begin
       Tbl := Pchar(TableName);
       IdxName := AnsiExtractQuotedStr(Tbl,'"');
       if Index.Primary then
        idxName := 'PK_'+IdxName
       else
        if Index.Unique then
         idxName := 'UNI_'+IdxName
        else
         idxName := 'IDX_'+IdxName
     end
    else
     IdxName := Index.IndexName;
    IdxName := AnsiQuotedStr(IdxName,'"');

    if Index.Primary then
      Result := Format('ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY(%s);',[TableName,IdxName,GetFieldList])
    else
      if Index.Unique then
       Result := Format('CREATE UNIQUE INDEX %s ON %s (%s);',[IdxName,TableName,GetFieldList])
      else
       Result := Format('CREATE INDEX %s ON %s (%s);',[IdxName,TableName,GetFieldList]);
end;

{******************************************************************************}
{                            EPSQLError                                        *}
{******************************************************************************}
Constructor EPSQLException.CreateBDE(ECode : Word);
begin
  Inherited Create('');
  FBDEErrorCode := ECode;
  FBDE := True;
end;

Constructor EPSQLException.CreateBDEMsg(ECode : Word; Const EMessage : string);
begin
  FPSQLErrorMsg  := string(EMessage);
  CreateBDE(ECode);
end;

Constructor EPSQLException.Create(PSQL : TNativeConnect);
begin
  FPSQL := PSQL;
  FPSQLErrorCode := 1;
  FPSQLErrorPos             := PSQL.ferrorpos;
  FPSQLErrorContext         := PSQL.ferrorcontext;
  FPSQLErrorseverity        := PSQL.FErrorseverity;
  FPSQLErrorsqlstate        := PSQL.FErrorsqlstate;
  FPSQLErrorprimary         := PSQL.FErrorprimary;
  FPSQLErrordetail          := PSQL.FErrordetail;
  FPSQLErrorhint            := PSQL.FErrorhint;
  FPSQLErrorinternalpos     := PSQL.FErrorinternalpos;
  FPSQLErrorinternalquery   := PSQL.FErrorinternalquery;
  FPSQLErrorsourcefile      := PSQL.FErrorsourcefile;
  FPSQLErrorsourceline      := PSQL.FErrorsourceline;
  FPSQLErrorsourcefunc      := PSQL.FErrorsourcefunc;
  FPSQLErrorMsg  := PSQL.GetErrorText;
  if FPSQLErrorCode > 0 then FBDEERRORCode := DBIERR_INVALIDPARAM;
  Inherited Create('');
end;

Constructor EPSQLException.CreateMsg(PSQL : TNativeConnect; Const ErrorMsg : String );
begin
  Create(PSQL);
  FPSQLErrorMsg := ErrorMsg;
  FBDEERRORCode := 1001;
end;

function EPSQLException.GetNativeErrordetail: String;
begin
      result:= FPSQLErrorDetail;
end;

function EPSQLException.GetNativeErrorprimary: String;
begin
      result:= FPSQLErrorPrimary;
end;

function EPSQLException.GetNativeErrorhint: String;
begin
     result:=FPSQLErrorhint;
end;

function EPSQLException.GetNativeErrorinternalpos: String;
begin
      result:= FPSQLErrorinternalpos;
end;

function EPSQLException.GetNativeErrorinternalquery: String;
begin
     result:=FPSQLErrorinternalquery;
end;

function EPSQLException.GetNativeErrorContext: String;
begin
     result:= FPSQLErrorContext;
end;

function EPSQLException.GetNativeErrorMsg : String;
begin
  Result := FPSQLErrorMsg;
end;
function EPSQLException.GetNativeErrorPos : String;
begin
     result:=FPSQLErrorPos;
end;

function EPSQLException.GetNativeErrorseverity: String;
begin
     result:= FPSQLErrorseverity;
end;

function EPSQLException.GetNativeErrorsourcefile: String;
begin
      result:= FPSQLErrorsourcefile;
end;

function EPSQLException.GetNativeErrorsourcefunc: String;
begin
       result:= FPSQLErrorsourcefunc;
end;

function EPSQLException.GetNativeErrorsourceline: String;
begin
      result:= FPSQLErrorsourceline;
end;

function EPSQLException.GetNativeErrorsqlstate: String;
begin
      result:=FPSQLErrorsqlstate;
end;

{******************************************************************************}
{                            TNativeConnect                                   *}
{******************************************************************************}
Constructor TNativeConnect.Create;
begin
  Inherited Create;
  Tables    := TContainer.Create;
  FLoggin  := False;
  FHandle := nil;
end;

Destructor TNativeConnect.Destroy;
begin
  Tables.Free;
  InternalDisconnect;
  Inherited Destroy;
end;

procedure TNativeConnect.DirectExecute(SQL: string);
var
  LocHandle: PPGconn;
  locResult: PPGresult;
  ErrStr: String;
  OldLoggin : Boolean;
begin
  if SQLLibraryHandle <= HINSTANCE_ERROR then LoadPSQLLibrary();
  OldLoggin := FLoggin;
  if FLoggin then InternalDisconnect;
  with DBOptions do
    LocHandle := PQconnectdb(PAnsiChar(UTF8Encode((FDirectConnectString))));
  if not Assigned(LocHandle) then Exit;
  LocResult := _PQExecute(Self, SQL);
  if Assigned(LocResult) then
  begin
     ErrStr := RawToString(PQerrorMessage(LocHandle));
     PQclear(LocResult);
  end;
  PQfinish(LocHandle);
  if OldLoggin then InternalConnect;
  if ErrStr <> '' then
     raise EPSQLException.CreateMsg(self,ErrStr);
end;

{$IFDEF DELPHI_5}
function ifThen(aCondition: boolean; IfTrue, IfFalse: string): string;
begin
 if aCondition then Result := IfTrue else Result := IfFalse;
end;
{$ENDIF}

class function TNativeConnect.Ping(Params: TStrings): TPingStatus;
var ConnStr: string;
    i: integer;
begin
  ConnStr := '';
  for i := 0 to Params.Count - 1 do
    ConnStr := ConnStr + Params[i] + ' ';
  Result := PQping(PAnsiChar(Utf8Encode(ConnStr)));
end;

procedure TNativeConnect.ProcessDBParams(Params : TStrings);
var i: integer;
begin
  for i := 0 to Params.Count - 1 do
   begin
    FConnectString := FConnectString + Params[i] + ' ';
    if Params.Names[i] = 'dbname' then
      FDirectConnectString := FDirectConnectString + 'dbname=template1 '
    else
      FDirectConnectString := Params[i] + ' ';
   end;
end;

procedure TNativeConnect.InternalConnect(ConnParams: TStrings = nil);
var
   Result: PPGresult;
   Utf8Encoded: PAnsiChar;
begin
 if not FLoggIn then
  try
   if SQLLibraryHandle <= HINSTANCE_ERROR then LoadPSQLLibrary();
   FLastOperationTime := GetTickCount;
   if Assigned(ConnParams) then
     FHandle := _PQConnectDBParams(ConnParams)
   else
    begin
     Utf8Encoded := PAnsiChar(Utf8Encode(FConnectString));
     FHandle := PQconnectdb(Utf8Encoded);
    end;
   FLastOperationTime := GetTickCount - FLastOperationTime;
   if PQstatus(Handle) = CONNECTION_BAD then
     CheckResult();
   Result := PQexec(Handle, 'SET DateStyle TO ''ISO, MDY''');
   PQclear(Result);

   FNativeByteaFormat := nbfEscape;
   if GetserverVersionAsInt >= 090000 then
    begin
     Result := PQexec(Handle, 'SELECT current_setting(''bytea_output'')');
     if Assigned(Result) then
      begin
       if PQntuples(Result) > 0 then
       begin
         Utf8Encoded := PQgetvalue(Result, 0, 0);
         if Utf8Encoded = 'hex' then
           FNativeByteaFormat := nbfHex
         else
           FNativeByteaFormat := nbfEscape;
       end;
       PQclear(Result);
      end;
    end;

   FLoggIn := True;
   MonitorHook.DBConnect(Self, True);
  except
   on e:EPSQLException do
    begin
     MonitorHook.DBConnect(Self, False);
     PQFinish(Handle);
     raise;
    end;
  end;
end;

function TNativeConnect.GetBackendPID: Integer;
begin
  Result := PQbackendPID(Handle);
end;

function TNativeConnect.GetCommitOperation: Boolean;
begin
  Result :=FTransState <> xsActive;
end;

procedure TNativeConnect.InternalDisconnect;
begin
  if FLoggin then
  begin
     FLastOperationTime := GetTickCount;
     PQfinish(Handle);
     FLastOperationTime := GetTickCount - FLastOperationTime;
     Handle := nil;
     FLoggin := False;
     FServerVersion := '';
     MonitorHook.DBDisconnect(Self);
  end;
end;

function TNativeConnect.GetErrorText: String;
begin
  Result := RawToString(PQerrorMessage(Handle));
end;

function TNativeConnect.Success: Boolean;
begin
   Result := GetErrorText = '';
end;

function TNativeConnect.RollBack: boolean;
var
  Res: PPGresult;
begin
   Res := PQexec(Handle, 'ROLLBACK');
   Result := PQresultStatus(Res) = PGRES_COMMAND_OK;
   PQclear(Res);
end;

function TNativeConnect.Commit: boolean;
var
  Res: PPGresult;
begin
   Res := PQexec(Handle, 'COMMIT');
   Result := PQresultStatus(Res) = PGRES_COMMAND_OK;
   PQclear(Res);
end;


procedure TNativeConnect.CheckResult;
begin
   if GetErrorText <> '' then
    raise EPSQLException.CreateMsg(self,GetErrorText);
end;

procedure TNativeConnect.CheckResult(FStatement:PPGresult);
begin
 if GetErrorText <> '' then
    begin
     FErrorSeverity := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_SEVERITY)));
     FErrorSQLState := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_SQLSTATE)));
     FErrorPrimary := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_MESSAGE_PRIMARY)));
     FErrorDetail := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_MESSAGE_DETAIL)));
     FErrorHint := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_MESSAGE_HINT)));
     FErrorInternalPos := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_INTERNAL_POSITION)));
     FErrorInternalQuery := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_INTERNAL_QUERY)));
     FErrorSourceFile := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_SOURCE_FILE)));
     FErrorSourceLine := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_SOURCE_LINE)));
     FErrorSourceFunc := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_SOURCE_FUNCTION)));
     FErrorContext := Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_CONTEXT)));
     FErrorPos :=  Trim(RawToString(PQresultErrorField(FStatement, PG_DIAG_STATEMENT_POSITION)));
     raise EPSQLException.CreateMsg(self,GetErrorText);
    end;
end;

procedure TNativeConnect.TableList(pszWild: string; SystemTables: Boolean; List: TStrings);
var
   CRec : string;
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  List.Clear;

  Sql := 'SELECT c.oid :: regclass FROM pg_class as c, pg_namespace as ns'+
         ' WHERE (c.relkind = ''r'' OR c.relkind = ''v'')'+
         ' AND (ns.oid = c.relnamespace)';

  if not SystemTables then
    Sql := SQL + ' AND (ns.nspname NOT LIKE ''pg_%'')'+
                 ' AND (ns.nspname NOT LIKE ''information_schema'')';

  if pszWild <> '' then
    Sql := Sql + ' AND relname LIKE '''+ pszWild+ '''';
  Sql := Sql + ' ORDER BY 1';
  RES := _PQExecute(Self, Sql);
  if Assigned(RES) then
  begin
     CheckResult;
     for I := 0 to PQntuples(RES)-1 do
     begin
        CREC := RawToString(PQgetvalue(RES,I,0)); //'"'+RawToString(PQgetvalue(RES,I,1))+'"."'+RawToString(PQgetvalue(RES,I,0))+'"';
        List.Add(CREC);
     end;
  end;
  PQclear(RES);
end;

procedure TNativeConnect.UserList(pszWild : string; List : TStrings);
var
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  Sql := 'SELECT usename FROM pg_shadow ';
  if pszWild <> '' then
    Sql := Sql + ' WHERE usename LIKE ''' + pszWild + '''';
  Sql := Sql + ' ORDER BY 1';
  RES := _PQExecute(Self, Sql);
 try
  if Assigned(RES) then
  begin
     CheckResult;
     for I := 0 to PQntuples(RES)-1 do
     begin
        List.Add(RawToString(PQgetvalue(RES,I,0)));
     end;
  end;
 finally
  PQclear(RES);
 end;
end;

procedure TNativeConnect.SchemaList(pszWild : string; SystemSchemas: Boolean; List : TStrings);
var
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  Sql := 'SELECT nspname FROM pg_namespace WHERE True ';
  if pszWild <> '' then
    Sql := Sql + ' AND nspname LIKE ''' + pszWild + '''';
  if not SystemSchemas then
   begin
    Sql := Sql + ' AND nspname NOT IN (''pg_catalog'', ''pg_toast'', '+
                    '''pg_sysviews'', ''information_schema'') ';
    if GetserverVersionAsInt > 080200 then
      Sql := Sql + 'AND nspname NOT LIKE E''pg\\_temp\\_%'' AND nspname NOT LIKE E''pg\\_toast_temp\\_%'''
    else
      Sql := Sql + 'AND nspname NOT LIKE ''pg\\_temp\\_%'' AND nspname NOT LIKE ''pg\\_toast_temp\\_%'''
   end;
  Sql := Sql + ' ORDER BY 1';
  RES := _PQExecute(Self, Sql);
 try
  if Assigned(RES) then
  begin
     CheckResult;
     for I := 0 to PQntuples(RES)-1 do
     begin
        List.Add(RawToString(PQgetvalue(RES,I,0)));
     end;
  end;
 finally
  PQclear(RES);
 end;
end;

procedure TNativeConnect.DatabaseList(pszWild : string; List :TStrings);
var
   CRec : string;
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  List.Clear;
   Sql := 'SELECT datname FROM pg_database';
   if pszWild <> '' then
    Sql := Sql + ' WHERE datname LIKE '''+pszWild+'''';
  Sql := Sql + ' ORDER BY datname';
  RES := _PQExecute(Self, Sql);
  if Assigned(RES) then
  begin
     for I := 0 to PQntuples(RES)-1 do
     begin
        CREC := RawToString(PQgetvalue(RES,I,0));
        List.Add(CREC);
     end;
  end;
  PQclear(RES);
end;

procedure TNativeConnect.OpenTable(pszTableName: string;
                                   pszIndexName: string;
                                   iIndexId: Word;
                                   eOpenMode: DBIOpenMode;
                                   eShareMode: DBIShareMode;
                                   var hCursor: hDBICur;
                                   AnOptions: TPSQLDatasetOptions;
                                   Limit, Offset : integer);
begin
  InternalConnect;
  if FSystem then
  begin
     hCursor := hDBICur(TNativeDataSet.Create(Self, Tables, AnOptions, pszTableName, pszIndexName, iIndexId,1,0,True));
     FSystem := False;
  end else
     hCursor := hDBICur(TNativeDataSet.Create(Self, Tables, AnOptions, pszTableName, pszIndexName, iIndexId,Limit,Offset));
  TNativeDataSet(hCursor).OpenTable;
end;

procedure TNativeConnect.QueryAlloc(var hStmt: hDBIStmt);
begin
    hStmt := hDBIStmt(TNativeDataSet.Create(Self, nil, [], '' , '', 0, 0, 0));
end;

procedure TNativeConnect.QueryPrepare(var hStmt: hDBIStmt;Query : String);
begin
   FLastOperationTime := GetTickCount;
   TNativeDataSet(hStmt).SQLQuery := Query;
   TNativeDataSet(hStmt).isQuery  := True;
   FLastOperationTime := GetTickCount - FLastOperationTime;
   MonitorHook.SQLPrepare(TNativeDataSet(hStmt));
end;

procedure TNativeConnect.BeginTran(eXIL: eXILType; var hXact: hDBIXact);
var
  Result: PPGresult;
  TransParam: AnsiString;
begin
  if FTransState <> xsActive then
  begin
    hXact := hDBIXact(Self);
    FTransState := xsActive;
    FTransLevel := eXIL;
    Result := PQexec(Handle, 'BEGIN');
    PQclear(Result);
    TransParam := 'SET TRANSACTION ISOLATION LEVEL ';
    case FTransLevel of
      xilDIRTYREAD,
      xilREADCOMMITTED : TransParam := TransParam + 'READ COMMITTED';
      xilREPEATABLEREAD: TransParam := TransParam + 'SERIALIZABLE';
    end;
    Result := PQexec(Handle, PAnsiChar(TransParam));
    PQclear(Result);
    MonitorHook.TRStart(Self, PQresultStatus (Result) = PGRES_COMMAND_OK);
  end
end;

procedure TNativeConnect.EndTran(hXact : hDBIXact; eEnd : eXEnd);
begin
  if eEnd = xendABORT then
     MonitorHook.TRRollback(Self, Rollback)
  else
     MonitorHook.TRCommit(Self, Commit);
  CheckResult;
  FTransState := xsInactive;
end;

procedure TNativeConnect.GetTranInfo(hXact : hDBIXact; pxInfo : pXInfo);
begin
  ZeroMemory(pxInfo, Sizeof(pxInfo^));
  if GetTransactionStatus in [trstACTIVE, trstINTRANS, trstINERROR] then
      FTransState := xsActive;
  pxInfo^.eXState := FTransState;
  pxInfo^.eXIL    := FTransLevel;
end;

procedure TNativeConnect.QExecDirect(pszQuery : String; phCur: phDBICur; var AffectedRows : LongInt);
var
  hStmt : hDBIStmt;
begin
  hStmt := nil;
  QueryAlloc(hStmt);
  QueryPrepare(hStmt, pszQuery);
  if hStmt <> nil then
    try
      FLastOperationTime := GetTickCount;
      TNativeDataSet(hStmt).Execute;
    finally
      FLastOperationTime := GetTickCount - FLastOperationTime;
      AffectedRows := TNativeDataSet(hStmt).FAffectedRows;
      TNativeDataSet(hStmt).Free;
    end;
end;

procedure TNativeConnect.OpenFieldList(pszTableName : string;
                                       pszDriverType: string;
                                       bPhyTypes    : Boolean;
                                       var hCur     : hDBICur);
var
  P : TNativeDataSet;
procedure ProcessTable;
var
    Props : CURProps;
    Items : Word;
    Descs : TFLDDescList;
begin
    P.GetCursorProps(Props);
    Items := Props.iFields;
    if Items > 0 then
    begin
      SetLength(Descs, Items);
      try
        P.GetFieldDescs(Descs);
        hCur := hDBICur(TFieldList.Create(Self, Descs, Items));
      finally
        Finalize(Descs);
      end;
    end;
end;

begin
   FSystem := True;
   OpenTable(pszTableName, '', 0, dbiREADONLY, dbiOPENSHARED, hDBICur(P), [], 0, 0);
   ProcessTable;
   P.CloseTable;
   P.Free;
   FSystem := False;
end;

procedure TNativeConnect.OpenIndexList(pszTableName: string; pszDriverType: string; var hCur: hDBICur);
var
  P     : hDBICur;
  Ind   : TIndexList;

  procedure ProcessTable;
  var
    Props : CURProps;
    Items : Word;
    Descs : TIDXDescList;
  begin
    Descs := nil;
    TNativeDataSet(P).GetCursorProps(Props);
    Items := Props.iIndexes;
    try
      if Items > 0 then
      begin
        SetLength(Descs, Items);
        TNativeDataSet(P).GetIndexDescs(Descs);
      end else
        Descs := nil;
      Ind  := TIndexList.Create(Self, Descs, Items);
      hCur := hDBICur(Ind);
    finally
     Finalize(Descs);
    end;
  end;

  procedure OpenAndProcessTable;
  begin
    FSystem := True;
    OpenTable(pszTableName, '', 0, dbiREADONLY, dbiOPENSHARED, P, [], 0, 0);
    try
      ProcessTable;
      TNativeDataSet(P).CloseTable;
    Finally
      TNativeDataSet(P).Free;
    end;
    FSystem := False;
  end;

begin
  hCur := nil;
  try
    OpenAndProcessTable;
  except
    On E:EPSQLException do OpenAndProcessTable;
  end;
end;

function TNativeConnect.GetCharSet: string;
begin
  Result := 'UNDEFINED';
  if not FLoggin then Exit;
  Result := UpperCase(string(PQparameterStatus(Handle, 'client_encoding')));
end;

procedure TNativeConnect.EmptyTable(hCursor : hDBICur; pszTableName : string);
var
  isNotOpen : Boolean;
begin
  isNotOpen := not Assigned(hCursor);
  if isNotOpen then
    OpenTable(pszTableName, '', 0, dbiREADWRITE, dbiOPENEXCL, hCursor, [], 0, 0);
  try
    TNativeDataSet(hCursor).EmptyTable;
  Finally
    if isNotOpen then
      TNativeDataSet(hCursor).Free;
  end;
end;

procedure TNativeConnect.TableExists(pszTableName : string);
var
   List : TStrings;
   I : Integer;
   Found : Boolean;
begin
   Found := False;
   List := TStringList.Create;
   try
     TableList('', False, List);
     for I:=0 to List.Count-1 do
     begin
         Found := SameText(pszTableName, List[I]);
         if Found then break;
     end;
   finally
     List.Free;
   end;
   if not Found then
      Raise EPSQLException.CreateBDEMsg(DBIERR_NOSUCHTABLE, pszTableName);
end;

procedure TNativeConnect.CreateTable(bOverWrite: Boolean; var crTblDsc: CRTblDesc);

{      function CreateSQLForCreateTable:String;
      var
        Fld : String;
        I : Integer;
        VCHK : pVCHKDesc;
        PSQLFlds : TPSQLFields;
        PSQLIdxs : TPSQLIndexes;
      begin
        PSQLFlds := TPSQLFields.Create(nil);
        try
          PSQLIdxs := TPSQLIndexes.Create(nil);
          try
            for I := 1 to crTblDsc.iFldCount do
            begin
               if (crTblDsc.iValChkCount > 0) and (crTblDsc.iValChkCount >= I) then
                begin
                   VCHK := crTblDsc.pvchkDesc;
                   if VCHK.iFldNum <> I then VCHK := nil;
                end else VCHK := nil;
                TPSQLField.CreateField(PSQLFlds, crTblDsc.pfldDesc^, VCHK^, i, 0, 0,False);
                Inc(crTblDsc.pfldDesc);
                if crTblDsc.iValChkCount > 0 then
                   if crTblDsc.iValChkCount > I then
                      Inc(CrTblDsc.pvchkDesc);
            end;
            for I := 1 to crTblDsc.iIdxCount do
            begin
                TPSQLIndex.CreateIndex(PSQLIdxs,crTblDsc.pidxDesc);
                Inc(crTblDsc.pidxDesc);
            end;
            Result := Format('CREATE TABLE %s ( ',[crTblDsc.szTblName]);
            for I := 1 to PSQLFlds.Count do
            begin
               Fld := BDETOPSQLStr(PSQLFlds[I]);
                Result := Result + Fld;
                if I < PSQLFlds.Count then Result := Result+', ';
            end;
            Result := Result+');';
            //indexes
            for I := 1 to PSQLIdxs.Count do
            begin
               Fld := SQLCreateIdxStr(PSQLIdxs[I],string(crTblDsc.szTblName),PSQLFlds);
               Result := Result + #13#10 + Fld;
            end;
          finally
            PSQLIdxs.Free;
          end;
        finally
         PSQLFlds.Free;
        end;
      end;

var Res: PPGresult;    }

begin
(*
      Res := PQExec(FHandle,PAnsiChar(ansistring(CreateSQLForCreateTable)));
      try
        CheckResult;
      finally
        PQclear(RES);
      end;
*)
end;

procedure TNativeConnect.AddIndex(hCursor: hDBICur; pszTableName: string; pszDriverType: string; var IdxDesc: IDXDesc; pszKeyviolName: string);
var
  NDS : TNativeDataSet;
begin
  if Assigned(hCursor) then
    NDS := TNativeDataSet(hCursor) else
    OpenTable(pszTableName, '', IdxDesc.iIndexId, dbiREADWRITE, dbiOPENEXCL, hDBICur(NDS), [], 0, 0);
  try
    NDS.AddIndex(idxDesc,pszKeyViolName);
  Finally
    if not Assigned(hCursor) then NDS.Free;
  end;
end;

procedure TNativeConnect.DeleteIndex(hCursor: hDBICur; pszTableName: string; pszDriverType: string; pszIndexName: string; pszIndexTagName: string; iIndexId: Word);
var
  NDS : TNativeDataSet;
begin
  if Assigned(hCursor) then
    NDS := TNativeDataSet(hCursor) else
    OpenTable(pszTableName, pszIndexName, iIndexId, dbiREADWRITE, dbiOPENEXCL, hDBICur(NDS), [], 0, 0);
  try
    NDS.DeleteIndex(pszIndexName, pszIndexTagName, iIndexID);
  Finally
    if not Assigned(hCursor) then NDS.Free;
  end;
end;


//////////////////////////////////////////////////////////
//Constructor : TPSQLField.CreateField
//Description : constructor CreateNewField
//////////////////////////////////////////////////////////
//Input       : Owner: TCollection
//              P: pFldDesc
//              FNum: Word
//              LType: Word
//              LSize: Word
//////////////////////////////////////////////////////////
Constructor TPSQLField.CreateField(Owner : TCollection; P : FldDesc; P1 : VCHKDesc; FNum, LType, LSize : Word; isArray : Boolean);
begin
  Create(Owner);

  FDesc := P;
  FValCheck := P1;

  FieldNumber := FNum;
  NativeType   := LType;
  Case NativeType of
   FIELD_TYPE_BYTEA: NativeBLOBType := nbtBytea;
   FIELD_TYPE_OID: NativeBLOBType := nbtOID
  else
   NativeBLOBType := nbtNotBlob;
  end;
  NativeSize   := LSize; //Max(LSize, FDesc.iLen);

  FieldArray := isArray;
end;

function TPSQLField.GetFieldName : String;
begin
  Result := string(FDesc.szName);
end;

procedure TPSQLField.SetFieldName(Const Value : String);
begin
  StrPCopy(@FDesc.szName, Copy(Value,1,SizeOf(FDesc.szName)-1));
end;

procedure TPSQLField.SetBuffer(PRecord : Pointer);
begin
  FBuffer := PRecord;
  if FBuffer <> nil then
  begin
    FData := FBuffer;
    Inc(PAnsiChar(FData), FDesc.iOffset);
    if FDesc.INullOffset > 0 then
    begin
      FStatus := FBuffer;
      Inc(PAnsiChar(FStatus), FDesc.iNullOffset);
    end else
      FStatus := NIL;
  end else
  begin
    FData := nil;
    FStatus := nil;
  end;
end;

function TPSQLField.GetNativeDataset: TNativeDataSet;
begin
  Result := TPSQLFields(Collection).NativeDataset;
end;

function TPSQLField.GetNativeConnect: TNativeConnect;
begin
  Result := TPSQLFields(Collection).NativeConnect;
end;

function TPSQLField.GetNull : Boolean;
var AVal: PAnsiChar;
begin
  Result := True;
  case NativeType of
   FIELD_TYPE_BPCHAR,
   FIELD_TYPE_VARCHAR: if (dsoEmptyCharAsNull in GetNativeDataset.Options) then
       begin
         AVal := FieldValue;
         Inc(AVal);
         if AVal = '' then Exit; //we have empty string and it's treated as NULL due options
       end;
  end;
  if FStatus <> nil then
    Result := TFieldStatus(FStatus^).isNULL = -1
  else
    Result := FALSE;
end;

procedure TPSQLField.SetNull( Flag : Boolean );
Const
  VALUES : Array[ Boolean ] of SmallInt = ( 0, -1 );
begin
  if FStatus <> nil then  FStatus^.isNULL := VALUES[ Flag ];
end;

function TPSQLField.GetChanged : Boolean;
begin
  if FStatus <> nil then  Result := TFieldStatus(FStatus^).Changed else Result := TRUE;
end;

procedure TPSQLField.SetChanged(Flag : Boolean);
begin
  if FStatus <> nil then TFieldStatus(FStatus^).Changed := Flag;
end;

function TPSQLField.GetLocalSize : Word;
begin
  Result := FDesc.iUnused[1];
end;

procedure TPSQLField.SetLocalSize(S : Word);
begin
  FDesc.iUnused[1] := S;
end;

function TPSQLField.GetLocalType : Word;
begin
  Result := FDesc.iUnused[0];
end;

procedure TPSQLField.SetLocalType(S : Word);
begin
  FDesc.iUnused[0] := S;
end;

function TPSQLField.FieldValue: PAnsiChar;
begin
   Result := PAnsiChar(PAnsiChar(FData) + FieldNumber - 1);
end;

function TPSQLField.FieldValueAsStr: string;
 var Src: pointer;

 function SimpleQuote(const S: string): string;
 begin
  Result := '''' + S + '''';
 end;

begin
  Src := FieldValue();
  Inc(PAnsiChar(Src));
  case FieldType of
     fldBOOL:    Result := IfThen(SmallInt(Src^) > 0, 'TRUE', 'FALSE');
     fldINT16:   Result := IntToStr(SmallInt(Src^));
     fldINT32:   Result := IntToStr(LongInt(Src^));
     fldINT64:   Result := IntToStr(Int64(Src^));
     fldFloat:   Result := SQLFloatToStr(Double(Src^));
     fldZSTRING:
                 case NativeType of
                   FIELD_TYPE_BIT,
                   FIELD_TYPE_VARBIT: Result := 'B' + NativeDataset.StrValue(Src)
                 else
                   Result := NativeDataset.StrValue(Src);
                 end;
     fldUUID:    Result := NativeDataset.UuidValue(Src);
     fldBLOB:    if FieldSubType = fldstMemo then
                   Result := NativeDataset.MemoValue(Src)
                 else
                   Result := NativeDataset.BlobValue(Src, Self);
     fldDate:    Result := SimpleQuote(DateTimeToSqlDate(TDateTime(Src^), DATE_MODE));
     fldTime:    Result := SimpleQuote(DateTimeToSqlDate(TDateTime(Src^), TIME_MODE));
     fldTIMESTAMP: Result := SimpleQuote(DateTimeToSqlDate(TDateTime(Src^), TIMESTAMP_MODE));
     fldPOINT:   Result := SimpleQuote(PointToSQLPoint(TPSQLPoint(Src^)));
     fldCIRCLE:  Result := SimpleQuote(CircleToSQLCircle(TPSQLCircle(Src^)));
     fldBOX:     Result := SimpleQuote(BoxToSQLBox(TPSQLBox(Src^)));
     fldLSEG:    Result := SimpleQuote(LSegToSQLLSeg(TPSQLLSeg(Src^)));
  end;
end;

function TPSQLField.GetFieldDefault : string;//mi
begin
  Result := string(FValCheck.aDefVal);
end;

procedure TPSQLField.SetFieldDefault(aStr : string);//mi
begin
  FValCheck.aDefVal := aStr;
end;

//////////////////////////////////////////////////////////
//   TPSQLFields
//////////////////////////////////////////////////////////

function TPSQLFields.AddField(P: FldDesc; P1: VCHKDesc; FNum, LType,
  LSize: Word; isArray: Boolean): TPSQLField;
begin
 Result := Add as TPSQLField;
 Result.Description := P;
 Result.ValCheck := P1;
 Result.FieldNumber := FNum;
 Result.NativeType   := LType;
 case Result.NativeType of
   FIELD_TYPE_BYTEA: Result.NativeBLOBType := nbtBytea;
   FIELD_TYPE_OID: Result.NativeBLOBType := nbtOID
  else
   Result.NativeBLOBType := nbtNotBlob;
 end;
 Result.NativeSize   := LSize; //Max(LSize, Result.FieldLength);
 Result.FieldArray := isArray;
end;

Constructor TPSQLFields.Create(Table : TNativeDataSet);
begin
  Inherited Create(TPSQLField);
  FTable := Table;
end;


function TPSQLFields.GetField(Index : Integer) : TPSQLField;
var
  LocType : Word;
  LocSize : Word;
  LocArray : Boolean;
  Desc    : FldDesc;
  ValCheck : VCHKDesc;
begin
  if ( Count >= Index ) and ( Index > 0 ) then
    Result := TPSQLField(Items[Index-1]) else
  begin
    if not ((Index > 0) and (FTable <> nil)) then raise EPSQLException.CreateBDE(DBIERR_INVALIDRECSTRUCT);
    FTable.GetNativeDesc(Index, Desc, ValCheck, LocType, LocSize, LocArray);
    Result := TPSQLField.CreateField(Self, Desc, ValCheck, Index, LocType, LocSize,LocArray);
  end;
end;

function TPSQLFields.GetNativeConnect: TNativeConnect;
begin
  Result := FTable.Connect;
end;


procedure TPSQLFields.SetFields(PRecord : Pointer);
var
  i : Word;
begin
  For i := 1 to Count do
  begin
    With Field[i] do
    begin
      Buffer     := PRecord;
      FieldChanged := FALSE;
      FieldNull       := TRUE;
    end;
  end;
end;

function TPSQLFields.FieldNumberFromName(SearchName : PChar) : Integer;
var
  I   : Integer;
begin
  Result := 0;
  For i := 1 to Count do
  begin
    With GetField( i ) do
    begin
      if (StrIComp(SearchName, PChar(FieldName)) = 0) then
      begin
        Result := Integer(FieldNumber);
        Exit;
      end;
    end;
  end;
end;

//////////////////////////////////////////////////////////
//Constructor : TPSQLIndex.CreateIndex
//Description : constructor CreateIndex
//////////////////////////////////////////////////////////
//Input       : Owner: TCollection
//              P: pIDXDesc
//////////////////////////////////////////////////////////
Constructor TPSQLIndex.CreateIndex(Owner : TCollection; P : pIDXDesc);
begin
  Create(Owner);
  FDesc := P^;
end;

Constructor TPSQLIndexes.Create(Table : TNativeDataSet);
begin
  Inherited Create(TPSQLIndex);
  FTable := Table;
end;

function TPSQLIndexes.GetIndex(Index : Integer) : TPSQLIndex;
begin
  Result := nil;
  if ( Count >= Index ) and ( Index > 0 ) then Result := TPSQLIndex(Items[Index-1]);
end;

function TPSQLIndexes.FindByName(Name :String): TPSQLIndex;
var
  I : Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
     if (CompareText(TPSQLIndex(Items[I]).IndexName, Name) = 0) then
     begin
        Result := TPSQLIndex(Items[I]);
        Exit;
     end;
  end;
end;

function TPSQLIndexes.SetIndex(Name,Fields : String;aPrimary,aUnique,aDesc : Boolean): integer;
var
  Item : TPSQLIndex;
  I,K,J : Integer;
  FldLen : Word;
  FieldList : TStrings;

  //this is used, because Postgres didn't delete dropped columns,
  //but only mark them as deleted. See pg_attribute for details
  function GetLogicalIndexByPhysical(const PhNum: integer):integer;
  var I: integer;
  begin
   Result := 0;
   For I:=0 to FTable.FieldCount-1 do
    if FTable.FieldPosInTable(I) = PhNum then
     begin
      Result := I+1;
      Break;
     end;
  end;

begin
   Result := -1;
   Item := FindByName(Name);
   if not Assigned(Item) then
   begin
      Item := TPSQLIndex(Add);
      Item.IndexNumber := Item.Index+1;
      Item.IndexName := Name;
   end;

  Item.Primary := aPrimary;
  Item.Unique := aUnique;
  Item.Descending := aDesc;
  Item.FDesc.bMaintained := True;
  FieldList := TStringList.Create;
  try
    FieldList.CommaText := Fields;
    for J := 0 to FieldList.Count-1 do
    begin
       I := StrToIntDef(FieldList[J],0);
       if FTable.FConnect.GetserverVersionAsInt >= 070400 then
         I := GetLogicalIndexByPhysical(I)
       else
         I := I; //dropped columns will make problems here
                 //however, handling this in < 7.4.0 is too complex
                 
       if I = 0 then //we have index built on expressions.
         begin   
          Item.Free;
          Exit;
         end;
       FldLen := FTable.FFieldDescs.GetField(I).NativeSize; //utf indices built on varchar need 2 bytes per character
       Item.FldsInKey := Item.FldsInKey+1;
       Item.BlockSize := Item.BlockSize+FldLen;
       Item.KeyLen := Item.BlockSize+Item.FldsInKey;
       K :=Item.FldsInKey;
       Item.FDesc.aiKeyFld[K-1] := I;
       Result := Item.IndexNumber;
    end;
  finally
    FieldList.Free;
  end;
end;

function TPSQLIndexes.FieldNumberFromName( SearchName : PChar ) : Integer;
var
  I   : Integer;
begin
  Result := 0;
  if FTable.FFieldDescs.Count = 0 then FTable.InitFieldDescs;
  For i := 1 to FTable.FFieldDescs.Count do
  begin
    With FTable.FFieldDescs.GetField(i) do
    begin
      if (StrIComp(SearchName, PChar(FieldName))= 0) then
      begin
        Result := Integer(FieldNumber);
        Exit;
      end;
    end;
  end;
end;

//////////////////////////////////////////////////////////
//Constructor : TPSQLNative.CreateNative
//Description : constructor CreateNative
//////////////////////////////////////////////////////////
//Input       : Owner: TCollection
//////////////////////////////////////////////////////////
Constructor TPSQLNative.CreateNative(Owner : TCollection; P : PPGFIELD_INFO);
begin
  Create(Owner);
  FDesc := P^;
end;


Constructor TPSQLNatives.Create(Table : TNativeDataSet);
begin
  Inherited Create(TPSQLNative);
  FTable := Table;
end;

function TPSQLNatives.GetNative(Index : Integer) : TPSQLNative;
begin
  Result := nil;
  if ( Count >= Index ) and ( Index > 0 ) then Result := TPSQLNative(Items[Index-1]);
end;

procedure TPSQLNatives.SetNative(aIndex : Integer; aName : String; aType, aSize, aMaxSize : Integer; aDefault : String);
var
  Item : TPSQLNative;
begin
  Item := TPSQLNative(Add);
  Item.NativeNumber := aIndex;
  Item.NativeName := aName;
  Item.NativeType := aType;
  Item.NativeSize := aSize;
  Item.NativeMaxSize := aMaxSize;
  Item.NativeDefault := aDefault;
end;


//////////////////////////////////////////////////////////
//Description : TPSQLFilter impementation
//////////////////////////////////////////////////////////
Constructor TPSQLFilter.Create(Owner : TNativeDataSet; AClientData : Longint; Exp : pCANExpr; pfFilt : pfGENFilter);
begin
  Inherited Create;
  FDataset := Owner;
  FClientData  := AClientData;
  if Assigned(Exp) then
  begin
    FExprSize := CANExpr(Exp^).iTotalSize;
    if FExprSize > 0 then
    begin
      GetMem(FExpression, FExprSize);
      if Assigned(FExpression) then Move(Exp^, FExpression^, FExprSize);
    end;
  end;
  FPfFilter:= pfFilt;
  FActive:= FALSE;
end;

Destructor TPSQLFilter.Destroy;
begin
  if (FExprSize > 0) and Assigned(FExpression) then FreeMem(FExpression, FExprSize);
  Inherited Destroy;
end;

function TPSQLFilter.GetFilterResult(PRecord : Pointer) : Variant;
var
   I : Integer;
begin
  if FActive then
  begin
     FRecBuff := PRecord;
     if Assigned(FpfFilter) then
     begin
        i := 0;
        try
          i := FpfFilter(FClientData, FRecBuff, Longint(0));
        finally
          result := i <> 0;
        end;
     end else
     begin
        if Assigned(FExpression) then
        begin
           try
             Result := CalcExpression(GetNodeByOffset(NodeStart));
             if Result = Null then Result := False;
           except
             Result := FALSE;
           end;
        end;
     end;
  end else Result := False;
end;

function TPSQLFilter.GetNodeStart : Integer;
begin
  Result := FExpression.iNodeStart;
end;

function TPSQLFilter.GetLiteralPtr(AOffset: Word): Pointer;
var
  i : word;
begin
  i := CANExpr(FExpression^).iLiteralStart + AOffset;
  Result := @MemPtr(FExpression)^[i];
end;
    
function TPSQLFilter.GetNodeByOffset(AOffSet : Integer) : PCanNode;
begin
    Result := pCanNode(Integer(FExpression)+AOffset);
end;

function TPSQLFilter.CalcExpression(ANode : PCanNode) : Variant;
Var
  FldType : TFldType;
begin
  Case pCanHdr(ANode).nodeClass Of
    PSQLTypes.nodeUNARY    : Result := UnaryNode(pCANUnary(ANode));
    PSQLTypes.nodeBINARY   : Result := BinaryNode(pCANBinary(ANode));
    PSQLTypes.nodeCOMPARE  : Result := CompareNode(pCANCompare(ANode));
    PSQLTypes.nodeFIELD    : Result := FieldNode(pCANField(ANode));
    PSQLTypes.nodeCONST    : Result := PerformCanConst(PCANConst(ANode),
        GetLiteralPtr(PCANConst(ANode).iOffset), FldType);
    PSQLTypes.nodeLISTELEM : Result := ListOfValues(pCANListElem(ANode));
  else
    result := Null;
  End;
end;

function TPSQLFilter.ListOfValues(ANode : pCANListElem) : Variant;
Var
  I          : Integer;
  CurNode    : pCANListElem;
begin
  CurNode := ANode;
  I := 0;
  While True Do
  begin
    Inc(I);
    if CurNode^.iNextOffset = 0 Then break;
    CurNode := pCanListElem(GetNodeByOffset(NodeStart + CurNode^.iNextOffset));
  end;
  Result := varArrayCreate([1, I], varVariant);
  I := 1;
  While True Do
  begin
    Result[ I ] := CalcExpression(PCanNode(GetNodeByOffset(NodeStart + ANode^.iOffset)));
    if ANode^.iNextOffset = 0 Then break;
    ANode := pCanListElem(GetNodeByOffset(NodeStart + ANode^.iNextOffset));
    Inc(I);
  end;
end;

function TPSQLFilter.PerformLikeCompare(Const Value, Mask : String; CaseSen : Boolean) : Boolean;
begin
   Result := PSQLExtMask.MatchesMask(Value, Mask, CaseSen);
end;

function TPSQLFilter.PerformInCompare(AOp1, AOp2 : Variant) : Boolean;
Var
  Save   : Variant;
  I, Top : Integer;
begin
  if varType(AOp1) = varArray then
  begin
    Save := AOp2;
    AOp2 := AOp1;
    AOp1 := Save;
  end;
  Result := True;
  Top := VarArrayHighBound(AOp2, 1);
  For I := VarArrayLowBound(AOp2, 1) to Top do
    if AOp1 = AOp2[I] then Exit;
  Result := False;
end;

function TPSQLFilter.UnaryNode( ANode : PCANUnary ) : Variant;
begin
  With ANode^ Do Result := PerformCANOp(canOp, GetNodeValue(iOperand1), UnAssigned);
end;

function TPSQLFilter.BinaryNode(ANode : PCANBinary) : Variant;
begin
  With ANode^ Do  Result := PerformCANOp(canOp, GetNodeValue(iOperand1), GetNodeValue(iOperand2));
end;

function TPSQLFilter.CompareNode(ANode : PCANCompare) : Variant;
Var
  Op1, Op2 : Variant;
begin
   Op1 := GetNodeValue(Anode^.iOperand1);
   Op2 := GetNodeValue(Anode^.iOperand2);
   if varIsNull(Op1) Or varIsEmpty(Op1) Then Op1 := '';
   if varIsNull(Op2) Or varIsEmpty(Op2) Then Op2 := '';
   if ANode.canOp = canLike then
      Result := PerformLikeCompare(Op1,Op2, ANode^.bCaseInsensitive) else
   begin
      Result := Search(Op1,Op2, OEMConv, Anode^.bCaseInsensitive, Anode^.iPartialLen);
      if Anode^.canOp = canNE Then  Result := Not Result;
   end;

end;

function TPSQLFilter.FieldNode(ANode : pCANField) : Variant;
Var
  Field     : TPSQLField;
  blank     : Boolean;
  Dest      :  array[0..MAX_CHAR_LEN] of Char;
  TimeStamp : TTimeStamp;
  DateD     : Double;
begin
  Result := Null;
  Field := FDataset.Fields[ANode.iFieldNum];
  FDataSet.NativeToDelphi(Field, FrecBuff, @Dest, blank);
  if blank then Exit;
  case Field.FieldType of
    fldINT16: Result := PSmallInt(@Dest)^;
    fldUINT16:Result := PWord(@Dest)^;
    fldINT32: Result := PLongInt(@Dest)^;
    fldUINT32:Result := PLongInt(@Dest)^;
    {$IFDEF DELPHI_6}
    fldINT64: Result := PInt64(@Dest)^;
    {$ENDIF}
    fldFLOAT: Result := PDouble(@Dest)^;
    fldZSTRING:
                {$IFDEF DELPHI_12}
                if FDataset.FConnect.IsUnicodeUsed then
                  Result := string(PChar(@Dest))
                else
                {$ENDIF}
                  Result := string(PAnsiChar(@Dest));
    fldUUID:  Result := string(PAnsiChar(@Dest));
    fldBOOL : Result := PWordBool(@Dest)^;
    fldDATE : begin
                 LongWord(TimeStamp.Date) := PLongWord(@Dest)^;
                 TimeStamp.Time := 0;
                 Result := SysUtils.Time+Trunc(TimeStampToDateTime(TimeStamp) + 1E-11);
              end;
    fldTIME : begin
                 LongWord(TimeStamp.Time) := PLongWord(@Dest)^;
                 TimeStamp.Date := 0;
                 Result := SysUtils.Date+TimeOf(TimeStampToDateTime(TimeStamp));
              end;
    fldTIMESTAMP : begin
                     DateD := PDouble(@Dest)^;
                     Result := TimeStampToDateTime(MSecsToTimeStamp(DateD));
                    end;
  else Result := NULL;
  end;
end;

function TPSQLFilter.GetNodeValue(AOffSet : Integer) : Variant;
begin
  Result := CalcExpression(GetNodeByOffset(NodeStart + AOffset));
end;

function TPSQLFilter.PerformCANOp(AOperator : CANOp; AOp1, AOp2 : Variant) : Variant;
begin
  Case AOperator of
    canNOTDEFINED : Result := Null;
    canISBLANK    : Result := VarIsNull(AOp1);
    canNOTBLANK   : Result := not VarIsNull(AOp1);
    canNOT        : Result := not AOp1;
    canEQ         : Result := AOp1 = AOp2;
    canNE         : Result := AOp1 <> AOp2;
    canGT         : Result := AOp1 > AOp2;
    canLT         : Result := AOp1 < AOp2;
    canGE         : Result := AOp1 >= AOp2;
    canLE         : Result := AOp1 <= AOp2;
    canAND        : Result := AOp1 and AOp2;
    canOR         : Result := AOp1 or AOp2;
    canMinus      : Result := -AOp1;
    canADD        : Result := AOp1+AOp2;
    canSUB        : Result := AOp1-AOp2;
    canMUL        : Result := AOp1*AOp2;
    canDIV        : Result := AOp1 /  AOp2;
    canMOD        : Result := AOp1 mod AOp2;
    canREM        : Result := AOp1 mod AOp2;
    canSUM        : Result := Null;
    canCONT       : Result := Null;
    canLike       : Result := PerformLikeCompare(AOp1,AOp2,True);
    canIN         : Result := PerformInCompare(AOp1,AOp2);
    canUPPER      : Result := AnsiUpperCase(AOp1);
    canLOWER      : Result := AnsiLowerCase(AOp1);
    canASSIGN     : Result := VarIsNull(AOp1);
    Else Result := Null;
  end;
end;

function TPSQLFilter.PerformCanConst(ANode:PCANConst; ValuesStart : Pointer; Var FldType : TFldType) : Variant;

function _PerformCanConst( ANode : PCANConst; ValuePtr : Pointer; Var FldType : TFldType) : Variant;
Var
  Offs      : Integer;
  TimeStamp : TTimeStamp;
  DateData  : Double;
  S: String;
{$IFDEF DELPHI_12}
  Len: word;
  buffer: PChar;
{$ENDIF}
begin
  With ANode^ Do
  begin
    Offs := Integer(ValuePtr);
    FldType := FT_UNK;
    Result := Null;
    Case iType Of
      fldZSTRING   : begin
                       S:= string(PAnsiChar(Offs));
                       Result := S;
                       FldType := FT_STRING;
                     end;
{$IFDEF DELPHI_12}
      fldUNICODE   : begin
                       buffer := ValuePtr;
                       Len := Word(buffer[0]);
                       Inc(buffer);
                       SetLength(S, Len div 2);
                       if Len > 0 then
                         S := Copy(buffer, 1, Len div 2);
                       Result := S;
                       FldType := FT_STRING;
                     end;
{$ENDIF}
      fldDATE      : begin
                       LongWord( TimeStamp.Date ) := PLongWord( Offs )^;
                       TimeStamp.Time := 0;
                       Result := SysUtils.Time+ Trunc(TimeStampToDateTime(TimeStamp) + 1E-11);
                       FldType := FT_DATE;
                     end;
      fldBOOL      : begin
                       Result := PWordBool( Offs )^;
                       FldType := FT_BOOL;
                     end;

      fldINT16     : begin
                       Result := PSmallInt( Offs )^;
                       FldType := FT_INT;
                     end;
      fldINT32     : begin
                       Result := PInteger( Offs )^;
                       FldType := FT_INT;
                     end;
      {$IFDEF DELPHI_6}
      fldINT64     : begin
                       Result := Pint64( Offs )^;
                       FldType := FT_INT;
                     end;
      {$ENDIF}
      fldFLOAT     : begin
                       Result := PDouble( Offs )^;
                       FldType := FT_FLOAT;
                     end;
      fldTIME      : begin
                       LongWord( TimeStamp.Time ) := PLongWord( Offs )^;
                       TimeStamp.Date := 0;
                       Result := SysUtils.Date+TimeOf(TimeStampToDateTime( TimeStamp ));
                       FldType := FT_TIME;
                     end;

      fldTIMESTAMP : begin
                       DateData := PDouble( Offs )^;
                       Result := TimeStampToDateTime( MSecsToTimeStamp( DateData ) );
                       FldType := FT_DATETIME;
                     end;
      fldUINT16    : begin
                       Result := PWord( Offs )^;
                       FldType := FT_INT;
                     end;
      fldUINT32    : begin
                       Result := PInteger( Offs )^;
                       FldType := FT_INT;
                     end;
    end;
  end;
end;
begin
  Result:=_PerformCanConst(ANode,ValuesStart,FldType);
end;

function TPSQLFilter.TimeOf(const ADateTime: TDateTime): TDateTime;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(ADateTime, Hour, Min, Sec, MSec);
  Result := EncodeTime(Hour, Min, Sec, MSec);
end;

{$O-}
Constructor TNativeDataSet.Create(PSQL : TNativeConnect;
                                  Container : TContainer;
                                  AnOptions: TPSQLDatasetOptions;
                                  AName, IndexName : string;
                                  Index : Word;
                                  Limit, Offset : Integer;
                                  ASystem : Boolean = False);
begin
  Inherited Create;
  FStatement := nil;
  FFilters    := TContainer.Create;
  if IndexName <> '' then FIndexName := IndexName;
  FFieldDescs := TPSQLFields.Create(Self);
  FIndexDescs := TPSQLIndexes.Create(Self);
  FNativeDescs := TPSQLNatives.Create(Self);
  FKeyNumber               := 0;
  FPrimaryKeyNumber        := 0;
  AutoReExec     := True;
  FConnect := PSQL;
  FOptions := AnOptions;
  FOpen := False;
  FRecSize:=-1;
  FLimit := Limit;
  FOffset := Offset;
  StandartClause := TStringList.Create;
  OrderClause := TStringList.Create;
  RangeClause := TStringList.Create;
  LimitClause := TStringList.Create;
  TableName  := string(AName);
  MasterCursor      := nil;
  FOIDTable := TList.Create;
  FSystemNeed := ASystem;
  IsQuery := False;
  FPreventRememberBuffer := False; //mi:2008-08-27
end;

Destructor TNativeDataSet.Destroy;
begin
  if FOIDTable <> nil then
     FOIDTable.Free;
  FOIDTable := nil;
  MasterCursor      := nil;
  CloseTable;
  ClearIndexInfo;
  StandartClause.Free;
  OrderClause.Free;
  RangeClause.Free;
  limitClause.Free;
  FNativeDescs.Free;
  FIndexDescs.Free;
  FFieldDescs.Free;
  FFilters.Free;
  Inherited Destroy;
end;

//////////////////////////////////////////////////////////
//            PROTECTED METHODS                         //
//////////////////////////////////////////////////////////
procedure TNativeDataSet.SetBufferAddress(P : Pointer);
begin
  FCurrentBuffer  := P;
end;

procedure TNativeDataSet.SetInternalBuffer(Buffer : Pointer);
begin
  if not FPreventRememberBuffer then //mi:2008-08-27 check if we need to remember buffer
  BufferAddress := Buffer;
end;

function TNativeDataSet.GetInternalBuffer : Pointer;
begin
  Result := FInternalBuffer;
end;

procedure TNativeDataSet.SetCurrentBuffer(PRecord : Pointer);
begin
  FCurrentBuffer := PRecord;
end;

function TNativeDataSet.GetCurrentBuffer : Pointer;
begin
  Result := FCurrentBuffer;
end;

function TNativeDataSet.FieldOffset(iField: Integer): Word;
var
   i: SmallInt;
begin
   Result:=0;
   if not ((iField>=1) or (iField<=FieldCount)) then Raise EPSQLException.CreateBDE(DBIERR_INVALIDPARAM);
   Dec(iField);
   Dec(iField);
   for i:=0 to iField do
   begin
     case FieldType(I) of
        FIELD_TYPE_INT2,
        FIELD_TYPE_BOOL: Inc(Result,SizeOf(SmallInt));
        FIELD_TYPE_OID,
        FIELD_TYPE_TEXT,
        FIELD_TYPE_BYTEA: Inc(Result,SizeOf(TBlobItem));
        FIELD_TYPE_INT4: Inc(Result,SizeOf(LongInt));
        FIELD_TYPE_INT8: Inc(Result,SizeOf(Int64));
        FIELD_TYPE_DATE,
        FIELD_TYPE_TIME: Inc(Result,SizeOf(TDateTime));
        FIELD_TYPE_TIMESTAMP: Inc(Result,SizeOf(TDateTime));
        FIELD_TYPE_FLOAT4,
        FIELD_TYPE_NUMERIC,
        FIELD_TYPE_FLOAT8:Inc(Result,SizeOf(Double));

     else
       Inc(Result,FieldMaxSizeInBytes(I));
     end;
   end;
end;

function TNativeDataSet.GetBookMarkSize : Integer;
begin
  Result := Sizeof(TPSQLBookMark);
end;

procedure TNativeDataSet.SetBufBookmark;
Var
  Buffer : Pointer;
begin
  if (CurrentBuffer <> nil) and (FBookOfs > 0) then
  begin
    Buffer := CurrentBuffer;
    Buffer := Pointer(Int64(Buffer) + FBookOfs);
    GetBookMark(Buffer);
  end;
end;

function TNativeDataSet.GetRecordNumber: Longint;
begin
 Result := RecNo;
end;

procedure TNativeDataSet.SetRecordNumber(RecNom : Longint);
var
  Original: LongInt;
begin
  Original := RecNom;
  if RecNom < 0 then
  begin
     RecNom := RecordCount;
     Dec(RecNom);
     try
       if RecordState <> tsEmpty then CurrentRecord(RecNom);
     except
     end;
   end else
     if RecordState <> tsEmpty then CurrentRecord(RecNom);
   Recno := Original;
end;

function TNativeDataSet.GetRecCount: LongInt;
begin
  if FStatement = nil then
     Result := 0
  else
     Result := PQntuples(FStatement)
end;

procedure TNativeDataSet.GetRecordCount( var iRecCount : Longint );
var
  P      : Pointer;
  Buff   : Pointer;
  Marked : Boolean;
begin
    if not FFilterActive then
      iRecCount := GetRecCount()
    else
      begin
        iRecCount := 0;
        GetMem(Buff, GetWorkBufferSize);
        try

          GetMem(P, BookMarkSize);
          try
            try
              GetBookMark(P);
              Marked := true;
            except
              on E:EPSQLException do
                Marked := false;
            end;

            SetToBegin();
            try
              repeat
                GetNextRecord(dbiNOLOCK, Buff, nil);
                Inc(iRecCount);
              until false;
            except
              on E:EPSQLException do;
            end;

            if Marked then
              SetToBookMark(P)
            else
              SetToBegin;
          finally
            FreeMem(P, BookMarkSize);
          end;
        finally
          FreeMem(Buff, GetWorkBufferSize);
        end;
    end;
end;

procedure TNativeDataSet.CheckFilter(PRecord : Pointer);
var
  P    : Pointer;
  B    : Boolean;
  aSize: integer;
begin
  if PRecord <> nil then
  begin
    if FFilterActive then
      While not FilteredRecord(PRecord) do
      begin
        InternalBuffer := PRecord;
        if FLastDir <> tdPrev then NextRecord else PrevRecord;
      end;
  end else
  begin
     if FFilterActive then
     begin
        aSize := GetWorkBufferSize;
        P := AllocMem(aSize);
       try
        InternalBuffer := P;
        InternalReadBuffer;
        B := FilteredRecord(P);
        While not B do
        begin
           InternalBuffer := P;
           if FLastDir <> tdPrev then
              NextRecord()
           else
              PrevRecord();
           B := FilteredRecord(P);
        end;
       finally
        FreeMem(P,aSize);
        InternalBuffer := nil;
       end;
     end;
  end;
end;

procedure TNativeDataSet.InternalCommit;
begin
  FConnect.Commit;
  FConnect.CheckResult;
end;

procedure TNativeDataSet.FirstRecord;
begin
  if Assigned(FStatement) then
  begin                               
    RecNo := 0;
    if RecNo >= RecordCount then
       raise EPSQLException.CreateBDE(DBIERR_EOF);
  end;
  InternalReadBuffer;
  SetBufBookmark;
  MonitorHook.SQLFetch(Self);
end;

procedure TNativeDataSet.LastRecord;
begin
  if Assigned(FStatement) then
  begin
     RecNo := ELSEIF(RecordCount>0, RecordCount-1, -1);
     if RecNo <= -1 then
        raise EPSQLException.CreateBDE(DBIERR_EOF);
  end;
  InternalReadBuffer;
  SetBufBookmark;
  MonitorHook.SQLFetch(Self);
end;

procedure TNativeDataSet.NextRecord();
begin
  if Assigned(FStatement) then
  begin
    Inc(RecNo);
    if RecNo >= RecordCount then
      raise EPSQLException.CreateBDE(DBIERR_EOF);
  end;
  InternalReadBuffer;
  SetBufBookmark;
  MonitorHook.SQLFetch(Self);
end;

procedure TNativeDataSet.PrevRecord();
begin
   if Assigned(FStatement) then
   begin
      if RecNo >= RecordCount then raise EPSQLException.CreateBDE(DBIERR_BOF);
      Dec(Recno);
      if RecNo <= -1 then
         raise EPSQLException.CreateBDE(DBIERR_BOF);
   end;
   InternalReadBuffer;
   SetBufBookmark;
   MonitorHook.SQLFetch(Self);
end;

procedure TNativeDataSet.CurrentRecord(ARecNo : Longint);
begin
  if Assigned(FStatement) then
     RecNo := ARecNo;
  if RecNo <= -1 then raise EPSQLException.CreateBDE(DBIERR_BOF);
  SetBufBookmark;
  InternalReadBuffer;
end;

procedure TNativeDataSet.GetWorkRecord(eLock: DBILockType;PRecord: Pointer);
var
  P : TPSQLBookMark;
begin
  GetBookMark(@P);
  CheckParam(@P=nil,DBIERR_INVALIDPARAM);
  if not FIsLocked then
  begin
    SetToBookMark(@P);
    if eLock = dbiWRITELOCK then LockRecord(eLock);
    RecordState := tsPos;
  end;
end;

procedure TNativeDataSet.GetRecordNo(var iRecNo: Longint);
begin
  iRecNo := RecordNumber;
end;

procedure TNativeDataSet.LockRecord(eLock : DBILockType);
begin
  FIsLocked := (eLock <> dbiNOLOCK);
end;

function TNativeDataSet.FilteredRecord(PRecord : Pointer) :  Boolean;
var
  P    : TPSQLFilter;
  I    : Integer;
begin
  Result := TRUE;
  if FFilterActive then
  begin
    For i := 0 to FFilters.Count-1 do
    begin
      P := FFilters.Items[i];
      if P.Active and not P.GetFilterResult(PRecord) then
      begin
        Result := FALSE;
        Exit;
      end;
    end;
  end;
end;

procedure TNativeDataSet.UpdateFilterStatus;
Var
  P : TPSQLFilter;
  I : Integer;
begin
  For i := 0 to FFilters.Count-1 do
  begin
    P := FFilters.Items[i];
    if (P <> NIL) and (P.Active) then
    begin
      FFilterActive := TRUE;
      Exit;
    end;
  end;
  FFilterActive := FALSE;
end;

procedure TNativeDataSet.NativeToDelphi(P: TPSQLField; PRecord: Pointer; pDest: Pointer; var bBlank: Boolean);
begin
  CheckParam(PRecord = nil, DBIERR_INVALIDPARAM);
  P.Buffer := PRecord;
  bBlank   := P.FieldNull;
  if not bBlank and (pDest <> nil) then AdjustNativeField(P,P.FieldValue,pDest,bBlank);
end;

procedure TNativeDataSet.DelphiToNative(P: TPSQLField;PRecord: Pointer;pSrc: Pointer);
begin
  if pSrc <> nil then AdjustDelphiField(P, pSrc, PAnsiChar(P.Data) + P.FieldNumber - 1);
end;

procedure TNativeDataSet.CheckParam(Exp : Boolean;BDECODE : Word);
begin
   if Exp then Raise EPSQLException.CreateBDE(BDECODE);
end;

/////////////////////////////////////////////////////////////////////
//                       PUBLIC METHODS                            //
/////////////////////////////////////////////////////////////////////
procedure TNativeDataSet.GetRecord(eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps);
begin
  InternalBuffer := PRecord;
  Case RecordState of
    tsPos:
      begin
        GetWorkRecord(eLock,PRecord);
        try
          CheckFilter(PRecord);
        except
          On E:EPSQLException do
          begin
            if FReRead then
            begin
              FReRead := FALSE;
              RecordState  := tsNoPos;
              GetNextRecord( eLock, PRecord, pRecProps );
            end
            else
            begin
              if eLock = dbiWRITELOCK then FIsLocked := FALSE;
              Raise;
            end;
          end;
        end;
         if pRecProps <> nil then
         begin
            pRecProps^.iPhyRecNum := RecNo+1;
            pRecProps^.iSeqNum := RecNo+1;
         end;
      end;
    tsFirst: Raise EPSQLException.CreateBDE(DBIERR_EOF);
    tsLast: Raise EPSQLException.CreateBDE(DBIERR_BOF);
    tsEmpty:
      begin
        try
          GetNextRecord( eLock, PRecord, pRecProps );
        except
          On E:EPSQLException do
          begin
            try
              GetPriorRecord( eLock, PRecord, pRecProps );
            except
              On E:EPSQLException do
              begin
                RecordState  := tsNoPos;
                GetNextRecord( eLock, PRecord, pRecProps );
              end;
            end;
          end;
        end;
      end;
    else Raise EPSQLException.CreateBDE(DBIERR_NOCURRREC);
  end;
end;

procedure TNativeDataSet.GetNextRecord(eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps);
begin
  FLastDir     := tdNext;
  InternalBuffer := PRecord;
  Case RecordState of
    tsPos,
    tsEmpty: NextRecord;
    tsFirst,
    tsNoPos: FirstRecord;
  else Raise EPSQLException.CreateBDE(DBIERR_EOF);
  end;
  CheckFilter(PRecord);
  if eLock <> dbiNOLOCK then GetRecord(eLock, PRecord, pRecProps);
  if pRecProps <> nil then
  begin
     pRecProps^.iPhyRecNum := RecNo+1;
     pRecProps^.iSeqNum := RecNo+1;
  end;
  RecordState := tsPos;
end;

procedure TNativeDataSet.GetPriorRecord(eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps);
begin
  FLastDir     := tdPrev;
  InternalBuffer := PRecord;
  Case RecordState of
    tsPos,
    tsEmpty: PrevRecord;
    tsLast,
    tsNoPos: LastRecord;
  else Raise EPSQLException.CreateBDE(DBIERR_BOF);
  end;
  CheckFilter(PRecord);
  if eLock <> dbiNOLOCK then GetRecord(eLock, PRecord, pRecProps);
  if pRecProps <> nil then
  begin
     pRecProps^.iPhyRecNum := RecNo+1;
     pRecProps^.iSeqNum := RecNo+1;
  end;
  RecordState := tsPos;
end;

procedure TNativeDataSet.AddFilter(iClientData: Longint;iPriority: Word;bCanAbort: Boolean;pcanExpr: pCANExpr;pfFilter: pfGENFilter;var hFilter: hDBIFilter);
var
  P : TPSQLFilter;
begin
  P := TPSQLFilter.Create(Self,iClientData,pcanExpr,pfFilter);
  FFilters.Insert(P);
  UpdateFilterStatus;
  hFilter := hDBIFilter(P);
end;

procedure TNativeDataSet.DropFilter(hFilter: hDBIFilter);
var
  Count : Integer;
begin
  if hFilter = NIL then FFilters.FreeAll else
  begin
    Count := FFilters.Count;
    FFilters.Delete(hFilter);
    if Count <> FFilters.Count then
    begin
      TPSQLFilter(hFilter).Free;
      UpdateFilterStatus;
    end;
  end;
end;

procedure TNativeDataSet.ActivateFilter(hFilter: hDBIFilter);
var
  i     : Integer;
  P     : TPSQLFilter;
  Found : Boolean;
begin
  Found := FALSE;
  For i := 0 to FFilters.Count-1 do
  begin
    P := FFilters.Items[i];
    if (hFilter = nil) or (hFilter = hDBIFilter(P)) then
    begin
      P.Active      := TRUE;
      FFilterActive := TRUE;
      Found         := TRUE;
    end;
  end;
  if not Found and (hFilter <> nil) then raise EPSQLException.CreateBDE(DBIERR_NOSUCHFILTER);
end;

procedure TNativeDataSet.DeactivateFilter(hFilter: hDBIFilter);
var
  i : Integer;
  P : TPSQLFilter;
begin
  if hFilter = nil then
  begin
    For i := 0 to FFilters.Count-1 do
    begin
      P := FFilters.Items[i];
      P.Active := FALSE;
    end;
    FFilterActive := FALSE;
  end else
  begin
    if TPSQLFilter( hFilter ).Active then
    begin
      TPSQLFilter( hFilter ).Active := FALSE;
      UpdateFilterStatus;
    end;
  end;
end;

procedure TNativeDataSet.SetToRecord(RecNo : LongInt);
begin
  if RecNo < 0 then
  begin
     try
       if RecordState <> tsEmpty then CurrentRecord(RecNo);
     except
     end;
  end
  else
     if RecordState <> tsEmpty then CurrentRecord(RecNo);
end;

procedure TNativeDataSet.SetToBookmark(P : Pointer);
begin
  CheckParam(P=nil,DBIERR_INVALIDPARAM);
  if TPSQLBookMark(P^).Position >= 1 then
     RecordNumber := TPSQLBookMark(P^).Position-1 else
     FirstRecord;
  RecordState := tsPos;
end;

procedure TNativeDataSet.OpenTable;
var
  sql_stmt : string;

    procedure InternalOpen;
    begin
       if SQLQuery = '' then
         if StandartClause.Count > 0  then
          sql_stmt := GetSQLClause
         else
          Raise EPSQLException.CreateBDE(DBIERR_QRYEMPTY)
       else
        sql_stmt := Trim(SQLQuery);
       FStatement := _PQExecute(FConnect, sql_stmt);
       if Assigned(FStatement) then
       begin
          try
            FConnect.CheckResult(FStatement);
            MonitorHook.SQLExecute(Self, True);
          except
            MonitorHook.SQLExecute(Self, False);
            CloseTable;
            Raise;
          end;
          FOpen := True;
          if FFieldDescs.Count = 0 then
             InitFieldDescs;
         RecNo := 0;
       end else
       begin
          FConnect.CheckResult;
          PQclear(FStatement);
       end;
    end;

begin
  if FOpen then CloseTable;
  FAffectedRows := 0;
  FOpen := False;
  sql_stmt := '';

  try
    if (StandartClause.Count = 0) and (SQLQuery = '') then
    begin
      isQuery := False;
      StandartClause.Add('SELECT * FROM ' + TableName);
      if FOpen then ClearIndexInfo;
      limitClause.Add('LIMIT 1');
      FSystemNeed := true;
      InternalOpen;
      FSystemNeed := False;
      limitClause.Clear;
      if FLimit > 0 then
         LimitClause.Add(Format('LIMIT %s',[IntToStr(FLimit)]));
      if FOffset > 0 then
         LimitClause.Add(Format('OFFSET %s',[IntToStr(FOffset)]));
      if IndexCount > 0 then
       begin
         if FPrimaryKeyNumber = 0 then FPrimaryKeyNumber := 1;
         SwitchToIndex(FIndexName, '', 0, False );
       end
      else
       begin
        PQClear(FStatement);
        InternalOpen;
       end;
      Exit;
    end;
    FLastOperationTime := GetTickCount;
    InternalOpen;
    FLastOperationTime := GetTickCount - FLastOperationTime;
    if (KeyNumber = 0) then
    begin
       if FPrimaryKeyNumber <> 0 then
          GetIndexDesc(FPrimaryKeyNumber, FKeyDesc) else
       if IndexCount > 0 then
         if FPrimaryKeyNumber <> 0 then
           GetIndexDesc(FPrimaryKeyNumber, FKeyDesc)
         else
           GetIndexDesc(1, FKeyDesc);
    end;
  finally
   SetLength(FFieldMinSizes,0);
   FFieldTypType := '';
   if FSortingFields > '' then
      SortBy(FSortingFields);
  end;
end;

procedure TNativeDataSet.GetField(FieldNo: Word;PRecord: Pointer;pDest: Pointer;var bBlank: Boolean);
var
  T    : TPSQLField;
begin
  CheckParam(PRecord=nil,DBIERR_INVALIDPARAM);
  T := FFieldDescs[FieldNo];
  T.Buffer := PRecord;
  if Assigned(pDest) then
    NativeToDelphi(T, PRecord, pDest, bBlank)
  else
    bBlank := T.FieldNull;
end;

procedure TNativeDataSet.PutField(FieldNo: Word;PRecord: Pointer;pSrc: Pointer);
var
  T : TPSQLField;
begin
  CheckParam(PRecord=nil,DBIERR_INVALIDPARAM);
  T := FFieldDescs[FieldNo];
  T.Buffer := PRecord;
  DelphiToNative(T, PRecord, pSrc);
  T.FieldChanged := TRUE;
  T.FieldNull := pSrc = nil;
end;

procedure TNativeDataSet.CloseTable;
begin
  FAffectedRows := 0;
  RecNo := -1;
  if not FConnect.FLoggin then exit;
  if FStatement <> nil then PQclear(FStatement);
  FStatement := nil;
  FOpen := False;
  SetLength(FFieldMinSizes,0);
  FFieldTypType := '';
end;

procedure TNativeDataSet.GetBookMark( P : Pointer );
begin
  ZeroMemory(P, BookMarkSize );
  With TPSQLBookMark(P^) do
    Position:= RecordNumber+1;
end;

procedure TNativeDataSet.GetVchkDesc(iValSeqNo: Word; var pvalDesc: VCHKDesc);
begin
  pvalDesc := Fields[iValSeqNo].ValCheck;
end;

procedure TNativeDataSet.GetCursorProps( var curProps : CURProps );
begin
  ZeroMemory(@curProps, SizeOf(curProps));
  With curProps do
  begin
    iFields := FieldCount;
    iRecSize  := RecordSize;
    iRecBufSize := GetWorkBufferSize;                     { Record size (physical record) }
    iValChecks      := FieldCount;
    iBookMarkSize   := BookMarkSize;                      { Bookmark size }
    bBookMarkStable := False;                             { Stable book marks }
    eOpenMode       := FOMode;                            { ReadOnly / RW }
    iSeqNums        := 1;                                 { 1: Has Seqnums; 0: Has Record# }
    exltMode        := xltNONE;                           { Translate Mode }
    bUniDirectional := True;                              { Cursor is uni-directional }
    eprvRights      := prvUNKNOWN;                        { Table  rights }
    iFilters        := FFilters.Count;                    { Number of Filters }
    if isQuery then
    begin
       iIndexes     := 0;
       iKeySize     := 0;
    end else
    begin
       iIndexes     := IndexCount;
       iKeySize     := FKeyDesc.iKeyLen;                  { Key size }
    end;
    bSoftDeletes    := False;
  end;
end;

procedure TNativeDataSet.GetFieldDescs(var pFDesc : TFLDDescList);
var
  i : Integer;
begin
  for i := Low(pFDesc) to High(pFDesc) do
    pFDesc[i] := Fields[i+1].Description;
end;

procedure TNativeDataSet.Execute;
begin
  if FOpen then CloseTable;
  FAffectedRows := 0;
  FStatement := nil;
  if not Assigned(FConnect) or not (FConnect.FLoggin) then  Exit;
  FLastOperationTime := GetTickCount;

  FStatement := _PQExecute(FConnect, SQLQuery);

  if FStatement <> nil  then
  begin
    try
      FConnect.CheckResult(FStatement);
      MonitorHook.SQLExecute(Self, True);
    except
      MonitorHook.SQLExecute(Self, False);
      CloseTable;
      raise;
    end;
    FAffectedRows := StrToIntDef(String(PQcmdTuples(FStatement)), 0);
    FLastOperationTime := GetTickCount - FLastOperationTime;
    PQclear(FStatement);
    FStatement := nil;
  end else
    FConnect.CheckResult;
  SQLQuery := '';
end;

function TNativeDataset.FieldCount: Integer;
begin
  if FStatement = nil then Result := 0
  else Result := PQnfields(FStatement);
end;

function TNativeDataSet.GetRecordSize: Integer;
var
   I, Size: Integer;
begin
   Size := 0;
   Result := 0;
   if FRecSize = -1 then
    begin
     if FStatement = nil then exit;

     for I := 1 to FieldCount do
        Inc(Size, Fields[i].NativeSize);

     Inc(Size, FieldCount);

     FRecSize := Size;
     Result := Size;
    end
   else
     Result := FRecSize;
end;

function TNativeDataSet.FieldName(FieldNum: Integer): String;
begin
  Result := '';
  if FStatement <> nil then
    Result := FConnect.RawToString(PQfname(FStatement, FieldNum));
end;

function TNativeDataSet.FieldIndex(FieldName: String): Integer;
var P: PAnsiChar;
begin
   Result := -1;
   if FStatement <> nil then
   begin
    P := FConnect.StringToRaw(FieldName);
    try
      Result := PQfnumber(FStatement, P);
    finally
      StrDispose(P);
    end;
   end;
end;

function TNativeDataSet.FieldSize(FieldNum: Integer): Integer;
begin
  Result := 0;
  if (FStatement <> nil) and (PQntuples(FStatement) > 0) then
     Result := PQgetlength(FStatement, GetRecNo, FieldNum);
end;

function TNativeDataSet.FieldMaxSizeInBytes(FieldNum: Integer): Integer;
var FT: cardinal;
begin
   FT := FieldType(FieldNum);
   case FT of
      FIELD_TYPE_BOOL,
      FIELD_TYPE_INT2:  Result := SizeOf(Smallint);

      FIELD_TYPE_INT4:  Result := SizeOf(Integer);

      FIELD_TYPE_INT8:  Result := SizeOf(Int64);

      FIELD_TYPE_DATE:  Result := Sizeof(TTimeStamp);

      FIELD_TYPE_TIME,
      FIELD_TYPE_TIMESTAMP: Result := SizeOf(TDateTime);

      FIELD_TYPE_FLOAT4,
      FIELD_TYPE_NUMERIC,
      FIELD_TYPE_FLOAT8: Result := Sizeof(Double);

      FIELD_TYPE_POINT: Result := SizeOf(TPSQLPoint);
      FIELD_TYPE_CIRCLE: Result := SizeOf(TPSQLCircle);
      FIELD_TYPE_BOX: Result := SizeOf(TPSQLBox);
      FIELD_TYPE_LSEG: Result := SizeOf(TPSQLLSeg);

      FIELD_TYPE_TEXT,
      FIELD_TYPE_BYTEA,
      FIELD_TYPE_OID: Result := SizeOf(TBlobItem);
      FIELD_TYPE_UUID: Result := UUIDLEN + 1;
   else
     begin
       case FT of
        FIELD_TYPE_UNKNOWN: Result := NAMEDATALEN;
        FIELD_TYPE_INET, FIELD_TYPE_CIDR: Result := INETLEN;
        FIELD_TYPE_MACADDR: Result := MACADDRLEN;
        FIELD_TYPE_TIMESTAMPTZ: Result := TIMESTAMPTZLEN;
        FIELD_TYPE_TIMETZ: Result := TIMETZLEN;
        FIELD_TYPE_NAME: Result := NAMEDATALEN;
       else
        Result := FieldMaxSize(FieldNum);
       end;
     {$IFDEF DELPHI_12}
       if FConnect.IsUnicodeUsed then
        Result := (Result + 1 )* SizeOf(Char) //we need two #0 bytes here 25.11.2008
       else
     {$ENDIF}
        Result := Result + 1;
     end;
   end;
end;

function TNativeDataSet.GetFieldTypType(Index: integer): AnsiChar;
var fCount: integer;
    fTypType: string;
    fTypeOid: oid;
    IsOK: boolean;
begin
  fCount := FieldCount();
  if (Length(FFieldTypType) < fCount) then
    FFieldTypType := StringOfChar(AnsiChar('u'), fCount); //unknown
  if FFieldTypType[Index + 1] = 'u' then
   begin
    fTypeOid := FieldType(Index);
    if fTypeOid < MAX_BUILTIN_TYPE_OID then
      FFieldTypType[Index + 1] := 'b' //base
    else
      begin
       fTypType := FConnect.SelectStringDirect('SELECT typtype FROM pg_catalog.pg_type WHERE oid = ' + UIntToStr(fTypeOid), IsOK, 0);
       if fTypType > '' then
         FFieldTypType[Index + 1] := AnsiChar(fTypType[1])
       else
         FFieldTypType[Index + 1] := 'X'; //failed to obtain
      end;
   end;
  Result := FFieldTypType[Index + 1];
end;

function TNativeDataSet.FieldMaxSize(FieldNum: Integer): Integer;
Var fMod: integer;
    fTypeOid: oid;
begin
  Result := 0;
  if FStatement <> nil then
   begin
     fMod := Max(PQfmod(FStatement, FieldNum), 0);
     fTypeOid := FieldType(FieldNum);
     case fTypeOid of
      FIELD_TYPE_BPCHAR,
      FIELD_TYPE_VARCHAR: Result := (fMod - 4);
      FIELD_TYPE_BIT,
      FIELD_TYPE_VARBIT: Result := fMod;

      FIELD_TYPE_NUMERIC: Result := fMod shr 16 and 65535 + 1; //frac delimiter
     else
      if fTypeOid > MAX_BUILTIN_TYPE_OID then  //suppose it's UDT or enum
          case FieldTypTypes[FieldNum] of //we're interested in composites & enums only
           'c': ;//composite TODO
           'e': Result := NAMEDATALEN; //enum
          end;
     end;
     if Result <= 0 then
       Result := FieldMinSize(FieldNum);
   end;
end;

function TNativeDataSet.FieldMinSize(FieldNum: Integer): Integer;
var
  I, H: Integer;
begin
  if dsoUDTAsMaxString in FOptions then
   begin
    Result := MAX_CHAR_LEN;
    Exit;
   end
  else
   Result := 0;
  if not Assigned(FFieldMinSizes) or
     (High(FFieldMinSizes) < FieldNum) or
     (FFieldMinSizes[FieldNum] = -1)
    then
     begin
      if Assigned(FFieldMinSizes) then
       H := High(FFieldMinSizes) + 1
      else
       H := 0;
      SetLength(FFieldMinSizes, FieldNum + 1);
      for i := H to High(FFieldMinSizes) - 1 do
        FFieldMinSizes[i] := -1;
      if FStatement <> nil then
        for I := 0 to PQntuples(FStatement) - 1 do
           if PQgetlength(FStatement, I, FieldNum) > Result then
              Result := PQgetlength(FStatement, I, FieldNum);
      if Result = 0 then
        Result := MAX_CHAR_LEN; //there is no field of length 0
      FFieldMinSizes[FieldNum] := Result;
     end
  else
    Result := FFieldMinSizes[FieldNum];
end;

function TNativeDataSet.FieldType(FieldNum: Integer): cardinal;
begin
  Result := InvalidOid;
  if FStatement <> nil then
     Result := PQftype(FStatement, FieldNum);
  case Result of
   FIELD_TYPE_OID: if dsoOIDAsInt in FOptions then Result := FIELD_TYPE_INT8;
   FIELD_TYPE_BYTEA: if dsoByteaAsEscString in FOptions then Result := FIELD_TYPE_TEXT;
   FIELD_TYPE_OIDVECTOR: Result := FIELD_TYPE_VARCHAR;
   FIELD_TYPE_CID,
   FIELD_TYPE_XID,
   FIELD_TYPE_TID: Result := FIELD_TYPE_INT4;
  else
   if (Result = FIELD_TYPE_VARCHAR) AND
          ((PQfmod(FStatement, FieldNum) < 0) or (PQfmod(FStatement, FieldNum) > MAX_CHAR_LEN))
      or
      (Result = FIELD_TYPE_XML) then
         Result := FIELD_TYPE_TEXT; //added to deal with varchar without length specifier
  end;       
end;

function TNativeDataSet.Field(FieldNum: Integer): string;
begin
  Result := '';
  if FStatement = nil then Exit;
  Result := FConnect.RawToString(PQgetvalue(FStatement,GetRecNo,FieldNum));
  if Fieldtype(FieldNum) = FIELD_TYPE_BPCHAR then
     Result := TrimRight(Result);
end;

function TNativeDataSet.FieldByName(FieldName: String): string;
begin
  Result := Field(FieldIndex(FieldName));
end;

function TNativeDataSet.FieldIsNull(FieldNum: Integer): Boolean;
begin
  Result := true;
  if FStatement <> nil then
     Result := PQgetisnull(FStatement,GetRecNo,FieldNum) <> 0;
end;

function TNativeDataSet.FieldBuffer(FieldNum: Integer): PAnsiChar;
begin
  Result := nil;
  if (FStatement = nil) or (PQgetisnull(FStatement, GetRecNo, FieldNum) <> 0) then Exit;
  Result := PQgetvalue(FStatement, GetRecNo, FieldNum);
end;

function TNativeDataSet.GetFieldInfo(Index : Integer) : TPGFIELD_INFO;
var
   Item : TPSQLNative;
   I : Integer;
   DefSL: TStrings;

   function GetDefault(const FieldNum: integer): string;
    var j: integer;
   begin
     Result := '';
     for j:=0 to DefSL.Count-1 do
      if integer(DefSL.Objects[j]) = FieldNum then
       begin
        Result := DefSL[j];
        DefSL.Delete(j);
        Break;
       end;
   end;

begin
  if FNativeDescs.Count = 0 then
  begin
     DefSL := TStringList.Create;
    try
     FillDefs(DefSL);
     for I := 0 to FieldCount -1 do
       FNativeDescs.SetNative(I, FieldName(I), FieldType(I), FieldMaxSizeInBytes(I), FieldMaxSize(I), GetDefault(I));
    finally
     DefSL.Free;
    end;
  end;
  Item := TPSQLNative(FNativeDescs.Items[Index]);
  if Item <> nil then
     Result := Item.FDesc;
end;

procedure TNativeDataSet.GetNativeDesc(FieldNo : Integer; var P : FldDesc; var P1 : VCHKDesc; Var LocType, LocSize : Word; var LocArray : Boolean);
var
  Fld : TPGFIELD_INFO;
begin
  CheckParam(not (FieldNo <= FieldCount), DBIERR_INVALIDRECSTRUCT);
  FLD := FieldInfo[FieldNo-1];
  ConverPSQLtoDelphiFieldInfo(FLD, FieldNo, FieldOffset(FieldNo), P, P1, LocArray);
  LocType := FieldType(FieldNo-1);
  case Loctype of
    FIELD_TYPE_BYTEA,
    FIELD_TYPE_TEXT,
    FIELD_TYPE_OID: LocSize := SizeOf(TBlobItem);
  else
    LocSize := FieldMaxSizeInBytes(FieldNo-1);
  end;
end;

procedure TNativeDataSet.FillDefs(SL: TStrings);
Var inS: String;
    i, j, fPos: integer;
    tabOID: cardinal;
    RES: PPGresult;
    sql: String;
const
      tS = ' c.oid = %d AND ad.adnum = %d ';

begin
 if not Assigned(SL) then Exit;
 if IsQuery and (FOMode = dbiReadOnly) then Exit;
 sql := 'SELECT ad.adnum, '+
        ' c.oid, '+                      // AS col_number_in_source_table
        ' pg_get_expr(ad.adbin, ad.adrelid) '+                      // AS column_default
        ' FROM  pg_attrdef ad, '+
        ' pg_class c'+
        ' WHERE ad.adrelid = c.oid AND '+
        ' (%s) ';                          //c.oid = %d AND ad.adnum = %d OR ...

 if not isQuery then
   inS := ' c.oid = ' + IntToStr(FieldTable(0))
 else
   for i:=0 to FieldCount-1 do
    begin
     tabOID := FieldTable(I);
     fPos := FieldPosInTable(I);
     if (tabOID > InvalidOid) and (fPos > -1) then
       if inS > '' then
          inS := inS + 'OR' + Format(ts,[tabOID,fPos])
       else
          inS := Format(ts,[tabOID,fPos]);
    end;
 if inS > '' then
  begin
    sql := Format(sql,[inS]);
    Res := PQExec(FConnect.Handle, PAnsiChar(AnsiString(sql)));
    if Assigned(RES) then
     try
      FConnect.CheckResult;
      for i:=0 to PQntuples(RES)-1 do
       for j:=0 to FieldCount-1 do
         if (IntToStr(FieldTable(j)) = FConnect.RawToString(PQGetValue(Res,i,1))) and
            (IntToStr(FieldPosInTable(j)) = FConnect.RawToString(PQGetValue(Res,i,0))) then
         SL.AddObject(FConnect.RawToString(PQgetvalue(RES,i,2)), TObject(j));
     finally
      PQclear(RES);
     end;
  end;
end;

procedure TNativeDataSet.InitFieldDescs;
var
  i         : Integer;
  FldInfo   : FLDDesc;
  ValCheck  : VCHKDesc;
  LocalType, LocalSize, NullOffset, RecSize: Word;
  LocArray : Boolean;
begin
   Fields.Clear;
   For i := 1 to FieldCount() do
    begin
      GetNativeDesc(i, FldInfo, ValCheck, LocalType, LocalSize, LocArray);
      Fields.AddField(FldInfo, ValCheck, i, LocalType, LocalSize, LocArray);
      Finalize(FldInfo);  //without this calls we have memory leak
      Finalize(ValCheck);
    end;

   RecSize  := RecordSize;
   NullOffset := RecSize+1;
   For i := 1 to Fields.Count do
   begin
      Fields[i].NullOffset := NullOffset;
      Inc(NullOffset, SizeOf(TFieldStatus));
   end;
end;

function TNativeDataSet.GetBufferSize : Word;
begin
  if FFieldDescs.Count = 0 then InitFieldDescs;
  Result := RecordSize;
end;

function TNativeDataSet.GetWorkBufferSize : Word;
begin
  Result := GetBufferSize;
  Inc(Result, FFieldDescs.Count * SizeOf(TFieldStatus) + 1);
  FBookOfs := Result;
  if FBookOfs > 0 then Inc(Result, BookMarkSize);
end;

procedure TNativeDataSet.GetProp(iProp: Longint;PropValue: Pointer;iMaxLen: Word;var iLen: Word);
begin
  iLen := 0;
  Case TPropRec( iProp ).Prop of
    Word( curMAXPROPS ): begin
                            iLen := SizeOf(Word);
                            Word(PropValue^) := maxCurProps;
                         end;
    Word( curXLTMODE ):  begin
                            iLen := SizeOf(xltMODE);
                            xltMODE( PropValue^ ) := xltNONE;
                         end;
    Word(curMAXFIELDID): begin
                            iLen := iMaxLen;
                            Integer( PropValue^ ) := FFieldDescs.Count;
                         end;
    Word(stmtROWCOUNT):  begin
                            iLen := SizeOf(Integer);
                            Integer(PropValue^) := FAffectedRows;
                         end;
    Word(curAUTOREFETCH):begin
                            iLen := SizeOf(Boolean);
                            Boolean(PropValue^) := FReFetch;
                         end;
  end;
end;

procedure TNativeDataSet.SetProp(iProp: Longint;PropValue: Longint);
begin
  Case TPropRec( iProp ).Prop of
    Word(curMAKECRACK): RecordState := tsEmpty;
    Word(stmtLIVENESS): begin
                          if PropValue = 1 then
                              FOMode := dbiReadWrite else
                              FOMode := dbiREADONLY;
                        end;
    Word(curAUTOREFETCH): FReFetch := PropValue > 0;
  end;
end;

procedure TNativeDataSet.SetToBegin;
begin
  RecordState  := tsFirst;
end;

procedure TNativeDataSet.SetToEnd;
begin
  RecordState  := tsLast;
end;


procedure TNativeDataSet.InternalReadBuffer;
var
  i, size: Integer;
  null: boolean; //temp var used for work with dsoEmptyCharAsNull
  MaxSize, tMS : Integer;
  aFType: integer;
  T: TPSQLField;
  origBuffer: Pointer;
  FldValue : String;
  Data : pointer;
  _CharSize: integer;
{$IFDEF DELPHI_5}
const
  MinDateTime: TDateTime = -657434.0;      { 01/01/0100 12:00:00.000 AM }
  MaxDateTime: TDateTime =  2958465.99999; { 12/31/9999 11:59:59.999 PM }
{$ENDIF}

begin
//   T := nil;
   if assigned(FCurrentBuffer) then
   begin
       MaxSize := 0;
       if FConnect.IsUnicodeUsed then
          _CharSize := SizeOf(Char)
       else
          _CharSize := SizeOf(AnsiChar);
       for i:=0 to FieldCount-1 do
       begin
           aFType := FieldType(I);
           if (aFType <> FIELD_TYPE_OID) and
              (aFType <> FIELD_TYPE_TEXT) and
              (aFType <> FIELD_TYPE_BYTEA) then
           begin
             case aFType of
              FIELD_TYPE_TIMESTAMPTZ:
                 if TIMESTAMPTZLEN * _CharSize > MaxSize then MaxSize := TIMESTAMPTZLEN * _CharSize;
              FIELD_TYPE_TIMETZ:
                 if TIMETZLEN * _CharSize > MaxSize then MaxSize := TIMETZLEN * _CharSize;
              FIELD_TYPE_NAME:
                 if NAMEDATALEN * _CharSize > MaxSize then MaxSize := NAMEDATALEN * _CharSize;
              FIELD_TYPE_DATE,
              FIELD_TYPE_TIME,
              FIELD_TYPE_TIMESTAMP:
                 if SizeOf(TDateTime) > MaxSize then MaxSize := SizeOf(TDateTime);
              FIELD_TYPE_INT4:
                 if SizeOf(Integer) > MaxSize then MaxSize := SizeOf(Integer);
              FIELD_TYPE_INT8:
                 if SizeOf(Int64) > MaxSize then MaxSize := SizeOf(Int64);
              FIELD_TYPE_FLOAT4,
              FIELD_TYPE_FLOAT8,
              FIELD_TYPE_NUMERIC:
                 if SizeOf(Double) > MaxSize then MaxSize := SizeOf(Double);
              FIELD_TYPE_POINT:
                 if SizeOf(TPSQLPoint) > MaxSize then MaxSize := SizeOf(TPSQLPoint);
              FIELD_TYPE_CIRCLE:
                 if SizeOf(TPSQLCircle) > MaxSize then MaxSize := SizeOf(TPSQLCircle);
              FIELD_TYPE_BOX:
                 if SizeOf(TPSQLBox) > MaxSize then MaxSize := SizeOf(TPSQLBox);
              FIELD_TYPE_LSEG:
                 if SizeOf(TPSQLLSeg) > MaxSize then MaxSize := SizeOf(TPSQLLSeg);
             else
              tMS := FieldMaxSizeInBytes(I);
             if tMS > MaxSize then MaxSize := tMS;
             end;
           end;
       end;
       GetMem(Data, MaxSize+1);
       origBuffer := FCurrentBuffer;
       for i:=0 to FieldCount-1 do
       begin

          T := Fields[i+1];
          T.Buffer  := origBuffer;
          T.FieldChanged := FALSE;
          null := FieldIsNull(I);
          T.FieldNull    := null;

          size := T.NativeSize; //FieldLength
          if null then
              ZeroMemory(FCurrentBuffer,size)
          else
            begin
             if (T.NativeType <> FIELD_TYPE_OID) and
                (T.NativeType <> FIELD_TYPE_TEXT) and
                (T.NativeType <> FIELD_TYPE_BYTEA)
               then
                 FldValue := FConnect.RawToString(FieldBuffer(i));
             case T.NativeType of
               FIELD_TYPE_INT2: SmallInt(Data^) := SmallInt(StrToint(FldValue));
               FIELD_TYPE_BOOL: if FldValue = 't' then
                                   SmallInt(Data^) := SmallInt(1) else
                                   SmallInt(Data^) := SmallInt(0);
               FIELD_TYPE_INT4: LongInt(Data^) := LongInt(StrToint(FldValue));
               FIELD_TYPE_INT8: Int64(Data^) := StrToInt64(FldValue);
               FIELD_TYPE_DATE:   TDateTime(Data^) := SQLDateToDateTime(FldValue,False);
               FIELD_TYPE_TIME:   TDateTime(Data^) := SQLDateToDateTime(FldValue,True);
               FIELD_TYPE_TIMESTAMP: if FldValue = 'infinity' then
                                       TDateTime(Data^) := MaxDateTime
                                     else
                                       if FldValue = '-infinity' then
                                         TDateTime(Data^) := MinDateTime
                                       else
                                         TDateTime(Data^) := SQLTimeStampToDateTime(FldValue);
               FIELD_TYPE_FLOAT4,
               FIELD_TYPE_FLOAT8,
               FIELD_TYPE_NUMERIC:   Double(Data^) := StrToSQLFloat(FldValue);
               FIELD_TYPE_POINT:     TPSQLPoint(Data^) := SQLPointToPoint(FldValue);
               FIELD_TYPE_CIRCLE:    TPSQLCircle(Data^) := SQLCircleToCircle(FldValue);
               FIELD_TYPE_BOX:       TPSQLBox(Data^) := SQLBoxToBox(FldValue);
               FIELD_TYPE_LSEG:      TPSQLLSeg(Data^) := SQLLSegToLSeg(FldValue);
               FIELD_TYPE_OID,
               FIELD_TYPE_TEXT,
               FIELD_TYPE_BYTEA:     begin
                                        size := SizeOf(TBlobItem);
                                        ZeroMemory(FCurrentBuffer, Size);
                                        Inc(PAnsiChar(FCurrentBuffer)); //Null byte allocate
                                        Inc(PAnsiChar(FCurrentBuffer), Size); //Pointer allocate
                                        continue;
                                    end;
               FIELD_TYPE_UUID: StrCopy(PAnsiChar(Data), PAnsiChar(BadGuidToGuid(AnsiString(FldValue))));
             else
              // if (T.NativeType = FIELD_TYPE_UUID) then
              //   FldValue := string(BadGuidToGuid(AnsiString(FldValue)));

               if dsoTrimCharFields in FOptions then
                 FldValue := TrimRight(FldValue);

               if FConnect.IsUnicodeUsed then
               {$IFDEF DELPHI_12}
                 StrCopy(PWideChar(Data), PWideChar(FldValue))
               {$ELSE}
                 StrCopy(PAnsiChar(Data), PAnsiChar(FldValue))
               {$ENDIF}
               else
                StrCopy(PAnsiChar(Data), PAnsiChar(AnsiString(FldValue)));
             end;
             move(Data^, (PAnsiChar(FCurrentBuffer)+1)^, Size);
             PAnsiChar(FCurrentBuffer)^ := #1; {null indicator 1=Data 0=null}
          end;
          Inc(PAnsiChar(FCurrentBuffer), Size+1); {plus 1 for null byte}
       end;
       FreeMem(Data,MaxSize+1);
       FCurrentBuffer:=nil;
   end;
end;

procedure TNativeDataSet.ReadBlock(var iRecords : Longint; pBuf : Pointer);
var
  M     : MemPtr;
  i     : Word;
  Limit : longint;
begin
  Limit     := iRecords;
  iRecords  := 0;

  CheckParam(pBuf = nil, DBIERR_INVALIDPARAM);

  M := pBuf;
  i := 0;

  repeat
    GetNextRecord(dbiNOLOCK, @M^[ i ], NIL);
    Inc(iRecords);

    if iRecords >= Limit then
      Break
    else
      Inc(i,GetWorkBufferSize);

  until False;
end;

procedure TNativeDataSet.ForceReread;
var
  P : TPSQLBookMark;
begin
  GetBookMark(@P);
  FReRead := TRUE;
  ReOpenTable;
  if RecordCount = 0 then
  begin
     ReOpenTable;
  end;
  SetToBookmark(@P);
end;

procedure TNativeDataSet.CompareBookMarks( pBookMark1, pBookMark2 : Pointer; var CmpBkmkResult : CmpBkmkRslt );

  function cmp2Values(val1, val2: LongInt): CmpBkmkRslt;
  begin
     if val1=val2 then result:=CMPEql else
     if val1 < val2 then result:=CMPLess else
        result:=CMPGtr;
  end;

begin
  CheckParam(pBookMark1=nil,DBIERR_INVALIDPARAM);
  CheckParam(pBookMark2=nil,DBIERR_INVALIDPARAM);
  if (TPSQLBookMark(pBookMark1^).Position <> 0) then
    CmpBkMkResult:=cmp2Values( TPSQLBookMark(pBookMark1^).Position, TPSQLBookMark(pBookMark2^).Position) else
    CmpBkMkResult := CMPGtr;
end;

procedure TNativeDataSet.InitRecord(PRecord : Pointer);
begin
  if PRecord = nil then Raise EPSQLException.CreateBDE(DBIERR_INVALIDPARAM);
  ZeroMemory(PRecord, GetWorkBufferSize);
  FFieldDescs.SetFields(PRecord);
  CurrentBuffer := PRecord;
end;


procedure TNativeDataSet.GetKeys(Unique: Boolean; var FieldList: TFieldArray; var FieldCount: Integer);
var
  I, N: Integer;
  Item : TPSQLIndex;
  Fld  : TPSQLField;
begin
  N := -1;
  FieldCount := 0;
  //Search for PrimaryKey
  for I := 1 to FindexDescs.Count do
  begin
    Item := FIndexDescs.mIndex[I];
    if Item.Primary then
    begin
       N := I;
       Break;
    end;
  end;
  if N = -1 then
     //Primary key not found.
     //Search for Unique Key
     for I := 1 to FindexDescs.Count do
     begin
        Item := FIndexDescs.mIndex[I];
        if Item.Unique then
        begin
           N := I;
           break;
        end;
     end;
  if N >= 0 then
  begin
    Item := FindexDescs.mIndex[N];
    for I := 0 to Item.FldsInKey-1 do
    begin
       FieldList[FieldCount] := Item.FDesc.aiKeyFld[I];
       Inc(FieldCount);
    end;
  end
  else
  if not Unique then
  begin
     for I := 1 to FFieldDescs.Count do
    begin
      Fld := FFieldDescs.Field[I];
      if not(Fld.FieldType in [fldBlob]) then
      begin
        if Fld.FDesc.bCalcField then continue;
        FieldList[FieldCount] := I;
        Inc(FieldCount);
      end;
    end;
  end;
end;

function TNativeDataSet.GetLOUnlinkSQL(ObjOID: cardinal): string;
begin
  Result := GetLOUnlinkSQL(UIntToStr(ObjOID));
end;

function TNativeDataSet.GetLOUnlinkSQL(ObjOID: string): string;
const
  _LoMng: string = #13#10'SELECT CASE WHEN EXISTS(SELECT 1 FROM pg_catalog.pg_largeobject WHERE loid = %) THEN lo_unlink(%) END;';
begin
  Result := StringReplace(_LoMng, '%', ObjOID, [rfReplaceAll]);
end;

function TNativeDataSet.GetDeleteSQL(Table: string; PRecord: Pointer): string;
var
  FieldCount, I: Integer;
  FieldList  : TFieldArray;
  Fld        : TPSQLField;
begin
  Result := '';
  GetKeys(False, FieldList, FieldCount);
  for I := 0 to FieldCount-1 do
  begin
    Fld := FFieldDescs.Field[FieldList[I]];
    Fld.Buffer:= PRecord;
    if Result <> '' then  Result := Result + ' AND ';
    Result := Result + AnsiQuotedStr(Fld.FieldName, '"');
    if Fld.FieldNull then
      Result := Result + ' IS NULL'
    else
      Result := Result + '=' + Fld.FieldValueAsStr;
  end;
  if Result = '' then Exit;
  Result := 'DELETE FROM ' + Table + ' WHERE ' + Result;

  if not (dsoManageLOFields in FOptions) then Exit;
  for I := 1 to FFieldDescs.Count do
   begin
    Fld := FFieldDescs.Field[I];
    if (Fld.NativeBLOBType = nbtOID) and not FieldIsNull(I-1) then
      Result := GetLOUnlinkSQL(Field(I-1)) + #13#10 + Result;
   end;
end;

procedure TNativeDataSet.FreeBlobStreams(PRecord: Pointer);
var
  I    : Integer;
begin
  for I := 1 to FFieldDescs.Count do
    if FFieldDescs.Field[I].FieldType = fldBLOB then
      FreeBlob(PRecord,i);
end;

function TNativeDataSet.GetInsertSQL(Table: string; PRecord: Pointer; ReturnUpdated: boolean = False): string;
var
  I    : Integer;
  Fld    : TPSQLField;
  Fields : String;
  Values : String;

  function GetRETURNING: string;
  var I: integer;
  begin
    Result := '';
    if not ReturnUpdated then Exit;
    if FieldCount > 0 then Result := ' RETURNING ' + AnsiQuotedStr(FieldName(0), '"');
    for I := 1 to FieldCount-1 do
       Result := Result + ', ' + AnsiQuotedStr(FieldName(I), '"');
  end;

begin
  Result := '';
  Fields := '';
  for I := 1 to FFieldDescs.Count do
  begin
    Fld := FFieldDescs.Field[I];
    Fld.Buffer:= PRecord;
    if (Fld.FieldNull) and (not Fld.FValCheck.bHasDefVal) then continue;
    Fields := Fields + AnsiQuotedStr(Fld.FieldName, '"') + ', ';
    if (Fld.FieldNull) and (Fld.FValCheck.bHasDefVal) then
       Values := Values + 'DEFAULT, '
    else
      Values := Values + Fld.FieldValueAsStr + ', ';
  end;
  Delete(Fields, Length(Fields)-1, 2);
  Delete(Values, Length(Values)-1, 2);
  if (Fields <> '') and (Values <> '') then
   Result := Format('INSERT INTO %s (%s) VALUES (%s)', [Table, Fields, Values]) + GetReturning();
end;

function TNativeDataSet.GetUpdateSQL(Table: string; OldRecord,PRecord: Pointer; ReturnUpdated: boolean = False): String;
var
  I: Integer;
  Fld: TPSQLField;
  FldName, Values: string;

  function GetWHERE(P : Pointer) : String;
  var
      I, FieldCount: Integer;
    FieldList  : TFieldArray;
    Fld        : TPSQLField;
    FldName: string;
  begin
    Result := '';
    GetKeys(False, FieldList, FieldCount);
    for I := 0 to FieldCount-1 do
    begin
      Fld := FFieldDescs.Field[FieldList[I]];
      Fld.Buffer:= P;
      FldName := AnsiQuotedStr(Fld.FieldName, '"');
        if Result <> '' then  Result := Result + ' AND ';
        if Fld.FieldNull then
          Result := Result + FldName + ' IS NULL'
                        else
          Result := Result + FldName + '=' + Fld.FieldValueAsStr;
    end;
      Result := ' WHERE ' + Result;
  end;

  function GetRETURNING: string;
  var I: integer;
  begin
    Result := '';
    if not ReturnUpdated then Exit;
    if FieldCount > 0 then Result := ' RETURNING ' + AnsiQuotedStr(FieldName(0), '"');
    for I := 1 to FieldCount-1 do
       Result := Result + ', ' + AnsiQuotedStr(FieldName(I), '"');
  end;

begin
  Result := '';
  for I := 1 to FFieldDescs.Count do
  begin
    Fld := FFieldDescs.Field[I];
    Fld.Buffer:= PRecord;
    if not Fld.FieldChanged then Continue;
    if (dsoManageLOFields in FOptions) and (Fld.NativeBLOBType = nbtOID) and not FieldIsNull(I-1) then
      Result := Result + GetLOUnlinkSQL(Field(I-1));
    FldName := AnsiQuotedStr(Fld.FieldName, '"');
    if Fld.FieldNull then
       Values := Values + FldName + '=NULL, '
    else
       Values := Values + FldName + '=' + Fld.FieldValueAsStr + ', ';
  end;
  Delete(VALUES,Length(Values)-1,2);
  if VALUES > '' then
    Result := Format('UPDATE %s SET %s', [Table, Values]) + GetWhere(OldRecord) + GetRETURNING() + ';' + Result; //LOUnlinkSql at the end
end;

procedure TNativeDataSet.AppendRecord (PRecord : Pointer);
begin
  InsertRecord(dbiNOLOCK, PRecord);
end;

procedure TNativeDataSet.InsertRecord( eLock : DBILockType; PRecord : Pointer );
var
  SQL : String;
  OldQueryFlag: boolean;
  KN, i: integer;
  AStatement, ATempCopyStmt: PPGResult;
  fval, fname: PAnsiChar;
  flen: integer;
  CurrentRecNum: integer;
begin
  AStatement := nil;

  KN := -1;
  if FOMode = dbiREADONLY then
     raise EPSQLException.CreateBDE(DBIERR_TABLEREADONLY);
  CheckUniqueKey(KN);

  SQL := GetInsertSQL(TableName, PRecord, dsoRefreshModifiedRecordOnly in FOptions);
  try
    AStatement := _PQExecute(FConnect, SQL);
    if Assigned(AStatement) then
      try
        FConnect.CheckResult(AStatement);
        MonitorHook.SQLExecute(Self, True);
        FAffectedRows := StrToIntDef(string(PQcmdTuples(AStatement)), 0);
        CurrentRecNum := PQntuples(FStatement);
        if (FAffectedRows > 0) and (dsoRefreshModifiedRecordOnly in Options) then
          begin
           ATempCopyStmt := PQcopyResult(FStatement, PG_COPYRES_TUPLES); //hack because libpq have some bugs. must be eliminated further
           if not Assigned(ATempCopyStmt) then
             raise EPSQLException.CreateMsg(FConnect, 'Refresh for inserted fiels failed, cannot copy results');
           for i := 0 to PQnfields(AStatement) - 1 do
             begin
               fval := PQgetvalue(AStatement, 0, i);
               fname := PQfname(AStatement, i);
               flen := PQgetlength(AStatement, 0, i);
               if PQsetvalue(ATempCopyStmt, CurrentRecNum, i, fval, flen) = 0 then
                 raise EPSQLException.CreateFmt('Refresh for inserted fiels "%s" failed', [fname]);
             end;
           PQclear(FStatement);
           FStatement := ATempCopyStmt;
           RecordState := tsPos;
           SettoSeqNo(RecordCount); //tuple added to the end
           FReFetch := False;
          end;
      except
        MonitorHook.SQLExecute(Self, False);
        raise;
      end
    else
      FConnect.CheckResult;
  finally
    PQclear(AStatement);
  end;

  FreeBlobStreams(PRecord);
  InternalBuffer := nil;
  if (FAffectedRows > 0) and not (dsoRefreshModifiedRecordOnly in Options) then
    if not FReFetch then
     begin
       OldQueryFlag := IsQuery;
       ReOpenTable;
       IsQuery := OldQueryFlag;
       RecordState := tsPos;
       try
        if not SetRowPosition(KN,GetLastInsertID(KN),PRecord) then
            SettoSeqNo(RecordCount);
       except
       end;
     end;
  FIsLocked := FALSE;
end;

procedure TNativeDataSet.ModifyRecord(OldRecord,PRecord : Pointer; bFreeLock : Boolean; ARecNo : Longint);
var
  SQL : String;
  OldQueryFlag: boolean;
  KN : Integer;
  i: integer;
  CurrentRecNum: LongInt;
  AStatement: PPGResult;
  fval, fname: PAnsiChar;
  flen: integer;
begin
  KN := -1;
  AStatement := nil;
  CurrentRecNum := RecNo;
  if FOMode = dbiREADONLY then
     raise EPSQLException.CreateBDE(DBIERR_TABLEREADONLY);
  CheckUniqueKey(KN);
  try
    SQL := GetUpdateSQL(TableName, OldRecord, PRecord, dsoRefreshModifiedRecordOnly in FOptions);
    if SQL <> '' then
      //FConnect.QExecDirect(SQL, nil, FAffectedRows);
      AStatement := _PQExecute(FConnect, SQL);
      if Assigned(AStatement) then
        try
          FConnect.CheckResult(AStatement);
          MonitorHook.SQLExecute(Self, True);
          FAffectedRows := StrToIntDef(string(PQcmdTuples(AStatement)), 0);
          if (FAffectedRows > 0) and (dsoRefreshModifiedRecordOnly in Options) then
           for i := 0 to PQnfields(AStatement) - 1 do
            begin
             fval := PQgetvalue(AStatement, 0, i);
             fname := PQfname(AStatement, i);
             flen := PQgetlength(AStatement, 0, i);
             if PQsetvalue(FStatement, CurrentRecNum, i, fval, flen) = 0 then
               raise EPSQLException.CreateFmt('Refresh for modifed fiels "%s" failed', [fname]);
            end;
          FReFetch := False;
          RecordState := tsPos;
        except
          MonitorHook.SQLExecute(Self, False);
          raise;
        end
      else
        FConnect.CheckResult;
  finally
    PQclear(AStatement);
  end;

  FreeBlobStreams(OldRecord);
  FreeBlobStreams(PRecord);
  InternalBuffer := nil;
  if bFreeLock then LockRecord(dbiNOLOCK);

  if (FAffectedRows > 0) and not (dsoRefreshModifiedRecordOnly in Options) then
    if not FReFetch then
     begin
       OldQueryFlag := IsQuery;
       ReOpenTable;
       IsQuery := OldQueryFlag;
       RecordState := tsPos;
       try
         if not SetRowPosition(KN, 0, PRecord) then
            SettoSeqNo(CurrentRecNum + 1);
       except
       end;
     end;
  FIsLocked := FALSE;
end;

procedure TNativeDataSet.DeleteRecord(PRecord : Pointer);
var
  SQL : String;
  CurrentRecNum: LongInt;
  ATempCopyStmt: PPGResult;
  fval: PAnsiChar;
  flen, i, j: integer;
begin
  if FOMode = dbiREADONLY then
     raise EPSQLException.CreateBDE(DBIERR_TABLEREADONLY);

  InternalBuffer := PRecord;
  SQL := GetDeleteSQL(TableName, PRecord);
  if Sql <> '' then
   begin
    FConnect.QExecDirect(SQL, nil, FAffectedRows);
    RecordState := tsEmpty;
   end;

  FreeBlobStreams(PRecord);
  InternalBuffer := nil;

  if (FAffectedRows > 0) and (dsoRefreshModifiedRecordOnly in Options) then
    begin
     ATempCopyStmt := PQcopyResult(FStatement, PG_COPYRES_ATTRS); //hack because libpq have some bugs. must be eliminated further
     if not Assigned(ATempCopyStmt) then
       raise EPSQLException.CreateMsg(FConnect, 'Refresh for deleted fiels failed, cannot copy results');
     j := 0;
     for CurrentRecNum := 0 to RecordCount - 1 do
      begin
       if CurrentRecNum = RecNo then Continue; //exclude row from new set and check bounds
       for i := 0 to PQnfields(FStatement) - 1 do
         begin
           fval := PQgetvalue(FStatement, CurrentRecNum, i);
           flen := PQgetlength(FStatement, CurrentRecNum, i);
           if PQsetvalue(ATempCopyStmt, j, i, fval, flen) = 0 then
             raise EPSQLException.CreateFmt('Refresh for deleted fiels failed', []);
         end;
       inc(j);
      end;
     PQclear(FStatement);
     FStatement := ATempCopyStmt;
     RecordState := tsPos;
     try
       SettoSeqNo(Min(RecNo + 1, RecordCount));
     except
     end;
  end;

  if (FAffectedRows > 0) and not (dsoRefreshModifiedRecordOnly in Options) then
    if not FReFetch then
    begin
     CurrentRecNum := RecNo;
     ReOpenTable;
     RecordState := tsPos;
     if CurrentRecNum >= RecordCount then
       CurrentRecNum := RecordCount;
     try
       SettoSeqNo(CurrentRecNum);
     except
     end;
    end;
  FIsLocked := FALSE;
end;

function TNativeDataSet.GetTableName : string;
var IsOK: boolean;
    s: string;
begin
  Result := FTablename;
  if (Length(Result) = 0) and (FOMode <> dbiREADONLY) then
   begin
    s := Format('SELECT %u::regclass',[FieldTable(0)]);
    Result := FConnect.SelectStringDirect(PChar(s),IsOK,0);
    if IsOK then
     FTableName := Result;
   end;
end;

procedure TNativeDataSet.SetTableName(Name : string);
begin
  FTableName := Name;
end;

function TNativeDataSet.GetSQLClause: string;
begin
  Result := StandartClause.Text + RangeClause.Text + OrderClause.Text + LimitClause.Text
end;

function TNativeDataSet.GetIndexCount : Integer;
var
  i: Integer;
  ATableOID: cardinal;
  aPrim,aUniq,aSort : Boolean;
  Buffer : String;
  J : Integer;
  LastIdx: integer;
  sSQLQuery: string;
  ATablename, Aliace: String;
  Tbl, ASchema: string;
  RES: PPGresult;
  IdxName: string;
begin
  Result :=0;
  if not FIndexDescs.Updated then
  begin
    if isQuery and (FOMode = dbiReadOnly) or (FieldCount <= 0)
      then Exit; //multitable or non-Select SQL query

    if FConnect.GetserverVersionAsInt <= 070400 then
     begin
        if SQLQuery <> ''then
           ATableName := GetTable(SQLQuery, Aliace) else
           ATableName := TableName;
        if ATableName = '' then Exit;
        ATableName := StringReplace(ATableName,'"','',[rfReplaceAll]);
        sSqlQuery := 'SELECT t1.relname AS name, i.indisunique as "unique", i.indkey as fields, i.indisprimary'+
                     ' FROM "pg_index" as i, "pg_class" as t1, "pg_class" AS t2'+
                     ' WHERE i.indexrelid=t1.oid'+
                     ' AND i.indrelid=t2.oid'+
                     ' AND t2.relname = ''%s''';
                     //' AND i.indexprs IS NULL';
        I := Pos('.',ATableName);
        if I > 0 then
          begin
           ASchema := Copy(ATableName, 1, I-1);
           Tbl := Copy(ATableName, I+1, MaxInt);
           sSQLQuery := sSQLQuery + ' AND t2.relnamespace =' +
                                    ' (SELECT oid FROM pg_namespace WHERE nspname = ''%s'')';
           sSQLQuery := Format(sSQLQuery,[Tbl,ASchema]);
          end
        else
           sSQLQuery := Format(sSQLQuery,[ATableName]);
     end
    else //if we have version >= 7.4.1
      begin
       ATableOID := FieldTable(0);
       sSqlQuery := 'SELECT t1.relname AS name,'+
                    ' i.indisunique as "unique",'+
                    ' i.indkey as fields,'+
                    ' i.indisprimary'+
                    ' FROM "pg_index" as i, "pg_class" as t1, "pg_class" AS t2'+
                    ' WHERE i.indexrelid=t1.oid'+
                    ' AND i.indrelid=t2.oid'+
                    ' AND t2.oid = %u'+
                    ' AND i.indexprs IS NULL';
       sSQLQuery := Format(sSQLQuery,[ATableOID]);
      end;
   try
    Res := _PQExecute(FConnect, sSQLQuery);
    if Assigned(RES) then
     try
      FConnect.CheckResult;
      for i:=0 to PQntuples(RES)-1 do
       begin
        aUniq := PQgetvalue(Res,i,1) = 't';
        aPrim := PQgetvalue(Res,i,3) = 't';
        aSort := False;
        Buffer :=  FConnect.RawToString(PQgetvalue(Res,i,2));
        for J :=1 to Length(Buffer) do
           if Buffer[J] = ' ' then Buffer[J] := ',';
        IdxName := FConnect.RawToString(PQgetvalue(Res,i,0));
        LastIdx := FIndexDescs.SetIndex(IdxName, Buffer, aPrim, aUniq, aSort);
        if LastIdx > 0 then
          if aPrim and (FPrimaryKeyNumber = 0) then
             FPrimaryKeyNumber := LastIdx; //pg: 14.02.07
       end;
       FIndexDescs.Updated := True;
     finally
      PQclear(RES);
     end;
    except
     if FConnect.GetserverVersionAsInt >= 070401
      then raise;
      //in case if parser failed to get correct tablename
      //and ver <= 7.4.0 swallow exception
    end;
  end;
  Result := FIndexDescs.Count;
end;

procedure TNativeDataSet.OpenBlob(PRecord: Pointer;FieldNo: Word;eOpenMode: DBIOpenMode);
var
  Field : TPSQLField;
  Mode  : Integer;
begin
  if eOpenMode = dbiREADONLY then Mode := INV_READ else Mode := INV_WRITE;
  Field := Fields[FieldNo];
  CheckParam(Field.FieldType <> fldBLOB,DBIERR_NOTABLOB);
  if Field.NativeBLOBType = nbtOID then //make sure we have deal with lo_xxx
   if FieldBuffer(FieldNo-1) <> nil then
   begin
    FBlobHandle := StrToUInt(Self.Field(FieldNo-1));
    if FBlobHandle <> InvalidOID then
    begin
     FConnect.BeginBLOBTran;
     FLocalBHandle := lo_open(Fconnect.Handle, FBlobHandle, Mode);
     if FLocalBHandle >= 0 then
       FBlobOpen := True
     else
       FConnect.RollbackBLOBTran; //17.08.2009
    end;
   end;
end;

procedure TNativeDataSet.CloseBlob(FieldNo: Word);
var
  Field : TPSQLField;
begin
  Field := Fields[FieldNo];
  CheckParam(Field.FieldType <> fldBLOB, DBIERR_NOTABLOB);
  if FBlobOpen and (Field.NativeBLOBType = nbtOID) and (FLocalBHandle >= 0) then
   begin
    lo_close(FConnect.Handle, FLocalBHandle);
    FConnect.CommitBLOBTran;
    FBlobHandle := InvalidOid;
    FBlobOpen := False;
   end;
end;

procedure TNativeDataSet.FreeBlob(PRecord: Pointer; FieldNo: Word);
var
  Field : TPSQLField;
  Buff : Pointer;
begin
  Field := Fields[FieldNo];
  CheckParam(Field.FieldType <> fldBLOB, DBIERR_NOTABLOB);
  Field.Buffer := PRecord;
  if not Field.FieldNull then
   begin
    Buff := Field.FieldValue;
    if PAnsiChar(Buff)^ = #1 then
     begin
       PAnsiChar(Buff)^ := #0;
       Inc(PAnsichar(Buff));
       FreeAndNil(TBlobItem(Buff^).Blob);
     end;
   end;
  CloseBlob(FieldNo);
end;

procedure TNativeDataSet.GetBlobSize(PRecord : Pointer; FieldNo : Word; var iSize : Longint);
Var
  Field : TPSQLField;

    function BlobSize(columnNumber: Integer; buff :Pointer): LongInt;
    var
      N, L: LongInt;
      Buffer : PAnsiChar;
    const
      MAX_PART_SIZE = 1024;
    begin
      Result := 0;
      if Field.FieldSubType = fldstMemo then
      begin
         if FieldBuffer(ColumnNumber-1) <> nil then
         if FConnect.IsUnicodeUsed then
         {$IFDEF DELPHI_12}
            Result := Length(FConnect.RawToString(FieldBuffer(ColumnNumber-1))) * SizeOf(Char)
         {$ELSE}
            Result := Length(UTF8ToString(FieldBuffer(ColumnNumber-1)))
         {$ENDIF}
          else
            Result := FieldSize(ColumnNumber-1);
     end else
      begin
        if FBlobOpen then
        begin
         L := 0;
         Buffer := AllocMem(1);
         repeat
          ReallocMem(Buffer, L + MAX_PART_SIZE);
          N  := lo_read(FConnect.Handle, FLocalBHandle, Buffer+L, MAX_PART_SIZE);
          Inc(L, N);
         until N < MAX_PART_SIZE;
         FreeMem(Buffer, L);
         lo_lseek(FConnect.Handle, FLocalBHandle, 0, 0);
         Result := L;
        end;
      end;
    end;

    function ByteaSize(ColumnNumber: Integer):integer;
    var P: PAnsiChar;
        i, Len: integer;
    begin
      Result := 0;
      if FieldBuffer(ColumnNumber-1) = nil then Exit;
      P := FieldBuffer(ColumnNumber-1);
      Len := StrLen(P);
      Result := 0;
      case FConnect.NativeByteaFormat of
        nbfEscape:
          begin
            I := 0;
            while i <= Len - 1  do
             begin
              if P[i] = '\' then
               begin
                inc(i);
                if P[i] = '\' then
                   inc(i)
                  else
                   inc(i,3);
               end
              else
               inc(i);
              inc(Result);
             end;
          end;
        nbfHex: //for >9.0
          begin
            Result := (Len - 2) div 2; // '\x' preceding bytea value + 2 hexadecimal digits per byte
          end;
      end;
    end;

var
  Buff : Pointer;

begin
  Field := Fields[FieldNo];
  CheckParam(Field.FieldType <> fldBLOB,DBIERR_NOTABLOB);
  Field.Buffer := PRecord;
  if not Field.FieldNULL  then
   begin
    Buff := Field.FieldValue;
    if PAnsiChar(Buff)^ = #1 then
      begin
         Inc(PAnsichar(Buff));
         iSize := TBlobItem(Buff^).Blob.Size;
      end
    else
     if (Field.NativeBLOBType = nbtOID) or (Field.NativeType = FIELD_TYPE_TEXT) then
       iSize  := BlobSize(FieldNo, Field.FieldValue)
     else
       iSize  := ByteaSize(FieldNo);
   end //not FieldNULL
  else
   iSize  := 0
end;

procedure TNativeDataSet.GetBlob(PRecord : Pointer; FieldNo : Word; iOffSet : Longint; iLen : Longint; pDest : Pointer; var iRead : Longint);
var
  Field : TPSQLField;

    function CachedBlobGet(Offset, Length: longint; buff, Dest: pointer): longint;
    begin
     if PAnsiChar(buff)^=#1 then
      begin
        Inc(PAnsiChar(buff));
        with TBlobItem(buff^) do
        begin
           Blob.Seek(Offset, 0);
           Result:=Blob.Read(Dest^, Length)
        end;
      end
     else
      Result := 0;
    end;

    function BlobGet(ColumnNumber: Integer; Offset, ALength : LongInt; buff, Dest :Pointer)  : LongInt;
    var
      L,N : integer;
      Len : LongInt;
     {$IFNDEF DELPHI_12}
      S: string;
     {$ENDIF}
    begin
     Result := CachedBlobGet(Offset, ALength, buff, Dest);
     if Result = 0 then
      if Field.FieldSubType = fldstMemo then
        begin
           if FConnect.IsUnicodeUsed then
        {$IFDEF DELPHI_12}
            begin
              Utf8ToUnicode(Dest, ALength, PAnsiChar(FieldBuffer(ColumnNumber - 1) + Offset), Cardinal(-1));
              Len := StrLen(PChar(Dest)) *  SizeOf(Char);
            end
        {$ELSE}
            begin
              S := UTF8ToString(FieldBuffer(ColumnNumber - 1));
              Move(PAnsiChar(PAnsiChar(S) + Offset)^, Dest^, ALength);
              Len := Length(S);
            end
        {$ENDIF}
           else
            begin
              Move(PAnsiChar(FieldBuffer(ColumnNumber - 1) + Offset)^, Dest^, ALength);
              Len := StrLen(FieldBuffer(ColumnNumber - 1));
            end;


           if (Offset + ALength >= Len) then
            Result := Len - Offset
           else
            Result := ALength;
        end
      else
        begin
         if FBlobOpen then
         begin
          lo_lseek(FConnect.Handle, FLocalBHandle, Offset, 0);
          L := 0;
          Len := ALength;
          if ALength > MAX_BLOB_SIZE then
          begin
           repeat
            if Len > MAX_BLOB_SIZE then
               N  := lo_read(FConnect.Handle, FLocalBHandle, PAnsiChar(Dest) + L, MAX_BLOB_SIZE)
            else
               N  := lo_read(FConnect.Handle, FLocalBHandle, PAnsiChar(Dest) + L, Len);
            Dec(Len, MAX_BLOB_SIZE);
            Inc(L, N);
           until N < MAX_BLOB_SIZE;
           Result := L;
          end else
             Result  := lo_read(FConnect.Handle, FLocalBHandle, PAnsiChar(Dest), ALength);
         end;
        end;
       end;

   function ByteaBlobGet(ColumnNumber: Integer; Offset, Length : LongInt; buff, Dest :Pointer)  : LongInt;
   var P: PAnsiChar;
       Len: integer;
   begin
    Result := CachedBlobGet(Offset, Length, buff, Dest);

    if (Result = 0) and Assigned(PAnsiChar(FieldBuffer(ColumnNumber-1)+Offset)) then
     begin
      P := PQUnescapeBytea(FieldBuffer(ColumnNumber-1), Len);
     try
      Move((P+Offset)^, Dest^, Length);
      Result := Length;
     finally
      PQFreeMem(P);
     end;
     end;
   end;

begin
  iRead  := 0;
  if Assigned(pDest) and (iLen > 0) then
  begin
    Field := Fields[FieldNo];
    CheckParam(Field.FieldType <> fldBLOB, DBIERR_NOTABLOB);
    Field.Buffer := PRecord;
    if not Field.FieldNull then
      if (Field.NativeBLOBType = nbtOID) or (Field.NativeType = FIELD_TYPE_TEXT) then
        iRead := BlobGet(FieldNo, iOffset, iLen, PAnsiChar(Field.Data) + Field.FieldNumber - 1 ,pDest)
      else
        iRead := ByteaBLOBGet(FieldNo, iOffset, iLen, PAnsiChar(Field.Data) + Field.FieldNumber - 1 ,pDest)
  end;
end;

procedure TNativeDataSet.PutBlob(PRecord: Pointer;FieldNo: Word;iOffSet: Longint;iLen: Longint; pSrc : Pointer);
var
  Field : TPSQLField;

  procedure BlobPut(ColumnNumber: Integer; Offset, Length : LongInt; pSrc, buff :Pointer);
  begin
    if PAnsiChar(buff)^ = #0 then
      begin
        PAnsiChar(buff)^ := #1;
        Inc(PAnsiChar(buff));
        TBlobItem(buff^).Blob := TMemoryStream.Create;
      end
    else
      Inc(PAnsiChar(buff));
    with TBlobItem(buff^) do
    begin
      Blob.Seek(Offset, 0);
      if Length > 0 then
        Blob.Write(pSrc^, Length) else
        if Offset = 0 then Blob.Clear;
    end;
  end;

begin
  Field := Fields[FieldNo];
  CheckParam(Field.FieldType <> fldBLOB,DBIERR_NOTABLOB);
  Field.Buffer := PRecord;
  BlobPut(FieldNo, iOffset, iLen, pSrc, PAnsiChar(Field.Data) + Field.FieldNumber-1);
  Field.FieldChanged := True;
  Field.FieldNull := (iOffset + iLen = 0);
end;

procedure TNativeDataSet.TruncateBlob(PRecord : Pointer; FieldNo : Word; iLen : Longint);
begin
   PutBlob(PRecord, FieldNo, 0, iLen, nil);
end;

procedure TNativeDataSet.QuerySetParams(Params : TParams; SQLText : String);
var
  Token, Temp, Value: string;
  Param: TParam;
  i: integer;
  byName: boolean;
  MS: TMemoryStream;

  function GetDateTime: string;
  var ts: string;
  begin
     case Param.DataType of
      ftDate: ts := 'mm-dd-yyyy';
      ftDateTime: ts := 'mm-dd-yyyy hh:nn:ss.zzz';
      ftTime: ts := 'hh:nn:ss';
     end;
     if VarType(Param.Value) = VarDate then
       Result := AnsiQuotedStr(FormatDateTime(ts, Param.Value, PSQL_FS),'''')
     else
       Result := AnsiQuotedStr(VarAsType(Param.Value, varString),'''');
  end;

begin
  if Params.Count = 0 then Exit;
  Temp := '';
  i := 0;
  while SQLText <> '' do
  begin
    if (Temp <> '') and CharInSet(SQLText[1], [' ',#9]) then Temp := Temp + ' ';
    GetToken(SQLText, Token);
    //Added: handle of ? params
    if (Token = ':') or (Token = '?') then
    begin
      ByName := False;
      if Token = ':' then
       begin
         GetToken(SQLText, Token);
         if (length(Token) = 1) and CharInSet(Token[1], [':','=']) then //handling of double colon & assignment
          begin
           Temp := Temp + Token;
           Continue;
          end;
         ByName := True;
       end;
      if (Token <> '') and (Token[1] = '[') then
      begin
         if Token[Length(Token)] = ']' then
            Token := Copy(Token, 2, Length(Token)-2)
         else
            Token := Copy(Token, 2, Length(Token)-1);
      end else
      if (Token <> '') and CharInSet(Token[1], ['"','''']) then
      begin
         if Token[1] = Token[Length(Token)] then
            Token := Copy(Token, 2, Length(Token)-2)
         else
            Token := Copy(Token, 2, Length(Token)-1);
      end;
      // if Params is set with ":" then select param by name
      Param := nil;
      if ByName then
         Param := Params.FindParam(Token)
      else
       begin
         if i < Params.Count then Param := Params[i];
         Inc(i);
       end;
      if not Assigned(Param) or (VarType(Param.Value) = varEmpty) or (VarType(Param.Value) = varNull) then
        Value := 'NULL'
      else
        case Param.DataType of
          ftADT: Value := 'DEFAULT';
          ftBLOB: begin
                    MS := TMemoryStream.Create;
                    try
                     MS.SetSize(Param.GetDataSize);
                     Param.GetData(MS.Memory);
                     Value := BlobValue(MS, TPSQLParam(Param).DataTypeOID <> FIELD_TYPE_OID, True);
                    finally
                     MS.Free;
                    end;
                  end;
          ftDate, ftTime, ftDateTime: Value := GetDateTime;
        else
         case VarType(Param.Value) of
           {$IFNDEF DELPHI_5}
           varInt64,
           {$ENDIF}
           varSmallint,
           varInteger,
           varByte     : Value := IntToStr(Param.Value);
           varSingle,
           varDouble,
           varCurrency : Value := SQLFloatToStr(VarAsType(Param.Value, varDouble));
           varBoolean  : if Param.Value then Value := 'TRUE' else Value := 'FALSE';
         else
           {$IFDEF DELPHI_12}
           if FConnect.IsUnicodeUsed then
             Value := StrValue(PWideChar(Param.AsString))
           else
           {$ENDIF}
             Value := StrValue(PAnsiChar(AnsiString(Param.AsString)));
         end;
        end;
      Temp := Temp + Value;
    end else
      Temp := Temp + Token;
  end;
  SQLQuery := Trim(Temp);
end;

procedure TNativeDataSet.RelRecordLock(bAll: Boolean);
begin
  FIsLocked := FALSE;
end;

procedure TNativeDataSet.ExtractKey(PRecord: Pointer;pKeyBuf: Pointer);
var
  i : Word;
  MKey    : PAnsiChar;
  Field   : TPSQLField;
  bBlank  : Boolean;
  Buffer  : array [0..MAX_CHAR_LEN] of Char;
  iFields : Word;
begin
  if not Assigned(PRecord) then PRecord := CurrentBuffer;
  ZeroMemory(pKeyBuf, FKeyDesc.iKeyLen);
  MKey := pKeyBuf;
  iFields := FKeyDesc.iFldsinKey;
  for i := 0 to iFields-1 do
    begin
      Field := Fields[FKeyDesc.aiKeyFld[i]];
      NativeToDelphi(Field, PRecord, @Buffer, bBlank);
      if not bBlank then
        AdjustDelphiField(Field, @Buffer, MKey);
      if bBlank then
        ZeroMemory(MKey, Field.NativeSize);
     Inc(MKey, Field.NativeSize + 1);
    end;
end;


procedure TNativeDataSet.GetIndexDesc(iIndexSeqNo: Word; var idxDesc: IDXDesc);
begin
  CheckParam(not(IndexCount > 0) ,DBIERR_NOASSOCINDEX);
  ZeroMemory(@idxDesc, Sizeof(idxDesc));
  if (iIndexSeqNo = 0) and not FGetKeyDesc then
     if KeyNumber <> 0 then iIndexSeqNo := KeyNumber;
  if iIndexSeqNo = 0 then iIndexSeqNo := 1;
  CheckParam(FIndexDescs.mIndex[iIndexSeqNo] = nil,DBIERR_NOSUCHINDEX);
  idxDesc := FIndexDescs.mIndex[iIndexSeqNo].Description;
end;

procedure TNativeDataSet.GetIndexDescs(Descs: TIDXDescList);
var
  Props : CURProps;
  i     : Word;
begin
  GetCursorProps(Props);
  if Props.iIndexes > 0 then
  begin
    FGetKeyDesc := TRUE;
    try
      for i := 1 to Props.iIndexes do
        GetIndexDesc(i, Descs[i-1]);
    finally
      FGetKeyDesc := FALSE;
    end;
  end;
end;

procedure TNativeDataSet.SwitchToIndex( pszIndexName, pszTagName : string; iIndexId : Word; bCurrRec : Boolean);

procedure ParseIndexName(pszIndexName: string; Var iIndexId : Word; pszTrueName  : string);
var
  //S     : ShortString;
  Found : Boolean;
  Desc  : IDXDesc;
begin
  Found := False;
  FGetKeyDesc := TRUE;
  try
     iIndexId := 1;
     Repeat
       GetIndexDesc ( iIndexId, Desc );
       if Desc.szName = pszIndexName then
       begin
         Found := TRUE;
         break;
       end;
       Inc(iIndexId);
     Until Found;
     if Found and ( iIndexId > 0 )  and ( pszTrueName <> '' ) then
       pszTrueName := Desc.szName;
  finally
    FGetKeyDesc := False;
  end;
end;

begin
  FIsLocked := FALSE;
  //CheckParam(pszIndexName = '', DBIERR_INVALIDPARAM);
  if FFieldDescs.Count = 0 then InitFieldDescs;
  if Length(pszIndexName) > 0 then
    ParseIndexName(pszIndexName, iIndexId, '')
  else
    if FPrimaryKeyNumber >= 1 then iIndexId := FPrimaryKeyNumber;
  try
    if Ranges then ResetRange;
    KeyNumber := iIndexId;
  finally
    AutoReExec := True;
  end;
  GetIndexDesc(iIndexId, FKeyDesc);
end;

procedure TNativeDataSet.ResetRange;
begin
  RangeClause.Clear;
  if Ranges then ReOpenTable;
  Ranges := False;
end;

procedure TNativeDataSet.SetRange(bKeyItself: Boolean;
               iFields1: Word;iLen1: Word;pKey1: Pointer;bKey1Incl: Boolean;
               iFields2: Word;iLen2: Word;pKey2: Pointer;bKey2Incl: Boolean);

procedure CreateRangeClause(First : Boolean; bKeyItself: Boolean;iFields: Word;iLen: Word; pKey: Pointer; bKeyIncl: Boolean);
var
  i         : integer;
  Field     : TPSQLField;
  WHERE     : String;
  FldVal    : String;
  bBlank    : Boolean;
  Buff : array[0..MAX_CHAR_LEN] of AnsiChar;
  CurBuffer : PAnsiChar;
  TimeStamp: TTimeStamp;
begin
    For i := 0 to iFields-1 do
     if Fields[FKeyDesc.aiKeyFld[i]].FieldNull then
      begin
       RangeClause.Text := 'WHERE 1=0';
       Exit; //null values have no details by standard
      end;

    WHERE := '';
    CurBuffer := PAnsiChar(pKey);
    for i := 0 to iFields-1 do
    begin
      Field := Fields[FKeyDesc.aiKeyFld[i]];
      if bKeyItself then
        AdjustNativeField(Field, CurBuffer, @Buff, bBlank)
      else
        NativeToDelphi(Field, CurBuffer, @Buff, bBlank);
      Inc(CurBuffer, Field.NativeSize + 1);
      if bBlank then Continue; //19.05.2008
      if RangeClause.Count > 0  then WHERE := 'and ' else WHERE := 'where ';
      WHERE := WHERE + AnsiQuotedStr(Field.FieldName,'"');
      if First then WHERE := WHERE + '>' else WHERE := WHERE + '<';
      if bKeyIncl then WHERE := WHERE + '=';
      case Field.Fieldtype of
        fldINT16: FldVal := IntToStr(PSmallInt(@Buff)^);
        fldINT32: FldVal := IntToStr(PLongInt(@Buff)^);
        fldFLOAT: FldVal := SQLFloatToStr(PDouble(@Buff)^);
        fldBOOL:  if PBoolean(@Buff)^ then FldVal := 'True' else FldVal := 'False';
        fldZSTRING: FldVal := StrValue(@Buff);
        fldUUID:  FldVal := UuidValue(@Buff);
        fldINT64: FldVal := IntToStr(PInt64(@Buff)^);
        fldDate:
                  begin
                    TimeStamp.Date := PLongInt(@Buff)^;
                    TimeStamp.Time := 0;
                    FldVal := '''' + DateTimeToSqlDate(TimeStampToDateTime(TimeStamp),1) + '''';
                  end;
        fldTime:  begin
                    TimeStamp.Date := DateDelta;
                    TimeStamp.Time := PLongInt(@Buff)^;
                    FldVal := '''' + DateTimeToSqlDate(TimeStampToDateTime(TimeStamp),2) + '''';
                  end;
        fldTIMESTAMP: FldVal := '''' + DateTimeToSqlDate(TimeStampToDateTime(MSecsToTimeStamp(PDouble(@Buff)^)),0) + '''';
      end;
      WHERE := WHERE + Trim(FldVal);
      RangeClause.Add(WHERE);
    end;
end;

begin
  try
    RangeClause.Clear;
    Ranges := True;
    CreateRangeClause(True,bKeyItself, iFields1, iLen1, pKey1, bKey1Incl);
    CreateRangeClause(False,bKeyItself, iFields2, iLen2, pKey2, bKey2Incl);
    ReOpenTable;
  except
    ResetRange;
  end;
end;

procedure TNativeDataSet.SetKeyNumber( newValue : SmallInt );
var
  x,y : Integer;
  Ind : TPSQLIndex;

function GetOrderByStr(Idx : TPSQLIndex; index : integer) : String;
var
   B : Boolean;
begin
   result := '';
   B := idx.Descending;
   Result := AnsiQuotedStr(string(FieldInfo[idx.FDesc.aiKeyFld[index]-1].FieldName), '"');
   if B then
      Result := Result +' DESC';
end;


begin
  if newValue <> FKeyNumber then
  begin
    OrderClause.Clear;
    if  newValue <= IndexCount then
    begin
      OrderClause.Add('ORDER BY ');
      Ind := FIndexDescs.mIndex[newValue];
      y := ind.FDesc.iFldsInKey-1;
      for x := 0 to y-1 do
        OrderClause.Add(GetOrderByStr(Ind, x) + ',');
      OrderClause.Add(GetOrderByStr(Ind, y));
    end;
    FKeyNumber := newValue;
    ReOpenTable;
  end;
end;

procedure TNativeDataSet.ReOpenTable;
var OldRowsAffected: integer;
begin
  OldRowsAffected := FAffectedRows;
  OpenTable;
  FAffectedRows := OldRowsAffected;
end;

procedure TNativeDataSet.ClearIndexInfo;
begin
  if FIndexDescs.Count > 0  then
    begin
     FIndexDescs.Clear;
     FIndexDescs.Updated := False;
    end;
  FKeyNumber        := 0;
  FPrimaryKeyNumber := 0;
end;

procedure TNativeDataSet.SettoSeqNo(iSeqNo: Longint);
begin
    if iSeqNo-1 < 0 then
       RecNo := -1 else
       if iSeqNo-1 >= RecordCount-1 then
          RecNo := RecordCount-1 else
          RecNo := iSeqNo-1;
    CurrentRecord(RecNo);
end;

procedure TNativeDataSet.EmptyTable;
var
  S : String;
  Result : PPGResult;
begin
  S := Format('TRUNCATE TABLE %s',[TableName]);
  FAffectedRows := 0;
  if not Assigned(FConnect) or not (FConnect.FLoggin) then  Exit;
  Result := _PQexecute(FConnect, S);
  if Result <> nil then
  begin
    FConnect.CheckResult;
    PQClear(Result);
  end else
    FConnect.CheckResult;
end;

procedure TNativeDataSet.AddIndex(var IdxDesc: IDXDesc; pszKeyviolName : string);
var
  Result : PPGResult;

 function CreateSQLForAddIndex: String;
 var
   Fld : String;
   PSQLIdxs : TPSQLIndexes;
 begin
   Result := '';
   PSQLIdxs := TPSQLIndexes.Create(nil);
   TPSQLIndex.CreateIndex(PSQLIdxs,@IdxDesc);
   Fld := SQLCreateIdxStr(PSQLIdxs[1],TableName,Fields);
   Result := Result+Fld;
   PSQLIdxs.Free;
 end;

begin
  if not Assigned(FConnect) or not (FConnect.FLoggin) then  Exit;
  Result := _PQexecute(FConnect, CreateSQLForAddIndex);
  if Result <> nil  then
  begin
    PQclear(Result);
  end else
    FConnect.CheckResult;
end;

procedure TNativeDataSet.DeleteIndex(pszIndexName: string; pszIndexTagName: string; iIndexId: Word);
var
   Result : PPGResult;
begin
  if not Assigned(FConnect) or not (FConnect.FLoggin) then  Exit;
    Result := _PQexecute(FConnect, Format('DROP INDEX %s ON %s',[pszIndexName, TableName]));
  if Result <> nil  then
  begin
    PQclear(Result);
  end else
    FConnect.CheckResult;
end;

procedure TNativeDataSet.AcqTableLock(eLockType: word; bNoWait: boolean);
const _lockmode: array[TPSQLLockType] of AnsiString = ('ACCESS SHARE', 'ROW SHARE',
        'ROW EXCLUSIVE', 'SHARE UPDATE EXCLUSIVE',
        'SHARE', 'SHARE ROW EXCLUSIVE', 'EXCLUSIVE',
        'ACCESS EXCLUSIVE');
      _nowait: array[boolean] of AnsiString = ('', 'NOWAIT');
var Res: PPGresult;
begin
  Res := _PQExecute(FConnect, Format('LOCK TABLE %s IN %s MODE %s', [TableName, _lockmode[TPSQLLockType(eLockType)], _nowait[bNoWait]]));
  try
    FConnect.CheckResult(Res);
  finally
    PQclear(RES);
end;
end;

procedure TNativeDataSet.SetToKey(eSearchCond: DBISearchCond; bDirectKey: Boolean;iFields: Word;iLen: Word;pBuff: Pointer);
var
  FldNo : Integer;
  Field : TPSQLField;
  Item  : TPSQLIndex;
  R : LongInt;
  I : Integer;
  Flds  : array of integer;
  SFlds : array of String;

begin
   Item := FIndexDescs.mIndex[iFields];
   SetLength(Flds,Item.Description.iFldsInKey);
   SetLength(SFlds,Item.Description.iFldsInKey);
   for I :=0 to Item.Description.iFldsInKey-1 do
   begin
      FldNo := Item.Description.aiKeyFld[I];
      Field := Fields[FldNo];
      Flds[i] := FldNo-1;
      SFlds[I] := FieldVal(Field.FieldNumber,Field.FieldValue);
   end;
   R := findrows(Flds,SFlds,False,ilen);
   if (R <> -1) then
      SetToSeqNo(R+1) else
      SetToSeqNo(RecordCount);
end;


procedure TNativeDataSet.Clone(bReadOnly : Boolean; bUniDirectional : Boolean; var hCurNew : hDBICur);
begin
  if FConnect = nil then raise EPSQLException.CreateBDE(DBIERR_INVALIDHNDL);
  TNativeConnect(FConnect).OpenTable(TableName, FIndexName, 0, FOMode, dbiOPENSHARED, hCurNew, [], 0, 0);
  TNativeDataSet(hCurNew).MasterCursor := Self;
end;

procedure TNativeDataSet.SetToCursor(hDest : hDBICur);
var
  M : Pointer;
begin
  if hDest = nil then raise EPSQLException.CreateBDE(DBIERR_INVALIDHNDL);
  M := AllocMem(BookMarkSize);
  try
    if MasterCursor = nil then
    begin
       GetBookMark(M);
       TNativeDataSet(hDest).SetToBookMark(M); // set main cursor bookmark
    end;
  Finally
    FreeMem(M, BookMarkSize);
  end;
end;



//////////////////////////////////////////////////////////////////////
//          TIndexList Object                                       //
//////////////////////////////////////////////////////////////////////
Constructor TIndexList.Create(PSQL: TNativeConnect; D : TIDXDescList; TotalCount : Word );
begin
  Inherited Create(PSQL, nil, [], '', '', 0, 0, 0);
  Items   := TotalCount;
  if D <> nil then
  begin
    SetLength(Descs, Items);
    Descs := Copy(D, Low(D), Length(D));
  end;
  SetToBegin;
end;

procedure TIndexList.SetToBegin;
begin
  inherited SetToBegin;
  Position := 0;
end;

Destructor TIndexList.Destroy;
begin
  Finalize(Descs);
  Inherited Destroy;
end;

procedure TIndexList.GetNextRecord(eLock: DBILockType; PRecord  : Pointer;pRecProps : pRECProps);
begin
  if Position = Items then
    raise EPSQLException.CreateBDE(DBIERR_EOF)
  else
    GetIdxDesc(PRecord);
  Inc(Position);
end;

procedure TIndexList.GetIdxDesc(Precord: PIdxDesc);
begin
 PRecord^ := TIDXDescList(Descs)[Position];
end;


function TIndexList.GetBufferSize : Word;
begin
  Result := SizeOf(idxDESC);
end;

function TIndexList.GetWorkBufferSize : Word;
begin
  Result := GetBufferSize;
end;

procedure TIndexList.SetToBookmark(P : Pointer);
begin
   SetToBegin;
end;

procedure TIndexList.GetRecordCount( Var iRecCount : Longint );
begin
   iRecCount := Items;
end;

constructor TFieldList.Create(PSQL: TNativeConnect; D: TFLDDescList;
  TotalCount: Word);
begin
  Inherited Create(PSQL, nil, [], '', '', 0, 0, 0);
  Items   := TotalCount;
  if D <> nil then
  begin
    SetLength(Descs, Items);
    Descs := Copy(D, Low(D), Length(D));
  end;
  SetToBegin;

end;

destructor TFieldList.Destroy;
begin
  Finalize(Descs);
  inherited Destroy;
end;

function TFieldList.GetBufferSize : Word;
begin
  Result := SizeOf(FLDDesc);
end;

{******************************************************************************}
{                           TPSQLEngine                                        }
{******************************************************************************}
Constructor TPSQLEngine.Create(P : TObject; Container : TContainer);
begin
  Inherited Create(P, Container);
  FDatabase := hDBIDb(Self);
end;

Destructor TPSQLEngine.Destroy;
begin
  FDatabase := nil;
  Inherited Destroy;
end;

function TPSQLEngine.GetDatabase : hDBIDb;
begin
  Result := FDatabase;
end;

procedure TPSQLEngine.SetDatabase( H : hDBIDb );
begin
  if H = nil then  Raise EPSQLException.CreateBDE(DBIERR_INVALIDHNDL);
  FDatabase := H;
end;

function TPSQLEngine.IsSqlBased(hDb : hDBIDB) : Boolean;
begin
  Result   := True;
end;

function TPSQLEngine.OpenDatabase(Params : TStrings; UseSinleLineConnInfo: boolean; var hDb : hDBIDb): DBIResult;
Var
  DB : TNativeConnect;
begin
  try
    Db := TNativeConnect.Create;
    if Db = nil then
      raise EPSQLException.CreateBDE(DBIERR_INVALIDHNDL);
    try
      if UseSinleLineConnInfo then
        begin
         DB.ProcessDBParams(Params);
         Db.InternalConnect;
        end
      else
        DB.InternalConnect(Params);
    except
      on E: EPSQLException do
      begin
         DB.Free;
         raise;
      end;
    end;
    hDb := hDBIDb(DB);
    Database := hDb;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CloseDatabase(var hDb : hDBIDb) : DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).Free;
    hDb := nil;
    FDatabase := nil;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenTable(hDb: hDBIDb; pszTableName: string; pszDriverType: string; pszIndexName: string; pszIndexTagName : string; iIndexId: Word;
         eOpenMode: DBIOpenMode;eShareMode: DBIShareMode;exltMode: XLTMode;bUniDirectional : Boolean;pOptParams: Pointer;var hCursor: hDBICur;
         AnOptions: TPSQLDatasetOptions; Limit, Offset : Integer): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).OpenTable(pszTableName, pszIndexName, iIndexId, eOpenMode, eShareMode, hCursor, AnOptions, Limit, Offset);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenStoredProcList(hDb: hDBIDb;pszWild: string; List : TStrings): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).StoredProcList(pszWild, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenTableList(hDb: hDBIDb;pszWild: string; SystemTables: Boolean; List : TStrings): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).TableList(pszWild,SystemTables, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenSchemaList(hDb: hDBIDb; pszWild: string; SystemSchemas: Boolean; List : TStrings): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).SchemaList(pszWild, SystemSchemas, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenUserList(hDb: hDBIDb; pszWild: string; List : TStrings): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).UserList(pszWild, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetNextRecord(hCursor: hDBICur;eLock: DBILockType;pRecBuff : Pointer;pRecProps: pRECProps): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetNextRecord(eLock, pRecBuff, pRecProps);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetToBookMark(hCur: hDBICur;pBookMark: Pointer) : DBIResult;
begin
  try
    TNativeDataSet(hCur).SetToBookMark(pBookMark);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CompareBookMarks(hCur : hDBICur;pBookMark1,pBookMark2 : Pointer;Var CmpBkmkResult : CmpBkmkRslt): DBIResult;
begin
  try
    TNativeDataSet(hCur).CompareBookMarks(pBookMark1, pBookMark2, CmpBkmkResult);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetRecord (hCursor: hDBICur;eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetRecord(eLock,PRecord,pRecProps);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetPriorRecord(hCursor: hDBICur;eLock: DBILockType;PRecord: Pointer;pRecProps: pRECProps): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetPriorRecord(eLock,PRecord,pRecProps);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
    if Result = DBIERR_EOF then Result := DBIERR_BOF;
  end;
end;

function TPSQLEngine.GetBookMark(hCur: hDBICur;pBookMark : Pointer) : DBIResult;
begin
  try
    TNativeDataSet(hCur).GetBookMark(pBookMark);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ReadBlock(hCursor : hDBICur; var iRecords : Longint; pBuf : Pointer): DBIResult;
begin
  try
    TNativeDataset(hCursor).ReadBlock(iRecords, pBuf);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetRecordCount(hCursor : hDBICur;Var iRecCount : Longint) : DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetRecordCount(iRecCount);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ForceReread(hCursor: hDBICur): DBIResult;
begin
  try
    TNativeDataSet(hCursor).ForceReread;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetField(hCursor: hDBICur;FieldNo: Word;PRecord: Pointer;pDest: Pointer;var bBlank: Boolean): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetField(FieldNo, PRecord, PDest, bBlank);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CloseCursor(hCursor : hDBICur) : DBIResult;
begin
  try
    TNativeDataSet(hCursor).CloseTable;
    TNativeDataSet(hCursor).Free;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.PutField(hCursor: hDBICur;FieldNo: Word;PRecord: Pointer;pSrc: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).PutField(FieldNo,PRecord,PSrc);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;eOpenMode: DBIOpenMode): DBIResult;
begin
  try
    TNativeDataSet(hCursor).OpenBlob(PRecord, FieldNo, eOpenMode);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetBlobSize(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;var iSize: Longint): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetBlobSize(PRecord, FieldNo, iSize);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;iOffSet: Longint;iLen: Longint;pDest: Pointer;var iRead: Longint): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetBlob(PRecord, FieldNo, iOffset, iLen, pDest, iRead);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.Ping(Params: TStrings;
  var PingResult: TPingStatus): DBIResult;
begin
  try
    PingResult := TNativeConnect.Ping(Params);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.PutBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;iOffSet: Longint;iLen: Longint;pSrc: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).PutBlob(PRecord, FieldNo, iOffset, iLen, pSrc);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.TruncateBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word;iLen: Longint): DBIResult;
begin
  try
    TNativeDataSet(hCursor).TruncateBlob( PRecord, FieldNo, iLen );
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.FreeBlob(hCursor: hDBICur;PRecord: Pointer;FieldNo: Word): DBIResult;
begin
  try
    TNativeDataSet(hCursor).FreeBlob(PRecord, FieldNo);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CloseBlob(hCursor: hDBICur; FieldNo: Word): DBIResult;
begin
  try
    TNativeDataSet(hCursor).CloseBlob(FieldNo);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.BeginTran(hDb: hDBIDb; eXIL: eXILType; var hXact: hDBIXact): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).BeginTran(eXIL, hXact);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.EndTran(hDb: hDBIDb;hXact : hDBIXact; eEnd : eXEnd): DBIResult;
begin
  try
   Database := hDb;
   TNativeConnect(hDb).EndTran(hXact,eEnd);
   Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetTranInfo(hDb : hDBIDb; hXact : hDBIXact; pxInfo : pXInfo): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).GetTranInfo(hXact,pxInfo);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;


function TPSQLEngine.GetTranStatus(hDb: hDBIDb; var TranStatus: TTransactionStatusType): DBIResult;
begin
  try
    Database := hDb;
    TranStatus := TNativeConnect(hDb).GetTransactionStatus;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetEngProp(hObj: hDBIObj;iProp: Longint;PropValue: Pointer;iMaxLen: Word;var iLen: Word): DBIResult;
begin
  iLen := 0;
  if Assigned( hObj ) then
  begin
    TNativeDataSet(hObj).GetProp( iProp, PropValue, iMaxLen, iLen );
    Result := DBIERR_NONE;
  end else
    Result := DBIERR_INVALIDPARAM;
end;

function TPSQLEngine.SetEngProp(hObj: hDBIObj;iProp: Longint;PropValue: Longint): DBIResult;
begin
  try
    if Assigned(hObj) then
    begin
      TNativeDataSet(hObj).SetProp(iProp, PropValue);
      Result := DBIERR_NONE;
    end else
      Result := DBIERR_INVALIDPARAM;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetVchkDesc(hCursor: hDBICur;iValSeqNo: Word; var pvalDesc: VCHKDesc): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetVchkDesc(iValSeqNo, pvalDesc);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetCursorProps(hCursor: hDBICur;var curProps: CURProps): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetCursorProps(curProps);
    Result := DBIERR_NONE;
  except
     Result := CheckError;
  end;
end;

function TPSQLEngine.GetFieldDescs(hCursor: hDBICur; var pfldDesc :  TFLDDescList): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetFieldDescs(pFldDesc);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetToBegin(hCursor : hDBICur) : DBIResult;
begin
  TNativeDataSet(hCursor).SetToBegin;
  Result := DBIERR_NONE;
end;

function TPSQLEngine.SetToEnd(hCursor : hDBICur) : DBIResult;
begin
  TNativeDataSet(hCursor).SetToEnd;
  Result := DBIERR_NONE;
end;

function TPSQLEngine.RelRecordLock(hCursor: hDBICur;bAll: Boolean): DBIResult;
begin
  try
    TNativeDataSet(hCursor).RelRecordLock(bAll);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.InitRecord(hCursor: hDBICur;PRecord: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).InitRecord(PRecord);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.InsertRecord(hCursor: hDBICur;eLock: DBILockType;PRecord: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).InsertRecord(eLock, PRecord);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.AppendRecord(hCursor : hDBICur;PRecord : Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).AppendRecord(PRecord);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ModifyRecord(hCursor: hDBICur;OldRecord,PRecord: Pointer;bFreeLock : Boolean; ARecNo : LongInt): DBIResult;
begin
  try
    TNativeDataSet(hCursor).ModifyRecord(OldRecord,PRecord, bFreeLock,ARecNo);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.DeleteRecord(hCursor: hDBICur;PRecord: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).DeleteRecord(PRecord);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetToSeqNo(hCursor: hDBICur;iSeqNo: Longint): DBIResult;
begin
  try
    TNativeDataSet(hCursor).SettoSeqNo(iSeqNo);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetObjFromObj( Source : hDBIObj; eObjType : DBIOBJType; var hObj : hDBIObj ) : DBIResult;
begin
  if ( eObjType = objSESSION ) then
  begin
    Result := DBIERR_NONE;
  end
  else
  begin
    hObj   := nil;
    Result := DBIERR_INVALIDPARAM;
  end;
end;

function TPSQLEngine.AddFilter(hCursor: hDBICur;iClientData: Longint;iPriority: Word;bCanAbort: Boolean;pcanExpr: pCANExpr;
                                pfFilter: pfGENFilter;var hFilter: hDBIFilter): DBIResult;
begin
  try
    TNativeDataSet(hCursor).AddFilter(iClientData,iPriority, bCanAbort,pcanExpr, pfFilter, hFilter );
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.DropFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
begin
  try
    TNativeDataSet(hCursor).DropFilter(hFilter);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ActivateFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
begin
  try
    TNativeDataSet(hCursor).ActivateFilter(hFilter);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.DeactivateFilter(hCursor: hDBICur;hFilter: hDBIFilter): DBIResult;
begin
  try
    TNativeDataSet(hCursor).DeactivateFilter(hFilter);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetErrorEntry(uEntry: Word;var ulNativeError: Longint;pszError: PChar): DBIResult;
Var
  tmp        : String;

  procedure AddMessage( P : pChar );
  begin
    if ( StrLen( P ) > 0 ) then
      if ( Tmp <> '' ) then
        Tmp := Tmp + #13#10 + StrPas( P ) else
        Tmp := StrPas( P );
  end;

begin
  ulNativeError := -100;
  tmp := 'Error';
  StrLCopy(pszError, pChar(tmp), SizeOf(DBIPATH)- 1);
  Result := 0;
end;

function TPSQLEngine.GetErrorString(rslt: DBIResult;ErrorMsg: String): DBIResult;
begin
  ErrorMsg := MessageStatus;
  Result := rslt;
end;

function TPSQLEngine.QExecDirect(hDb : hDBIDb; pszQuery: String;phCur : phDBICur; var AffectedRows : LongInt): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).QExecDirect(pszQuery,phCur, AffectedRows);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QAlloc(hDb: hDBIDb; eQryLang: DBIQryLang;var hStmt: hDBIStmt): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).QueryAlloc(hStmt);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QPrepare(hStmt: hDBIStmt;pszQuery: String): DBIResult;
begin
  try
    TNativeConnect(Database).QueryPrepare(hStmt,pszQuery);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QExec(hStmt: hDBIStmt; phCur : phDBICur; var AffectedRows: integer): DBIResult;
begin
   try
    if phCur = nil then
    begin
      try
        TNativeDataSet(hStmt).Execute;
        AffectedRows := TNativeDataSet(hStmt).FAffectedRows;
        Result := DBIERR_NONE;
      except
        Result := CheckError;
      end
    end
    else
    begin
      TNativeDataSet(hStmt).OpenTable;
      if TNativeDataSet(hStmt).FStatement <> nil then
        phCur^ := hDBICur(hStmt)
      else
        phCur^ := nil;
      Result := DBIERR_NONE;
    end;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QPrepareExt (                             { Prepare a query }
           hDb           : hDBIDb;                          { Database handle }
           eQryLang      : DBIQryLang;                      { Query language }
           pszQuery      : PChar;                           { Query }
           propBits      : Word;                            { properties for Prepare, e.g. qprepFORUPDATE }
           var hStmt     : hDBIStmt                         { Returned statment handle }
         ): DBIResult;
begin
  try
    Database := hDb;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QFree(var hStmt : hDBIStmt): DBIResult;
begin
  Result := CloseCursor(hDBICur(hStmt));
  hStmt := nil;
end;

function TPSQLEngine.QuerySetParams(hStmt: hDBIStmt;Params : TParams; SQLText : String): DBIResult;
begin
  try
    TNativeDataSet(hStmt).QuerySetParams(Params,SQLText);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CheckError : DBIResult;
begin
  if ExceptObject is EPSQLException then
  begin
    if EPSQLException(ExceptObject).BDEErrors then
      Result := EPSQLException(ExceptObject).BDEErrorCode
    else
      begin
       FNativeStatus := EPSQLException(ExceptObject).PSQLErrorCode;
       Result := 1001;
      end;
       FNativeMsg := EPSQLException(ExceptObject).PSQLErrorMsg;
       FNativeErrorPos:= EPSQLException(ExceptObject).FPSQLErrorPos;
       FNativeErrorContext:= EPSQLException(ExceptObject).FPSQLErrorContext;
       FNativeErrorseverity:= EPSQLException(ExceptObject).FPSQLErrorseverity;
       FNativeErrorsqlstate:= EPSQLException(ExceptObject).FPSQLErrorsqlstate;
       FNativeErrorprimary:=  EPSQLException(ExceptObject).FPSQLErrorprimary;
       FNativeErrordetail:=  EPSQLException(ExceptObject).FPSQLErrordetail;
       FNativeErrorhint:=    EPSQLException(ExceptObject).FPSQLErrorhint;
       FNativeErrorinternalpos:= EPSQLException(ExceptObject).FPSQLErrorinternalpos;
       FNativeErrorinternalquery:=EPSQLException(ExceptObject).FPSQLErrorinternalquery;
       FNativeErrorsourcefile:= EPSQLException(ExceptObject).FPSQLErrorsourcefile;
       FNativeErrorsourceline:= EPSQLException(ExceptObject).FPSQLErrorsourceline;
       FNativeErrorsourcefunc:= EPSQLException(ExceptObject).FPSQLErrorsourcefunc;
  end
  else
     Raise ExceptObject;
end;

function TPSQLEngine.GetDatabases(hDb: hDBIdb; pszWild: string; List : TStrings):DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hdb).DatabaseList(pszWild,List);
    Result := DBIERR_NONE;
   except
    Result := CheckError;
   end;
end;

function TPSQLEngine.GetCharacterSet(hDb : hDBIDb; var CharSet : string):DBIResult;
begin
   try
     Database := hDb;
     CharSet := TNativeConnect(hDb).GetCharSet;
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

///////////////////////////////////////////////////////////////////////////////
//                  Reserver for TPSQLTable                                 //
///////////////////////////////////////////////////////////////////////////////
function TPSQLEngine.OpenFieldList(hDb: hDBIDb; pszTableName: string; pszDriverType: string; bPhyTypes: Boolean; var hCur: hDBICur): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).OpenFieldList(pszTableName, pszDriverType, bPhyTypes, hCur );
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.OpenIndexList(hDb: hDBIDb;pszTableName: string; pszDriverType: string; var hCur: hDBICur): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).OpenIndexList(pszTableName, pszDriverType, hCur);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.EmptyTable(hDb: hDBIDb; hCursor : hDBICur; pszTableName : string; pszDriverType : string): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).EmptyTable(hCursor, pszTableName);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetRange(hCursor: hDBICur;bKeyItself: Boolean;iFields1: Word;iLen1: Word;pKey1: Pointer;bKey1Incl: Boolean;
                               iFields2: Word;iLen2: Word;pKey2: Pointer;bKey2Incl: Boolean): DBIResult;
begin
  try
    TNativeDataSet(hCursor).SetRange(bKeyItself, iFields1, iLen1, pKey1, bKey1Incl,iFields2, iLen2, pKey2, bKey2Incl);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ResetRange(hCursor: hDBICur): DBIResult;
begin
  try
    TNativeDataSet(hCursor).ResetRange;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SwitchToIndex(hCursor: hDBICur; pszIndexName, pszTagName: string; iIndexId: Word; bCurrRec: Boolean): DBIResult;
begin
  try
    TNativeDataSet(hCursor).SwitchToIndex(pszIndexName, pszTagName, iIndexId, bCurrRec);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ExtractKey(hCursor: hDBICur;PRecord: Pointer;pKeyBuf: Pointer): DBIResult;
begin
  try
    TNativeDataSet(hCursor).ExtractKey(PRecord, pKeyBuf);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetRecordForKey(hCursor: hDBICur; bDirectKey: Boolean; iFields: Word; iLen: Word; pKey: Pointer; pRecBuff: Pointer; AStrictConformity: boolean = False): DBIResult;
begin
   try
    TNativeDataSet(hCursor).GetRecordForKey(bDirectKey,iFields,iLen, pKey, pRecBuff, AStrictConformity);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.AddIndex(hDb: hDBIDb;hCursor: hDBICur;pszTableName: string;pszDriverType: string;var IdxDesc: IDXDesc;pszKeyviolName : string): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDB).AddIndex(hCursor, pszTableName, pszDriverType, idxDesc, pszKeyViolName);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.DeleteIndex(hDb: hDBIDb;hCursor: hDBICur;pszTableName: string;pszDriverType: string;pszIndexName: string;pszIndexTagName: string;iIndexId: Word): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDB).DeleteIndex(hCursor, pszTableName, pszDriverType, pszIndexName, pszIndexTagName, iIndexId);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetIndexDesc(hCursor: hDBICur;iIndexSeqNo: Word;var idxDesc: IDXDesc): DBIResult;
begin
  try
    Result := DBIERR_NONE;
    if TNativeDataSet(hCursor).isQuery then
      Result := DBIERR_NOASSOCINDEX
    else
      TNativeDataSet(hCursor).GetIndexDesc(iIndexSeqNo,idxDesc);
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetIndexDescs(hCursor: hDBICur; idxDescs: TIDXDescList): DBIResult;
begin
  try
    TNativeDataSet(hCursor).GetIndexDescs(idxDescs);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.TranslateRecordStructure(pszSrcDriverType : PChar;iFlds: Word;pfldsSrc: pFLDDesc;pszDstDriverType: PChar; pszLangDriver: PChar;pfldsDst: pFLDDesc; bCreatable: Boolean): DBIResult;
var
  M : pFldDesc;
  I : Integer;
begin
  try
    M  := pfldsDst;
    For i := 1 to iFlds do
    begin
       Move(pfldsSrc^, M^, SizeOf(FldDesc));
       Inc(M);
       Inc(pfldsSrc);
    end;
    Result :=DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.TableExists(hDb: hDBIDb; pszTableName: string): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).TableExists(pszTableName);
     Result := DBIERR_NONE;
  except
     Result := CheckError;
  end;
end;

function TPSQLEngine.CreateTable(hDb: hDBIDb; bOverWrite: Boolean; var crTblDsc: CRTblDesc): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).CreateTable(bOverwrite, crTblDsc);
     Result := DBIERR_NONE;
  except
     Result := CheckError;
  end;
end;

function TPSQLEngine.AcqTableLock(hCursor: hDBICur; eLockType: word; bNoWait: boolean): DBIResult;
begin
  try
    TNativeDataset(hCursor).AcqTableLock(eLockType, bNoWait);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetToKey(hCursor: hDBICur;eSearchCond: DBISearchCond;bDirectKey: Boolean;iFields: Word;iLen: Word;pBuff: Pointer): DBIResult;
begin
  try
    TNativeDataset(hCursor).SetToKey(eSearchCond, bDirectKey, iFields, iLen, pBuff);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TNativeDataSet.SetRowPosition(iFields : Integer; LID : Int64; pRecBuffer : Pointer):Boolean;
var
  FldNo : Integer;
  Field : TPSQLField;
  Item : TPSQLIndex;
  R   : Longint;
  I   : Integer;
  Flds  : array of Integer;
  SFlds : array of String;
  K     : Integer;

var
  SS : String;

begin
   if isQuery then iFields := -1;
   if iFields = -1 then
   begin
      K := 1;
      for I := 0 to Fields.Count-1 do
      begin
         Field := Fields[I+1];
         Field.Buffer := pRecBuffer;
         if (Field.FieldType = fldBLOB) or
            (Field.Description.bCalcField) or
            (Field.FieldNull and (Field.FieldSubType <> fldstAUTOINC)) or
            (Field.NativeType = FIELD_TYPE_TIMESTAMP) then Continue;
         SetLength(Flds,K);
         SetLength(SFlds,K);
         Flds[K-1] := I;
         if (Field.FieldSubType = fldstAUTOINC) and (LID > 0) then
            SFlds[K-1] := IntToStr(LID) else
            SFlds[K-1] := FieldVal(I+1, Field.FieldValue);
         Inc(K);
      end;
   end else
   begin
      Item := FIndexDescs.mIndex[iFields];
      SetLength(Flds,Item.Description.iFldsInKey);
      SetLength(SFlds,Item.Description.iFldsInKey);
      for I := 0 to Item.Description.iFldsInKey-1 do
      begin
         FldNo := Item.Description.aiKeyFld[I];
         Field := Fields[FldNo];
         Flds[I] := FldNo-1;
         Field.Buffer := pRecBuffer;
         SS := FieldVal(FldNo, Field.FieldValue);
         if SS = '' then
         begin
            if (Field.FieldSubType = fldstAUTOINC) and (LID > 0) then
            SS := IntToStr(LID);
         end;
         SFlds[I] := SS;
      end;
   end;
   R := findrows(Flds,SFlds,False,0);
   Result := R <> -1;
   if Result then
      SettoSeqNo(R+1);
end;


function TPSQLEngine.CloneCursor(hCurSrc: hDBICur;bReadOnly: Boolean;bUniDirectional: Boolean;var hCurNew: hDBICur): DBIResult;
begin
  try
    TNativeDataset(hCurSrc).Clone(bReadonly, bUniDirectional, hCurNew);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SetToCursor(hDest, hSrc : hDBICur) : DBIResult;
begin
  try
    TNativeDataset(hSrc).SetToCursor(hDest);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

{PSQLNotify}
function TPSQLEngine.OpenPGNotify(hDb: hDBIDb; var hNotify: hDBIObj): DBIResult;
Var
  ANotify : TNativePGNotify;
begin
  try
    hNotify := nil;
    Database := hDB;
    ANotify := TNativePGNotify.Create(TNativeConnect(Database));
    if ANotify = nil then Raise EPSQLException.CreateBDE(DBIERR_INVALIDHNDL);
    hNotify := hDBIObj(ANotify);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ClosePGNotify(var hNotify : hDBIObj) : DBIResult;
begin
 try
    TNativePGNotify(hNotify).Free;
    hNotify := nil;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.ListenTo(hNotify : hDBIObj; pszEvent: string) : DBIResult;
begin
   try
     TNativePGNotify(hNotify).ListenTo(pszEvent);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.UnlistenTo(hNotify : hDBIObj; pszEvent: string) : DBIResult;
begin
   try
     TNativePGNotify(hNotify).UnlistenTo(pszEvent);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.DoNotifyEx(hNotify: hDBIObj; pszChannel: string; pszPayload: string): DBIResult;
begin
   try
     TNativePGNotify(hNotify).DoNotifyEx(pszChannel, pszPayload);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.DoNotify(hNotify : hDBIObj; pszEvent: string) : DBIResult;
begin
   try
     TNativePGNotify(hNotify).DoNotify(pszEvent);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.CheckEvents(hNotify : hDBIObj; var Pid : Integer;  var pszOutPut, pszPayload : String)  : DBIResult;
begin
   try
     pszOutPut := TNativePGNotify(hNotify).CheckEvents(Pid, pszPayload);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.GetBackendPID(hDb: hDBIDb; var PID: Integer): DBIResult;
begin
   try
    Database := hDb;
    PID := TNativeConnect(hDB).BackendPID;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;


{****************************************************************************}
{                        TNativePGNOTIFY                                     }
{****************************************************************************}
constructor TNativePGNotify.Create(AConnect: TNativeConnect);
begin
  FConnect := AConnect;
  FHandle:=nil;
end;

destructor TNativePGNotify.Destroy;
begin
  inherited Destroy;
end;

procedure TNativePGNotify.InternalExecute(Sql: string);
var
   locResult : PPGResult;
begin
  LocResult := _PQexecute(FConnect, SQL);
  if Assigned(LocResult) then
     PQclear(LocResult);
end;

procedure TNativePGNotify.ListenTo(Event: string);
begin
  Event := Trim(Event);
  if Event <> '' then
     InternalExecute('LISTEN ' + Event);
end;

procedure TNativePGNotify.UnlistenTo(Event: string);
begin
  Event := Trim(Event);
  if Event <> '' then
    InternalExecute('UNLISTEN ' + Event);
end;

procedure TNativePGNotify.DoNotifyEx(Channel, Payload: string);
begin
  Channel := Trim(Channel);
  if Channel <> '' then
    InternalExecute(Format('NOTIFY %s, %s', [Channel, QuotedStr(Payload)]));
end;

procedure TNativePGNotify.DoNotify(Event: string);
begin
  Event := Trim(Event);
  if Event <> '' then
    InternalExecute('NOTIFY ' + Event);
end;

function TNativePGNotify.CheckEvents(var PID : Integer; var Payload: string): string;
begin
  Result := '';
  if not Assigned(FConnect) or not (FConnect.FLoggin) then Exit;
  PQconsumeInput(FConnect.Handle);
  FHandle := PQnotifies(FConnect.Handle);
  if Assigned(FHandle) then
  begin
    Result := FConnect.RawToString(FHandle^.relname);
    Payload := FConnect.RawToString(FHandle^.extra);
    PID := FHandle^.be_pid;
    PQfreemem(FHandle);
  end;
end;

function TPSQLEngine.GetServerVersion(hDb: hDBIDb;
  var ServerVersion: string): DBIResult;
begin
  try
    Database := hDb;
    ServerVersion := TNativeConnect(hDb).GetServerVersion;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetUserProps(hDb: hDBIDb; const UserName: string;
                var SuperUser, CanCreateDB,
                  CanUpdateSysCatalogs: boolean; var UserID: integer;
                var ValidUntil: string):DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).GetUserProps(UserName, SuperUser, CanCreateDB,
                             CanUpdateSysCatalogs, UserID, ValidUntil);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetDBProps(hDB: hDBIDB; const DB: string;
                        var Owner, Tablespace: string;
                        var IsTemplate: boolean;
                        var DBOid: cardinal; var Comment: string):DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).GetDBProps(DB, Owner, Tablespace,
                        IsTemplate, DBOid, Comment);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;


function TNativeConnect.GetServerVersion: string;
begin
  if FServerVersion > '' then
   begin
    Result := FServerVersion;
    Exit;
   end;

  InternalConnect;

  Result := RawToString(PQparameterStatus(Handle, 'server_version'));
  FServerVersion := Result;
end;

function TNativeDataSet.FieldVal(FieldNo: Integer; FieldPtr : Pointer):String;
var
  Field : TPSQLField;
  Blank : Boolean;
  Buff  : array[0..MAX_BLOB_SIZE] of Char;
  TimeStamp : TTimeStamp;
  DateD : Double;
begin
  Result :='';
  Field := Fields[FieldNo];
  AdjustNativeField(Field,FieldPtr,@Buff,Blank);
  if not Blank then
  case Field.FieldType of
    fldINT16: Result := IntToStr(PSmallInt(@Buff)^);
    fldUINT16: Result := IntToStr(PWord(@Buff)^);
    fldINT32: Result := IntToStr(PLongInt(@Buff)^);
    fldINT64: Result := IntToStr(PInt64(@Buff)^);
    fldFLOAT: Result := SysUtils.FloatToStr(PDouble(@Buff)^);
    fldZSTRING:
              {$IFDEF DELPHI_12}
              if FConnect.IsUnicodeUsed then
                Result := PWideChar(@Buff)
              else
              {$ENDIF}
                Result := String(PAnsiChar(@Buff));
    fldDATE : begin
                 LongWord(TimeStamp.Date) := PLongWord(@Buff)^;
                 TimeStamp.Time := 0;
                 Result := FormatDateTime('mm-dd-yyyy',SysUtils.Time+Trunc(TimeStampToDateTime(TimeStamp) + 1E-11), PSQL_FS);
              end;
    fldTIME : begin
                 LongWord(TimeStamp.Time) := PLongWord(@Buff)^;
                 TimeStamp.Date := DateDelta;
                 Result := FormatDateTime('hh:nn:ss',SysUtils.Date+TimeOf(TimeStampToDateTime(TimeStamp)), PSQL_FS);
              end;
    fldTIMESTAMP :
              begin
                 DateD := PDouble(@Buff)^;
                 Result := FormatDateTime('mm-dd-yyyy hh:nn:ss',TimeStampToDateTime(MSecsToTimeStamp(DateD)), PSQL_FS);
              end;
   else
      Result := string(StrPas(PAnsiChar(@Buff)));
   end;
end;


procedure TNativeDataSet.GetRecordForKey(bDirectKey: Boolean; iFields,
  iLen: Word; pKey, pRecBuff: Pointer;  AStrictConformity: boolean = False);


    procedure SetToLookupKey;
    var
      FieldPtr : Pointer;
      FldNo : Integer;
      Len : Integer;
      R   : Longint;
      I   : Integer;
      Field : TPSQLField;
      S : String;
      Flds  : array of Integer;
      SFlds : array of String;
    begin
      S := '';
      Len := 0;
      if bDirectKey then
      begin
         SetLength(Flds,FKeyDesc.iFldsinKey);
         SetLength(SFlds,FKeyDesc.iFldsinKey);
         for I := 0 to FKeyDesc.iFldsinKey-1 do
         begin
              FldNo := FKeyDesc.aiKeyFld[I];
              Field := Fields[FKeyDesc.aiKeyFld[I]];
              Flds[I] := FldNo-1;
              FieldPtr := pKey;
              Inc(PAnsiChar(FieldPtr),Len + i);
              SFlds[I] := FieldVal(FldNo, FieldPtr);
              Inc(Len, Field.FieldLength);
        end;
      end else
      begin
             SetLength(Flds,iFields);
             SetLength(SFlds,iFields);
             for I := 0 to iFields-1 do
             begin
                FldNo := FKeyDesc.aiKeyFld[I];
                Field := Fields[FKeyDesc.aiKeyFld[I]];
                Flds[I] := FldNo-1;
                Field.Buffer := pKey;
                SFlds[I] := FieldVal(FldNo, Field.FieldValue);
             end;
      end;
      R := findrows(Flds,SFlds,False,iLen,AStrictConformity);
      CheckParam(R=-1 ,DBIERR_RECNOTFOUND);
      SettoSeqNo(R+1);
    end;

    procedure SetToMasterKey;
    var
      FieldPtr : Pointer;
      FldNo : Integer;
      Len : Integer;
      R   : Longint;
      I   : Integer;
      Field : TPSQLField;
      S : String;
      Flds  : array of Integer;
      SFlds : array of String;
    begin
       S := '';
       Len := 0;
       SetLength(Flds,TNativeDataSet(MasterCursor).FKeyDesc.iFldsInKey);
       SetLength(SFlds,TNativeDataSet(MasterCursor).FKeyDesc.iFldsInKey);
       for I := 0 to  TNativeDataSet(MasterCursor).FKeyDesc.iFldsInKey-1 do
       begin
          FldNo := TNativeDataSet(MasterCursor).FKeyDesc.aiKeyFld[I];
          Field := TNativeDataSet(MasterCursor).Fields[FldNo];
          Flds[I] := FldNo-1;
          if bDirectKey then
          begin
             FieldPtr := pKey;
             Inc(PAnsiChar(FieldPtr),Len + i);
             SFlds[I] := S+FieldVal(FldNo, FieldPtr);
             Inc(Len, Field.FieldLength);
          end else
          begin
             Field.Buffer := pKey;
             SFlds[i] := FieldVal(FldNo, Field.FieldValue);
          end;
       end;
       R := TNativeDataSet(MasterCursor).findrows(Flds,SFlds,False,iLen,AStrictConformity);
       CheckParam(R=-1 ,DBIERR_RECNOTFOUND);
       TNativeDataSet(MasterCursor).SettoSeqNo(R+1);
    end;

begin
   SetToLookupKey;
   if MasterCursor <> nil then
      SetToMasterKey;
end;


function TNativeDataSet.findrows(const Fields: array of Integer;
  const SearchFields: array of String; ACaseSen: Boolean;
  APartLen: Integer; AStrictConformity: boolean = False): int64;
var
  I, K   : Integer;
  Cmp : Integer;
  IsSorted: boolean; //05.05.2008

    function Compare1(const S1: String; const S2 : String; FldType : integer):Integer;

        function CompWithLen(const S1,S2 : PChar):Integer;
        var
          I : Integer;
          P1,P2 : PChar;
        begin
          Result := 0;
          P1 := S1;
          P2 := S2;
          if (StrLen(P1) < StrLen(P2)) then
            Result := -1 else

            if (StrLen(P1) > StrLen(P2)) then
              Result := 1 else

              begin
                for I :=0 to Min(StrLen(P1),StrLen(P2)) do
                  begin
                    if P1^ > P2^ then
                      begin
                        Result := 1;
                        Break;
                      end else

                      if P1^ < P2^ then
                        begin
                          Result := -1;
                          Break;
                        end;
                      Inc(P1); Inc(P2);
                  end;
              end;
        end;

        function CompWithoutLen(const S1,S2 : PChar):Integer;
        var
          I : Integer;
          P1,P2 : PChar;
          Len : Integer;
        begin
          Result := 0;
          P1 := S1;
          P2 := S2;
          if (StrLen(P1) < StrLen(P2)) then
             Result := -1 else
             if (StrLen(P1) > StrLen(P2)) then
                Result := 1;
          Len := Min(StrLen(P1),StrLen(P2));
          for I :=0 to Len-1 do
            begin
              if P1^ > P2^ then
              begin
                 Result := 1;
                 Break;
              end else
              if P1^ < P2^ then
              begin
                 Result := -1;
                 Break;
              end else
              if P1^ = P2^ then
              begin
                 if MaskSearch(string(S2),string(S1)+'%') then
                 begin
                    Result := 0;
                    Break;
                 end;
              end;
              Inc(P1); Inc(P2);
            end;
        end;

        function SqlDateToBDEDateTime(const Value: string): string;
        var
          Year, Month, Day: String;
          Temp: string;
        begin
          Temp   := Value;
          Result := '';
          if Length(Temp) >= 10 then
          begin
            Year  := Copy(Temp,1,4);
            Month := Copy(Temp,6,2);
            Day   := Copy(Temp,9,2);
            Result := Format('%s-%s-%s',[Month,Day,Year]);
            Temp := Copy(Temp,12,8);
          end;
          if Length(Temp) >= 8 then
            Result := Result + ' ' + Temp;
        end;

    var BoolChar: char;

    begin
        case FldType of
          FIELD_TYPE_INT2,
          FIELD_TYPE_INT4,
          FIELD_TYPE_INT8,
          FIELD_TYPE_OIDVECTOR,
          FIELD_TYPE_OID: Result := CompWithLen(PChar(S1), PChar(S2));

          FIELD_TYPE_FLOAT4,
          FIELD_TYPE_FLOAT8,
          FIELD_TYPE_NUMERIC: if AStrictConformity then
                                  Result := CompWithLen(PChar(StringReplace(S1, PSQL_FS.DecimalSeparator,
                                                                '.', [rfReplaceAll])),
                                            PChar(S2))
                              else
                                  Result := CompWithoutLen(
                                            PChar(StringReplace(S1, PSQL_FS.DecimalSeparator,
                                                                '.', [rfReplaceAll])),
                                            PChar(S2));

          FIELD_TYPE_DATE,
          FIELD_TYPE_TIMESTAMP,
          FIELD_TYPE_TIMESTAMPTZ: if AStrictConformity then
                                    Result := CompWithLen(PChar(S1), PChar(SqlDateToBDEDateTime(S2)))
                                  else
                                    Result := CompWithoutLen(PChar(S1), PChar(SqlDateToBDEDateTime(S2)));

          FIELD_TYPE_BOOL: begin
                            if S1 = '' then
                             BoolChar := 'F'
                            else
                             BoolChar := 'T';
                            Result := ord(boolchar) - ord(UpCase(S2[1]));
                           end

          else
                            if AStrictConformity then
                              Result := CompWithLen(PChar(S1), PChar(S2))
                             else
                              Result := CompWithoutLen(PChar(S1), PChar(S2))
        end
    end;

    function FldVal(CurRow: integer; aIndex: Integer): String;
    begin
      if IsSorted then CurRow := FSortingIndex[CurRow]; //05.05.2008
      if (aIndex > -1) and (aIndex <= FieldCount-1) then //are we in range?
        Result := FConnect.RawToString(PQGetValue(Fstatement,CurRow,aIndex))
      else
        Result := '';
    end;

Var
  P1,P2 : String;

Begin
 try
  Cmp := -1;
  IsSorted := IsSortedLocally;
  for I := 0 to GetRecCount-1 do
    begin
      Cmp := 0;
      for K := 0 to High(Fields) do
        begin
          if ACaseSen then
            begin
              P1 := AnsiUpperCase(SearchFields[K]);
              P2 := AnsiUpperCase(FldVal(I, Fields[K]));
            end else
            begin
              P1 := SearchFields[K];
              P2 := FldVal(I, Fields[K]);
            end;
            Cmp := Cmp+Compare1(P1, P2, FieldType(Fields[K]));
            if Cmp <> 0 then Break;
        end;
      if Cmp = 0 then Break;
    end;
  if Cmp = 0 then
    Result := I else
    Result := -1;
  except
    Result := -1;
  end;
end;

function TNativeConnect.IsSSLUsed: boolean;
var P: pointer;
begin
  Result := False;
  if not FLoggin then Exit;
  P := PQgetssl(FHandle);
  Result := Assigned(P);
end;

function TNativeConnect.IsTransactionActive: boolean;
begin
 Result := FTransState = xsActive;
end;


procedure TNativeConnect.BeginBLOBTran;
var
  Result: PPGresult;
  TransParam: AnsiString;
begin
  if (FTransState <> xsActive)
     AND (GetTransactionStatus = trstIDLE)
   then
  begin
    FBlobTransactionInProgress := True;
    Result := PQexec(Handle, 'BEGIN /*BLOB handling*/');
    PQclear(Result);
    TransParam := 'SET TRANSACTION ISOLATION LEVEL ';
    case FTransLevel of
      xilDIRTYREAD,
      xilREADCOMMITTED : TransParam := TransParam + 'READ COMMITTED';
      xilREPEATABLEREAD: TransParam := TransParam + 'SERIALIZABLE';
    end;
    Result := PQexec(Handle, PAnsiChar(TransParam));
    PQclear(Result);
  end
end;

procedure TNativeConnect.RollbackBLOBTran;
var
  Result: PPGresult;
begin
  if FBlobTransactionInProgress AND
     (GetTransactionStatus <> trstIDLE) then
  begin
    FBlobTransactionInProgress := False;
    Result := PQexec(Handle, 'ROLLBACK /*BLOB handling*/');
    PQclear(Result);
  end
end;

procedure TNativeConnect.CommitBLOBTran;
var
  Result: PPGresult;
begin
  if FBlobTransactionInProgress AND
    (GetTransactionStatus <> trstIDLE) then
  begin
    FBlobTransactionInProgress := False;
    Result := PQexec(Handle, 'COMMIT /*BLOB handling*/');
    PQclear(Result);
  end
end;

procedure TNativeConnect.StoredProcList(pszWild: string; List: TStrings);
var
   CRec : string;
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  List.Clear;
   Sql := 'SELECT p.oid, p.oid::regproc' +
          ' FROM	pg_proc p';
   if pszWild <> '' then
    Sql := Sql + ' WHERE p.proname LIKE ' + QuotedStr(pszWild);
  Sql := Sql + ' ORDER BY 2';
  RES := _PQexecute(Self, Sql);
  if Assigned(RES) then
  try
    begin
     for I := 0 to PQntuples(RES)-1 do
     begin
        CREC := RawToString(PQgetvalue(RES,I,1));
        List.AddObject(CREC,TOBject(strtoint(RawToString(PQGetValue(Res,I,0)))));
     end;
    end;
  finally
   PQclear(RES);
  end;
end;

function TPSQLEngine.OpenStoredProcParams(hDb: hDBIDb; pszPName: string;
  ProcOID: cardinal; List: TList): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).StoredProcParams(pszPName, ProcOID, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

procedure TNativeConnect.StoredProcParams(pszPName: string; ProcOID: cardinal;
  List: TList);
var
   PDesc: ^SPParamDesc;
   N: string;
   I : LongInt;
   ProcSchema, ProcName: string;
   ArgNum: cardinal;
   Sql : String;
   MinOIDSel: string;
   RES : PPGresult;
   BdeType,BdeSubType: word;
   LogSize: integer;
   LocArray: boolean;

const
  sqlShowParameters     = 'SELECT NULLIF(proargnames[g.s],''''),   '+ //proargnames[g.s] > 8.0.0
                          '       proargtypes[g.s],   '+
                          '       ''i''::varchar     '+
                          ' FROM                      '+
                          '     			pg_proc p,      '+
                          '           pg_type t JOIN pg_namespace nsp ON t.typnamespace = nsp.oid ,   '+
                          '     			generate_series(1,%d) as g(s)  '+ //generate_series(...) > 8.0.0
                          ' WHERE                     '+
                          '   p.proargtypes[g.s] = t.oid AND '+
                          '   p.oid = %d';

  sqlShowParameters810  = 'SELECT NULLIF(proargnames[g.s],''''),   '+
                          '       COALESCE(proargtypes[g.s-1], proallargtypes[g.s]),'+
                          '       COALESCE(proargmodes[g.s], ''i'') '+
                          ' FROM                      '+
                          '     			pg_proc p,      '+
                          '           pg_type t JOIN pg_namespace nsp ON t.typnamespace = nsp.oid ,   '+
                          '     			generate_series(1,%d) as g(s)'+
                          ' WHERE                     '+
                          '   COALESCE(p.proargtypes[g.s-1],proallargtypes[g.s]) = t.oid AND '+
                          '   p.oid = %d '+
                          ' ORDER BY g.s ';
begin
  InternalConnect;
  List.Clear;
  ArgNum := 0;
  if GetserverVersionAsInt > 080100 then
    MinOIDSel :=  'SELECT GREATEST(pronargs, array_upper(proallargtypes,1)), '
  else
    MinOIDSel :=  'SELECT pronargs, ';
  if ProcOID = 0 then
   begin
    I := Pos('.',pszPName);
    if I > 0 then
     begin
      ProcSchema := Copy(pszPName,1,I-1);
      ProcSchema := StringReplace(ProcSchema, '"', '', [rfReplaceAll]);
      ProcName := Copy(pszPName,I+1,MaxInt);
     end
    else
      begin
       ProcName := pszPName;
       ProcSchema := '%';
      end;
    ProcName := StringReplace(ProcName, '"', '', [rfReplaceAll]);

    MinOIDSel :=  MinOIDSel + 'pg_proc.oid '+
                  'FROM pg_catalog.pg_proc, pg_catalog.pg_namespace '+
                  Format(' WHERE proname LIKE ''%s'''+
                         ' AND nspname LIKE ''%s''',
                         [ProcName,ProcSchema]) +
                  ' ORDER BY 2 '+
                  ' LIMIT 1';
   end
  else
    MinOIDSel :=  MinOIDSel +
                  ' FROM pg_catalog.pg_proc '+
                  Format(' WHERE oid = %d', [ProcOID]);



  RES := _PQexecute(Self, MinOIDSel);
  if (PQresultStatus(RES) = PGRES_TUPLES_OK) and (PQntuples(RES) > 0) then
   begin
    ArgNum := StrToInt(RawToString(PQgetvalue(RES,0,0)));
    if ProcOID = 0 then
      ProcOID := StrToInt(RawToString(PQgetvalue(RES,0,1)));
   end;
  PQclear(Res);

  if ProcOID * ArgNum = 0 then Exit;


  if GetserverVersionAsInt >= 080100 then
    Sql := Format(sqlShowParameters810,[ArgNum,ProcOID])
  else
    Sql := Format(sqlShowParameters,[ArgNum,ProcOID]);

  RES := _PQExecute(Self, Sql);
  if PQresultStatus(RES) = PGRES_TUPLES_OK then
  begin
     for I := 0 to PQntuples(RES)-1 do
      begin
          New(PDesc);
          ZeroMemory(PDesc,SizeOf(PDesc^));

          if (PQgetisnull(RES,I,0) = 1) then
            N := 'arg' + IntToStr(I)
          else
            N := RawToString(PQgetvalue(RES,I,0));
          PDesc^.szName := N;
          PDesc^.uParamNum := I;
          FieldMapping(StrToInt(RawToString(PQgetvalue(RES,I,1))), 0, BdeType, BdeSubType, LogSize, LocArray);
          PDesc^.uFldType := BdeType;
          PDesc^.uSubType := BdeSubType;
          N := RawToString(PQgetvalue(RES,I,2));
          case N[1] of
           'o': PDesc^.eParamType := paramOUT;
           'b': PDesc^.eParamType := paramINOUT;
          else
           PDesc^.eParamType := paramIN;
          end;
          List.Add(PDesc)
      end;
  end;
  PQclear(RES);
end;

function TNativeConnect.StringToRaw(S: string): PAnsiChar;
var _S: AnsiString;
begin
  _S := StringToRawS(S);
  {$IFDEF DELPHI_12}
  Result := AnsiStrAlloc(Length(S) + 1);
  {$ELSE}
  Result := StrAlloc(Length(S) + 1);
  {$ENDIF}
  StrPCopy(Result, _S);
end;

function TNativeConnect.StringToRawS(S: string): AnsiString;
begin
 if IsUnicodeUsed then
  Result := UTF8Encode(S)
 else
  Result := AnsiString(S);
end;

function TPSQLEngine.QPrepareProc(hDb: hDBIDb; pszProc: PChar;
  hParams: pointer; var hStmt: hDBIStmt): DBIResult;
var SQLText,ParStr: string;
    i: integer;
    aParams: TPSQLParams;
begin
  try
    aParams := TPSQLParams(hParams);
    QAlloc(hDb,qryLangSQL,hStmt);
    SQLText := 'SELECT * FROM '+pszProc+'(%s)';
    if (aParams.Count > 0) and (aParams[0].ParamType in [ptInput,ptInputOutput]) then
      ParStr := ':' + AnsiQuotedStr(aParams[0].Name, '"')
    else
      ParStr := '';
    for i := 1 to aParams.Count - 1 do
      if aParams[i].ParamType in [ptInput,ptInputOutput] then
        ParStr := ParStr + ', :' + AnsiQuotedStr(aParams[i].Name, '"');
    TNativeDataSet(hStmt).SQLQuery := Format(SQLText,[ParStr]);
    TNativeDataSet(hStmt).isQuery := True; // PaGo 24.07.2007
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.QSetProcParams(hStmt: hDBIStmt; Params: TParams): DBIResult;
begin
  try
    TNativeDataSet(hStmt).StoredProcSetParams(Params);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

procedure TNativeDataSet.StoredProcSetParams(Params: TParams);
begin
  QuerySetParams(Params,SQLQuery);
end;


procedure TNativeConnect.SetCharSet(var ACharSet: string);
begin
  if (ACharSet = '') or not FLoggin then
   begin
    ACharSet := GetCharSet();
    Exit;
   end;
  if PQsetClientEncoding(Handle, PAnsiChar(AnsiString(ACharSet))) = 0 then
   ACharset := UpperCase(ACharset)
  else
   ACharSet :=  GetCharSet();
  {$IFDEF M_DEBUG}
  LogDebugMessage('INFO', 'Encoding changed to ' + ACharset);
  {$ENDIF}
end;

procedure TNativeConnect.GetCharSetList(var List: TStrings);
var
   sql, s : String;
   RES : PPGresult;
   i: integer;
   CREC: string;
begin
  InternalConnect;
  List.Clear;
  if Self.GetserverVersionAsInt >= 080000 then
   S := Format('generate_series(0,%d)',[MAX_ENCODING_ID])
  else
   S := Format(sqlGenerateSeries,[0,MAX_ENCODING_ID]);
  Sql := 'SELECT pg_encoding_to_char(num.n) FROM '+S+' as num(n)';
  RES := PQexec(Handle, PAnsiChar(AnsiString(Sql)));
  if Assigned(RES) then
   try
    CheckResult;
    List.BeginUpdate;
     try
      for i:=0 to PQntuples(RES)-1 do
       begin
          CREC := Trim(RawToString(PQgetvalue(RES,i,0)));
          if CREC > '' then List.Append(CREC);
       end;
     finally
      List.EndUpdate;
     end;
   finally
    PQclear(RES);
   end;
end;

function TPSQLEngine.SetCharacterSet(hDb: hDBIDb; var CharSet: string): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).SetCharSet(CharSet);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.SetErrorVerbosity(hDb : hDBIDb; const ErrorVerbosity: TErrorVerbosity): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).SetErrorVerbosity(ErrorVerbosity);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.SetCommandTimeout(hDb : hDBIDb; var Timeout : cardinal):DBIResult;
begin
   try
     Database := hDb;
     Timeout := TNativeConnect(hDb).SetTimeout(Timeout);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TPSQLEngine.GetCharacterSets(hDb: hDBIDb;
  List: TStrings): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).GetCharSetList(List);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

function TNativeDataSet.CheckUniqueKey(var KeyNumber : integer): Boolean;
var
  I: Integer;
  Item : TPSQLIndex;
begin
  Result := False;
  for I := 1 to FindexDescs.Count do
  begin
    Item := FIndexDescs.mIndex[I];
    if Item.Primary or Item.Unique then
    begin
        Result := True;
        KeyNumber := I;
        Break;
    end;
  end;
end;

function TNativeDataSet.GetLastInsertID(const KeyNumber: integer): integer;
var
  S: string;
  i: integer;
    RES: PPGresult;
begin
  Result := -1;
  S := FFieldDescs.Field[KeyNumber+1].FieldDefault;
  i := Pos('nextval(',lowercase(S));
  if i>0 then
   S := StringReplace(S, 'next', 'curr', [rfReplaceAll])
  else
   Exit;
  S := 'SELECT ' + S;
  Res := PQExec(FConnect.Handle,PAnsiChar(AnsiString(S)));
  if Assigned(RES) then
   try
    FConnect.CheckResult;
    if PQntuples(RES)>0 then
      Result := StrToIntDef(FConnect.RawToString(PQgetvalue(RES,0,0)),-1);
   finally
    PQclear(RES);
   end;
end;

function TNativeConnect.GetTransactionStatus: TTransactionStatusType;
begin
 Result := PQTransactionStatus(FHandle);
end;

procedure TNativeConnect.GetUserProps(const UserName: string;
                             var SuperUser, CanCreateDB,
                               CanUpdateSysCatalogs: boolean; var UserID: integer;
                             var ValidUntil: string);
var
   sql : String;
   RES : PPGresult;
begin
  SuperUser := False;
  CanCreateDB := False;
  CanUpdateSysCatalogs := False;
  UserID := -1;
  ValidUntil := '';
  InternalConnect;
  Sql := 'SELECT usesysid, usecreatedb, usesuper, usecatupd, valuntil '+
         ' FROM pg_user WHERE usename = '''+UserName+'''';
 try
  RES := _PQExecute(Self, Sql);
  if Assigned(RES) then
   try
    CheckResult;
    if PQntuples(RES) > 0 then
     begin
      UserID := strtoint(string(PQgetvalue(RES,0,0)));
      CanCreateDB := PQgetvalue(RES,0,1) = 't';
      SuperUser := PQgetvalue(RES,0,2) = 't';
      CanUpdateSysCatalogs := PQgetvalue(RES,0,3) = 't';
      ValidUntil := string(PQgetvalue(RES,0,4));
     end;
   except
    PQclear(RES);
   end;
  except
  end;
end;


procedure TNativeConnect.GetDBProps(const DB: string; var Owner,
  Tablespace: string; var IsTemplate: boolean; var DBOid: cardinal;
  var Comment: string);
var
   sql : String;
   RES : PPGresult;
   SV: integer;
begin
  DBOid := 0;
  IsTemplate := False;
  Tablespace := '';
  Owner := '';
  SV := GetServerVersionAsInt;

  if SV >= 080200 then
   Sql := Format(' LEFT JOIN %s ON (db.oid = %s.objoid), ', ['pg_shdescription', 'pg_shdescription'])
  else
   Sql := Format(' LEFT JOIN %s ON (db.oid = %s.objoid), ', ['pg_description', 'pg_description']);

  Sql := 'SELECT db.oid, datistemplate, usename, %s '+
         ' COALESCE(description,'''')::varchar '+
         ' FROM pg_database as db '+
         Sql +
         ' %s '+
         ' pg_user as sh '+
         ' WHERE '+
         ' %s '+
         ' sh.usesysid = datdba AND '+
         ' db.datname = '''+DB+'''';



  if SV >= 080000 then
   Sql := Format(Sql,['spcname,','pg_tablespace as tsp,','tsp.oid = dattablespace AND'])
  else
   Sql := Format(Sql,['','','']);

  RES := _PQExecute(Self, Sql);
  if Assigned(RES) then
   try
    CheckResult;
    if PQntuples(RES) > 0 then
     begin
      DBOid := StrToInt64(string(PQgetvalue(RES,0,0)));
      IsTemplate := PQgetvalue(RES,0,1) = 't';
      Owner := RawToString(PQgetvalue(RES,0,2));
      if SV >= 080000 then
        Tablespace := RawToString(PQgetvalue(RES,0,3));
      Comment := RawToString(PQgetvalue(RES,0,4));
     end;
   finally
    PQclear(RES);
   end;
end;


procedure TNativeConnect.GetTableProps(const TableName: string; var Owner,
  Comment, Tablespace: string; var HasOIDs: boolean;
  var TableOid: cardinal);
var
   sql, Tbl, Schema : String;
   I, SV : integer;
   RES : PPGresult;
begin
  Owner := '';
  Comment := '';
  Tablespace := '';
  HasOIDs := False;
  TableOid := 0;
  SV := GetServerVersionAsInt;


  Sql :=    'SELECT   pg_class.oid,                                            '+
      '         relhasoids,                                                    '+
      '         usename,                                                       '+
      '         COALESCE(pg_description.description,'''')                     '+
      '    %s                                '+
      ' FROM pg_class                                                               '+
      ' INNER JOIN pg_namespace ON (pg_class.relnamespace = pg_namespace.oid)       '+
      ' INNER JOIN pg_user ON (pg_class.relowner = pg_user.usesysid)            '+
      ' %s     '+
      ' LEFT JOIN pg_description ON (pg_description.objoid = pg_class.oid)          '+
      ' WHERE relkind IN (''r'', ''v'') AND relname = ''%s'' AND nspname LIKE ''%s''';


  Tbl := StringReplace(TableName,'"','',[rfReplaceAll]);
  I := Pos('.',Tbl);
  if I > 0 then
   begin
    Schema := Copy(Tbl, 1, I-1);
    Tbl := Copy(Tbl, I+1, MaxInt);
   end
  else
   Schema := '%';

  if SV >= 080000 then
   Sql := Format(Sql,[', COALESCE(pg_tablespace.spcname,''<DEFAULT>'')'
                ,'LEFT JOIN pg_tablespace ON (pg_class.reltablespace = pg_tablespace.oid)',
                Tbl,Schema])
  else
   Sql := Format(Sql,['','',Tbl,Schema]);
 try
  RES := _PQExecute(Self, Sql);
  if Assigned(RES) then
   try
    CheckResult;
    if PQntuples(RES) > 0 then
     begin
      TableOid := StrToInt64(RawToString(PQgetvalue(RES,0,0)));
      HasOIDs := PQgetvalue(RES,0,1) = 't';
      Owner := RawToString(PQgetvalue(RES,0,2));
      if SV >= 800000 then
        Tablespace := RawToString(PQgetvalue(RES,0,4));
      Comment := RawToString(PQgetvalue(RES,0,3));
     end;
   except
    PQclear(RES);
   end;
  PQclear(RES);
 except
 end;
end;

function TPSQLEngine.GetTableProps(hDB: hDBIDB; const TableName: string;
  var Owner, Comment, Tablespace: string; var HasOIDs: boolean;
  var TableOid: cardinal): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).GetTableProps(TableName, Owner, Comment,
                                Tablespace, HasOIDs, TableOid);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TNativeConnect.GetServerVersionAsInt: integer;
begin
  if FIntServerVersion <= 0 then
    FIntServerVersion := PQserverVersion(Handle);
  Result := FIntServerVersion
end;


function TPSQLEngine.GetFieldOldValue(hCursor: hDBICur; AFieldName: string; AParam: TParam): DBIResult;
begin
  try
    TNativeDataSet(hCursor).FieldOldValue(AFieldName, AParam);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.GetFieldValueFromBuffer(hCursor: hDBICur;
  PRecord: Pointer; AFieldName: string; AParam: TParam; const UnchangedAsNull: boolean): DBIResult;
begin
  try
    TNativeDataSet(hCursor).FieldValueFromBuffer(PRecord, AFieldName, AParam, UnchangedAsNull);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;


procedure TNativeDataSet.FieldOldValue(AFieldName: string; var AParam: TParam);
var AFNum, Len: integer;
    FVal: PAnsiChar;
begin
 AFNum := FieldIndex(AnsiQuotedStr(AFieldName, '"'));
 if AFNum = -1 then Exit;
 if FieldIsNull(AFNum) then
  begin                                        
   AParam.Value := Null;
   Exit;
  end;
 case FieldType(AFNum) of
  FIELD_TYPE_BYTEA:
                   begin
                     FVal := PQUnescapeBytea(FieldBuffer(AFNum),Len);
                     try
                      AParam.SetBlobData(FVal, Len);
                      TPSQLParam(AParam).DataTypeOID := FIELD_TYPE_BYTEA;
                     finally
                      PQFreeMem(FVal);
                     end;
                   end;
  FIELD_TYPE_BOOL: AParam.AsBoolean := SameText('t', Field(AFNum));
 else
   AParam.Value := Field(AFNum);
 end;
 if FieldType(AFNum) = FIELD_TYPE_OID then TPSQLParam(AParam).DataTypeOID := FIELD_TYPE_OID;
end;


procedure TNativeDataSet.FieldValueFromBuffer(PRecord: Pointer; AFieldName: string;
      var AParam: TParam; const UnchangedAsNull: boolean);
var
  I    : Integer;
  Fld    : TPSQLField;
  Src    : Pointer;

begin

  for I := 1 to FFieldDescs.Count do
  begin
    Fld := FFieldDescs.Field[I];
    Fld.Buffer:= PRecord;
    if CompareText(Fld.FieldName, AFieldName)<>0 then Continue;
    Src := Fld.FieldValue;
    Inc(PAnsiChar(Src));
    if not Fld.FieldChanged and not UnchangedAsNull then //field was not changed, we put there old value
     begin
      FieldOldValue(AFieldName, AParam);
      Exit;
     end;
    if Fld.FieldNull then
     AParam.Value := Null
    else
     begin
       case Fld.FieldType of
           fldBOOL:    AParam.AsBoolean := Boolean(SmallInt(Src^));
           fldINT16:   AParam.AsSmallInt := SmallInt(Src^);
           fldINT32:   AParam.AsInteger := LongInt(Src^);
           fldINT64:   begin
                        {$IFDEF DELPHI_5}
                         AParam.AsString := IntToStr(Int64(Src^));
                        {$ELSE}
                         AParam.DataType := DataTypeMap[Fld.FieldType];
                         AParam.Value := Int64(Src^);
                        {$ENDIF}
                       end;
           fldFLOAT:   AParam.AsFloat := Double(Src^);
           fldZSTRING: begin
                           {$IFDEF DELPHI_12}
                            if FConnect.IsUnicodeUsed then
                              AParam.AsString := PWideChar(Src)
                            else
                            {$ENDIF}
                              AParam.AsString := String(PAnsiChar(Src));
                            if (Fld.NativeType = FIELD_TYPE_BIT) or (Fld.NativeType = FIELD_TYPE_VARBIT) then
                             AParam.AsString := 'B' + AParam.AsString;
                       end;
           fldUUID:    AParam.AsString := String(PAnsiChar(Src));
           fldBLOB:    if Fld.NativeBLOBType = nbtOID then
                          begin
                            AParam.AsInteger := StrToUInt(BlobValue(Src, Fld));
                            TPSQLParam(AParam).DataTypeOID := FIELD_TYPE_OID;
                          end
                       else
                          if not Assigned(TBlobItem(Src^).Blob) or (TBlobItem(Src^).Blob.Size = 0) then
                            AParam.Value := Null
                          else
                            if Fld.FieldSubType = fldstMemo then
                              AParam.AsString := MemoValue(Src, False)
                            else
                              AParam.LoadFromStream(TBlobItem(Src^).Blob, ftBlob);
           fldDate:    AParam.AsDate := TDateTime(Src^);
           fldTime:    AParam.AsTime := TDateTime(Src^);
           fldTIMESTAMP: AParam.AsDateTime := TDateTime(Src^);
       end; //case
     end; //else
     Break;
  end;
end;

function TNativeConnect.GetTimeout: cardinal;
var
   RES : PPGresult;
begin
  Result := 0;
  if GetserverVersionAsInt <= 070302 then
   Exit;
  InternalConnect;
  RES := PQexec(Handle, 'SELECT current_setting(''statement_timeout'')');
  if Assigned(RES) then
   try
    CheckResult;
    if PQntuples(RES) > 0 then
      Result := StrToIntDef(RawToString(PQgetvalue(RES,0,0)),0);
   finally
    PQclear(RES);
   end;
end;

{$HINTS OFF}
procedure TNativeConnect.SetErrorVerbosity(const ErrorVerbosity: TErrorVerbosity);
var OldEV: TErrorVerbosity;
{$IFDEF M_DEBUG}
const EVNames: array[TErrorVerbosity] of ansistring = ('TERSE', 'DEFAULT', 'VERBOSE');
{$ENDIF}
begin
  if FLoggin then
   begin
    OldEV := PQsetErrorVerbosity(Handle, ErrorVerbosity);
    {$IFDEF M_DEBUG}
     LogDebugMessage('INFO', Format('Error verbosity changed from %s to %s', [EVNames[OldEV], EVNames[ErrorVerbosity]]));
    {$ENDIF}
   end;
end;
{$HINTS ON}

function TNativeConnect.SetTimeout(const Timeout: cardinal): cardinal;
var
   S : String;
   RES : PPGresult;
begin
  Result := 0;
  if GetserverVersionAsInt <= 070302 then
   Exit;
  InternalConnect;
  S := Format('SELECT set_config(''statement_timeout'', ''%d'', false)',[Timeout]);
  RES := PQexec(Handle, PAnsiChar(AnsiString(S)));
  if Assigned(RES) then
   try
    CheckResult;
    Result := StrToIntDef(string(PQgetvalue(Res, 0, 0)), Timeout);
   finally
    PQclear(RES);
   end;
end;

function TPSQLEngine.GetCommandTimeout(hDb: hDBIDb;
  var Timeout: cardinal): DBIResult;
begin
   try
     Database := hDb;
     Timeout := TNativeConnect(hDb).GetTimeout;
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;


function TNativeDataSet.HasFieldTimeZone(const FldNum: integer): boolean;
begin
 case FieldType(FldNum-1) of
   FIELD_TYPE_TIMETZ,
   FIELD_TYPE_TIMESTAMPTZ: Result := True;
 else
   Result := False;
 end;
end;

function TNativeDataSet.FieldTable(FieldNum: integer): cardinal;
begin
 if FStatement <> nil then
   Result := PQftable(FStatement,FieldNum)
 else
   Result := InvalidOid;
end;

function TNativeDataSet.FieldOrigin(FieldNum: integer): string;
var TabOid: cardinal;
    ColNum: integer;
    s: string;
    IsOK: boolean;
begin
 if FStatement <> nil then
   begin
    TabOid := FieldTable(FieldNum - 1); //pg_attribute uses 1 as first field index, but low level rotines not
    if TabOid <= InvalidOid then Exit;
    ColNum := FieldPosInTable(FieldNum - 1);
    s := Format('SELECT %u::regclass || ''.'' || quote_ident(attname) '+
                'FROM pg_attribute WHERE attrelid = %u AND attnum = %d',
                    [TabOid, TabOid, ColNum]);
    Result := FConnect.SelectStringDirect(PChar(s), IsOK, 0);
   end;
end;

function TNativeDataSet.FieldPosInTable(FieldNum: integer): Integer;
begin
 if FStatement <> nil then
  begin
   Result := PQftablecol(FStatement,FieldNum);
   if Result = 0 then
     Result := -1;
  end
 else
  Result := -1;
end;

function TPSQLEngine.GetLastInsertId(hCursor: hDBICur;
  const FieldNum: integer; var ID: integer): DBIResult;
begin
  try
    ID := TNativeDataset(hCursor).GetLastInsertID(FieldNum);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

procedure TNativeDataSet.SortBy(FieldNames: string);
var
	a, cnt, i : integer;
	str : string;
	Fields : array of integer;
	IsReverseOrder : array of boolean;
const
	sAsc : string = ' ASC';
	sDesc : string = ' DESC';
begin
  FSortingFields := Trim(FieldNames);

	if FSortingFields = '' then
   begin
    SetLength(FSortingIndex,0);
		Exit;
   end;

	cnt := 0;

	for i:=1 to Length(FieldNames) do
	begin
		if FieldNames[i] = ',' then Inc(cnt);//count number of fields

		if FieldNames[i] = #9 then
			FieldNames[i] := ' ';//replace TABs to SPACEs
	end;

	SetLength(Fields, cnt + 1);
	SetLength(IsReverseOrder, cnt + 1);

	i := 0;
	if cnt > 0 then//multi-fields sorting
		while Pos(',', FieldNames) <> 0 do
		begin
			a := Pos(',', FieldNames);
			str := Trim(copy(FieldNames, 1, a - 1));
			Delete(FieldNames, 1, a);

			if AnsiUpperCase(copy(str, Length(str) - Length(sDesc) + 1, Length(sDesc))) = sDesc then
			begin
				IsReverseOrder[i] := true;
				Delete(str, Length(str) - Length(sDesc) + 1, Length(sDesc));
			end
			else if AnsiUpperCase(copy(str, Length(str) - Length(sAsc) + 1, Length(sAsc))) = sAsc then
			begin
				IsReverseOrder[i] := false;
				Delete(str, Length(str) - Length(sAsc) + 1, Length(sAsc));
			end
			else
			begin
				IsReverseOrder[i] := false;
			end;

			a := FieldIndex(Trim(str));//trying to find dield in fields definitions
			if a = -1 then
       begin
				DatabaseError(Format(SFieldNotFound, [Str]));
        FSortingFields := '';
        SetLength(FSortingIndex,0);
       end;
			Fields[i] := a;
			Inc(i);
		end;

	//single field sorting   (or last field sorting)
	str := Trim(FieldNames);

	if AnsiUpperCase(copy(str, Length(str) - Length(sDesc) + 1, Length(sDesc))) = sDesc then
	begin
		IsReverseOrder[i] := true;
		Delete(str, Length(str) - Length(sDesc) + 1, Length(sDesc));
	end
	else if AnsiUpperCase(copy(str, Length(str) - Length(sAsc) + 1, Length(sAsc))) = sAsc then
	begin
		IsReverseOrder[i] := false;
		Delete(str, Length(str) - Length(sAsc) + 1, Length(sAsc));
	end
	else
	begin
		IsReverseOrder[i] := false;
	end;
  Str := Trim(str);
	a := FieldIndex(str);//trying to find field in fields definitions
	if a = -1 then
    begin
		 DatabaseError(Format(SFieldNotFound, [str]));
     FSortingFields := '';
     SetLength(FSortingIndex,0);
    end;
	Fields[i] := a;

	InternalSortBy(Fields, IsReverseOrder);
end;

procedure TNativeDataSet.InternalSortBy(const Fields: array of Integer;
  const IsReverseOrder: array of boolean);

var aRecNum: integer;
    i: integer;

    function CmpRecords(Index1, Index2: integer): integer;
    var i, Idx1IsNull, Idx2IsNull: integer;
      s1, s2: string;

      function FVal(Index: integer): string;
      begin
       Result := FConnect.RawToString(PQGetValue(FStatement,FSortingIndex[Index],Fields[I]));
      end;

      {$IFDEF DELPHI_5}
      function Sign(const AValue: Double): integer;
      begin
        if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000) then
          Result := 0
        else if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000) then
          Result := -1
        else
          Result := 1;
      end;
      {$ENDIF}

    begin
     Result := 0;
     for i:= Low(Fields) to High(Fields) do
      begin
        Idx1IsNull := PQGetIsNull(FStatement,FSortingIndex[Index1],Fields[I]);
        Idx2IsNull := PQGetIsNull(FStatement,FSortingIndex[Index2],Fields[I]);
        case Idx1IsNull + Idx2IsNull of
         2: Result := 0;
         1: Result := Idx1IsNull - Idx2IsNull;
        else
         case PQFType(FStatement,Fields[I]) of
            FIELD_TYPE_INT2,
            FIELD_TYPE_INT4,
            FIELD_TYPE_INT8: Result := StrToInt64Def(FVal(Index1),0) -
                                       StrToInt64Def(FVal(Index2),0);

            FIELD_TYPE_FLOAT4,
            FIELD_TYPE_FLOAT8,
            FIELD_TYPE_NUMERIC:
                             try
                              s1 := FVal(Index1);
                              s2 := FVal(Index2);
                              Result := Sign(StrToFloat(s1, PSQL_FS) -
                                       StrToFloat(s2, PSQL_FS));
                             except
                              //D5 have no StrToFloatDef
                              on E: EConvertError do
                               Result := 0;
                             end;

            FIELD_TYPE_BOOL: Result :=  ord(FVal(Index1)[1]) -
                                        ord(FVal(Index2)[1]);

            FIELD_TYPE_OID: if dsoOIDAsInt in FOptions then
                              Result := StrToIntDef(FVal(Index1),InvalidOid) -
                                        StrToIntDef(FVal(Index2),InvalidOid)
                            else
                              Result := 0;
            FIELD_TYPE_TEXT,
            FIELD_TYPE_BYTEA: Result := 0; //BLOB's are not comparable

          else
             //datetime fields will be compared here also
             //cause we have ISO output datestyle: yyyy-mm-dd hh:mm:ss[-tz]
             Result := AnsiStrComp(PChar(FVal(Index1)),PChar(FVal(Index2)));
          end;
        end;
        if IsReverseOrder[i] then
          Result := -Result;
        if Result <> 0 then Break;
      end;
    end;

    procedure SwapIndexes(Index1, Index2: integer);
    var T: integer;
    begin
      T := FSortingIndex[Index1];
      FSortingIndex[Index1] := FSortingIndex[Index2];
      FSortingIndex[Index2] := T;
    end;

    procedure QuickSort(L, R: Integer);
    var
         I, J, P: Integer;
    begin
      repeat
        I := L;
        J := R;
        P := (L + R) shr 1;
        repeat
          while CmpRecords(I, P) < 0 do
            Inc(I);
          while CmpRecords(J, P) > 0 do
            Dec(J);
          if I <= J then
          begin
            SwapIndexes(I, J);
            if P = I then
              P := J
            else if P = J then
              P := I;
            Inc(I);
            Dec(J);
          end;
        until I > J;
        if L < J then QuickSort(L, J);
        L := I;
      until I >= R;
    end;

begin
  aRecNum := GetRecCount;
  if (High(Fields) = -1) or (aRecNum < 2) then
		Exit;
  SetLength(FSortingIndex,aRecNum);

  for i:=0 to aRecNum-1 do //initialization
   FSortingIndex[i] := i;

  QuickSort(Low(FSortingIndex), High(FSortingIndex));
end;

function TNativeDataSet.GetRecNo: integer;
var
  nGetRecCount: Integer; 
begin
  Result := RecNo;
  nGetRecCount := GetRecCount();  
  if nGetRecCount > 0 then 
    if (High(FSortingIndex) = nGetRecCount-1) then Result := FSortingIndex[RecNo]; 
end;

function TNativeDataSet.IsSortedLocally: boolean;
begin
 Result := (High(FSortingIndex) > -1) and (High(FSortingIndex) = GetRecCount-1);  //19.05.2008
end;

procedure TPSQLIndexes.SetNeedUpdate(const Value: boolean);
begin
  FUpdated := Value;
end;

function TNativeDataSet.CheckCanLive: boolean;
var i: integer;
    TabOID: cardinal;
begin

 Result := False;

 if FConnect = nil then
   exit;

 if not IsQuery then //assume tables are editable by default
  begin
   Result := True;
   Exit;
  end;

 TabOID := FieldTable(0);
 if TabOID = InvalidOid then Exit;
 for i:=1 to FieldCount-1 do
   if (TabOID = InvalidOid) or (TabOID <> FieldTable(i)) then
    Exit
   else
    TabOID := FieldTable(i);

 Result := True; //all checks passed
end;

//>> pasha_golub 10.08.06
function TPSQLEngine.CheckBuffer(hCursor: hDBICur;
  PRecord: Pointer): DBIResult;
begin
  try
    if TNativeDataSet(hCursor).FCurrentBuffer = PRecord then
      TNativeDataSet(hCursor).FCurrentBuffer:= nil;
	 Result := DBIERR_NONE;
  except
	 Result := CheckError;
  end;
end;
//<< pasha_golub 10.08.06

function TPSQLEngine.OpenTablespaceList(hDb: hDBIDb; pszWild: string;
  List: TStrings): DBIResult;
begin
  try
    Database := hDb;
    TNativeConnect(hDb).TablespaceList(pszWild, List);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;


procedure TNativeConnect.TablespaceList(pszWild: string; List: TStrings);
var
   CRec : string;
   I : LongInt;
   sql : String;
   RES : PPGresult;
begin
  InternalConnect;
  Sql := 'SELECT spcname '+
         ' FROM "pg_tablespace" ';
  if pszWild <> '' then
    Sql := Sql + ' WHERE spcname LIKE ' + AnsiQuotedStr(pszWild,'''');
  Sql := Sql + ' ORDER BY 1';
 RES := _PQExecute(Self, Sql);
 try
  if Assigned(RES) then
  begin
     CheckResult;
     for I := 0 to PQntuples(RES)-1 do
     begin
        CRec := RawToString(PQgetvalue(RES,I,0));
        List.Add(CRec);
     end;
  end;
 finally
  PQclear(RES);
 end;
end;

function TNativeDataSet.UuidValue(P : Pointer; NeedQuote: boolean = True): string;
var AVal: PAnsiChar;
begin
  Result := '';
  if not Assigned(P) then Exit;
  AVal := PAnsiChar(P);
  Result := FConnect.RawToString(AVal);
  if NeedQuote then Result := '''' + Result + '''';
end;

function TNativeDataSet.StrValue(P : Pointer; NeedQuote: boolean = True):String;
var
   Buffer, AVal : PAnsiChar;
   SZ, Err : Integer;
begin
  Result := '';
  if P <> nil then
   begin
    {$IFDEF DELPHI_12}
    if FConnect.IsUnicodeUsed then
     AVal := PAnsiChar(FConnect.StringToRawS(PWideChar(P)))
    else
    {$ENDIF}
     AVal := PAnsiChar(P);

    if not NeedQuote then
     begin
      Result := FConnect.RawToString(AVal);
      Exit;
     end;

    SZ := StrLen(AVal);
    GetMem(Buffer, 2*SZ+1);
    try
    ZeroMemory(Buffer, 2*SZ+1);
    PQEscapeStringConn(FConnect.Handle, Buffer, AVal, SZ, Err);
    if Err > 0 then
     FConnect.CheckResult;
     Result := '''' + FConnect.RawToString(Buffer) + '''';
    finally
     FreeMem(Buffer);
    end;
   end;
end;

function TNativeDataSet.MemoValue(P : Pointer; NeedQuote: boolean = True):String;
var
   Buffer : PAnsiChar;
   SZ : Integer;
begin
  Result := '';
  if TBlobItem(P^).Blob <> nil then
  begin
    if TBlobItem(P^).Blob.Size = 0 then exit;
    SZ := TBlobItem(P^).Blob.Size + SizeOf(Char); //null termination
    GetMem(Buffer, SZ);
    ZeroMemory(Buffer,SZ);
    TBlobItem(P^).Blob.Seek(0,0);
    TBlobItem(P^).Blob.Read(Buffer^, SZ);
    Result := StrValue(Buffer, NeedQuote);
    FreeMem(Buffer, SZ);
  end;
end;

function TNativeDataSet.BlobValue(P : Pointer; Fld: TPSQLField; NeedEscape: boolean = True): string;
begin
  Result := BlobValue(TBlobItem(P^).Blob, Fld.NativeBLOBType = nbtBytea, NeedEscape);
end;

function TNativeDataset.BlobValue(MS: TStream; isBytea: boolean; NeedEscape: Boolean = True): string;
var
   Buffer, PEsc : PAnsiChar;
   SZ : Integer;
   Res : LongInt;
   Off, BlSZ: Integer;
begin
  Result := '0';
  if not Assigned(MS) or (MS.Size = 0) then Exit;
  SZ := MS.Size;
  GetMem(Buffer, SZ + 1);
  ZeroMemory(Buffer, SZ + 1);
  MS.Seek(0,0);
  MS.Read(Buffer^, SZ);
  if isBytea then
    begin
      if NeedEscape then
       begin
         PEsc := PQEscapeByteaConn(FConnect.Handle, Buffer, SZ, BlSZ);
         try
          Result := '''' + FConnect.RawToString(PEsc) + '''';
         finally
          PQFreeMem(PEsc);
         end;
       end
      else
       Result := string(Buffer);
    end
  else    //nbtOID in other case
    begin
      FConnect.BeginBLOBTran;  //BLOB Trans
      FBlobHandle := lo_creat(FConnect.Handle,INV_WRITE or INV_READ);
      if FBlobHandle = 0 then
        begin
         FConnect.RollbackBLOBTran;
         raise EPSQLException.CreateMsg(FConnect,'Can''t create BLOB! lo_creat operation failed!')
        end;
      FLocalBHandle := lo_open(FConnect.Handle, FBlobHandle, INV_WRITE);
      try
      Off := 0;
      repeat
        BlSZ := Min(MAX_BLOB_SIZE, SZ - off);
        Res  := lo_write(FConnect.Handle, FLocalBHandle, Buffer + off, BLSZ);
        if Res < 0 then
          raise EPSQLException.CreateMsg(FConnect,'BLOB operation failed!')
        else
          Inc(Off, Res);
      until (off >= SZ);
      FreeMem(Buffer, SZ+1);
     except
      lo_close(FConnect.Handle,FlocalBHandle);
      //BLOB Trans
      FConnect.RollbackBLOBTran;
      raise;
     end;
      lo_close(FConnect.Handle,FlocalBHandle);
      //BLOB Trans
      FConnect.CommitBLOBTran;
      Result := UIntToStr(FBlobHandle);
    end;
end;

function TPSQLEngine.QGetProcParams(hStmt: hDBIStmt;
  Params: TParams): DBIResult;
begin
  try
    TNativeDataSet(hStmt).StoredProcGetParams(Params);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;

end;

procedure TNativeDataSet.StoredProcGetParams(Params: TParams);
var i,j: integer;
begin
  if not Assigned(FStatement) then Exit;
  for i:=0 to Params.Count-1 do
   if Params[i].ParamType in [ptOutput, ptInputOutput] then
     for j := 0 to FieldCount - 1 do
      if Params[i].Name = FieldName(j) then
        Params[i].AsString := Field(j);
end;

function TNativeConnect.RawToString(S: PAnsiChar): string;
begin

 if IsUnicodeUsed then
{$IFDEF DELPHI_12}
  Result := UTF8ToUnicodeString(S)
{$ELSE}
  Result := UTF8ToString(S)
{$ENDIF}
 else
  Result := string(S);
end;

procedure TNativeConnect.Reset;
begin
  PQreset(FHandle);
end;

function TPSQLEngine.Reset(hDb: hDBIDb): DBIResult;
begin
   try
    Database := hDb;
    TNativeConnect(hDB).Reset;
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.CancelBackend(hDb: hDBIdb; PID: Integer): DBIResult;
begin
   try
     Database := hDb;
     TNativeConnect(hDb).CancelBackend(PID);
     Result := DBIERR_NONE;
   except
     Result := CheckError;
   end;
end;

procedure TNativeConnect.CancelBackend(PID: Integer);
var
   RES: PPGresult;
   sql: AnsiString;
begin
  InternalConnect;
  sql := AnsiString(Format('SELECT pg_cancel_backend(%u)',[PID]));
  RES := PQexec(Handle,PAnsiChar(sql));
  try
    CheckResult;
  finally
    PQClear(RES);
  end;
end;

function TPSQLEngine.SelectStringDirect(hDb: hDBIDb; pszQuery: PChar;
  var IsOk: boolean; var aResult: string;
  aFieldNumber: integer): DBIResult;
begin
  try
    aResult := TNativeConnect(hDB).SelectStringDirect(pszQuery, IsOk, aFieldNumber);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SelectStringDirect(hDb: hDBIDb; pszQuery: PChar;
  var IsOk: boolean; var aResult: string; aFieldName: string): DBIResult;
begin
  try
    aResult := TNativeConnect(hDB).SelectStringDirect(pszQuery, IsOk, PChar(aFieldName));
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TNativeConnect.SelectStringDirect(pszQuery: string;
  var IsOk: boolean; aFieldNumber: integer): string;
var
	Stmt : PPGresult;
begin
  Result := '';
	InternalConnect;

	Stmt := _PQExecute(Self, pszQuery);
  try
    IsOK := (PQresultStatus(Stmt) = PGRES_TUPLES_OK) and
            (PQnfields(Stmt) > aFieldNumber) and
            (aFieldNumber >= 0) and
            (PQntuples(Stmt) > 0);
    if IsOK then
      Result := RawToString(PQgetvalue(Stmt,0,aFieldNumber));
    {else
      CheckResult;}
  finally
   PQClear(Stmt);
  end;
end;

function TNativeConnect.SelectStringDirect(pszQuery: string;
  var IsOk: boolean; pszFieldName : string): string;
var
	Stmt : PPGresult;
  P: PAnsiChar;
begin
  Result := '';
	InternalConnect;

	Stmt := _PQExecute(Self, pszQuery);
  try
    P := StringToRaw(pszFieldName);
    IsOK := (PQresultStatus(Stmt) = PGRES_TUPLES_OK) and
            (PQfnumber(Stmt, P) > -1) and
            (PQntuples(Stmt) > 0);
    if IsOK then
      Result := RawToString(PQgetvalue(Stmt,0,PQfnumber(Stmt, P)));
    {else
      CheckResult;}
    StrDispose(P);
  finally
   PQClear(Stmt);
  end;
end;

function TNativeConnect.IsUnicodeUsed: boolean;
var S: string;
begin
 S := GetCharSet();
 Result := (S = 'UNICODE') or (S = 'UTF8');
end;

function TPSQLEngine.GetFieldTypeOID(hCursor: hDBICur; const FieldNum: integer): cardinal;
begin
  Result := TNativeDataset(hCursor).FieldType(FieldNum);
end;

procedure TFieldList.GetFLDDesc(PRecord: pFLDDesc);
begin
 PRecord^ := TFLDDescList(Descs)[Position];
end;

procedure TFieldList.GetNextRecord(eLock: DBILockType; PRecord: Pointer;
  pRecProps: pRECProps);
begin
  if Position = Items then
    raise EPSQLException.CreateBDE(DBIERR_EOF)
  else
    GetFLDDesc(PRecord);
  Inc(Position);
end;


procedure TFieldList.GetRecordCount(var iRecCount: Integer);
begin
   iRecCount := Items;
end;

function TFieldList.GetWorkBufferSize: Word;
begin
  Result := GetBufferSize;
end;

procedure TFieldList.SetToBegin;
begin
  inherited SetToBegin;
  Position := 0;
end;

procedure TFieldList.SetToBookmark(P: Pointer);
begin
   SetToBegin;
end;

function TPSQLIndex.GetIndexName: string;
begin
 Result := FDesc.szName;
end;

procedure TPSQLIndex.SetIndexName(const Value: string);
begin
 Move(Value[1], FDesc.szName, (Min(Length(Value), DBIMAXNAMELEN)) * SizeOf(Char));
end;

function TPSQLEngine.GetFieldOrigin(hCursor: hDBICur;
  const FieldNum: integer): string;
begin
  Result := TNativeDataset(hCursor).FieldOrigin(FieldNum);
end;

function TPSQLEngine.SelectStringsDirect(hDb: hDBIDb; pszQuery: PChar;
  aList: TStrings; aFieldNumber: integer): DBIResult;
begin
  try
    TNativeConnect(hDB).SelectStringsDirect(pszQuery, aList, aFieldNumber);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TPSQLEngine.SelectStringsDirect(hDb: hDBIDb; pszQuery: PChar;
  aList: TStrings; aFieldName: string): DBIResult;
begin
  try
    TNativeConnect(hDB).SelectStringsDirect(pszQuery, aList, aFieldName);
    Result := DBIERR_NONE;
  except
    Result := CheckError;
  end;
end;

function TNativeConnect.SelectStringsDirect(pszQuery: string;
  aList: TStrings; aFieldNumber: integer): string;
var
	Stmt : PPGresult;
  i: integer;
  IsOK: boolean;
begin
  Result := '';
	InternalConnect;

	Stmt := _PQExecute(Self, pszQuery);
  try
    IsOK := (PQresultStatus(Stmt) = PGRES_TUPLES_OK) and
            (PQnfields(Stmt) > aFieldNumber) and
            (PQntuples(Stmt) > 0);
    if IsOK then
      try
       aList.BeginUpdate;
       for i := 0 to PQntuples(Stmt) - 1 do
         aList.Append(RawToString(PQgetvalue(Stmt, i, aFieldNumber)));
      finally
       aList.EndUpdate;
      end
    else
      CheckResult;
  finally
   PQClear(Stmt);
  end;
end;


function TNativeConnect.SelectStringsDirect(pszQuery: string;
  aList: TStrings; pszFieldName: string): string;
var
	Stmt : PPGresult;
  P: PAnsiChar;
  i, ColNum: integer;
  IsOK: boolean;
begin
  Result := '';
	InternalConnect;

	Stmt := _PQExecute(Self, pszQuery);
  try
    P := StringToRaw(pszFieldName);
    ColNum := PQfnumber(Stmt, P);
    IsOK := (PQresultStatus(Stmt) = PGRES_TUPLES_OK) and
            (ColNum > -1) and
            (PQntuples(Stmt) > 0);
    if IsOK then
      try
       aList.BeginUpdate;
       for i := 0 to PQntuples(Stmt) - 1 do
         aList.Append(RawToString(PQgetvalue(Stmt, i, ColNum)));
      finally
       aList.EndUpdate;
      end
    else
      CheckResult;
    StrDispose(P);
  finally
   PQClear(Stmt);
  end;
end;


initialization

  {$IFDEF M_DEBUG}
  OpenDebugFile;
  {$ENDIF}

  {$IFDEF DELPHI_15}
  PSQL_FS := TFormatSettings.Create();
  {$ELSE}
    {$IFNDEF FPC}
         GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, PSQL_FS);
    {$ENDIF}
  {$ENDIF}
  PSQL_FS.DecimalSeparator := '.'; //for use inside StrToFloat
  PSQL_FS.TimeSeparator := ':'; //for use inside FormatDateTime

finalization

  {$IFDEF M_DEBUG}
  CloseDebugFile;
  {$ENDIF}

end.
