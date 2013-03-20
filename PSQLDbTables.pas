{$I PSQLDAC.inc}
unit PSQLDbTables;

{SVN revision: $Id$}

{$R-,T-,H+,X+}
{$C+}
interface

uses  SysUtils, Classes, Db,
      {$IFDEF DELPHI_9}DbCommon{$ELSE}PSQLCommon{$ENDIF},
      {$IFDEF DELPHI_6}Variants,{$ENDIF}
      {$IFDEF FPC}Variants,{$ENDIF}
      {$IFDEF DELPHI_17}System.Types, System.Generics.Collections,{$ENDIF}
      PSQLAccess, PSQLTypes;

const
    VERSION : string = '2.9.4';
    {$IFDEF MICROOLAP_BUSINESS_LICENSE}
    LICENSETYPE : string = 'Business License';
    {$ELSE}
      {$IFDEF MICROOLAP_COMMERCIAL_LICENSE}
      LICENSETYPE : string = 'Commercial License';
      {$ELSE}
        {$IFDEF MICROOLAP_EDU_CLASSROOM_LICENSE}
        LICENSETYPE : string = 'Educational classroom License';
        {$ELSE}
          {$IFDEF MICROOLAP_EDU_INSTITUTION_LICENSE}
          LICENSETYPE : string = 'Educational institution License';
          {$ELSE}
            {$IFDEF MICROOLAP_PERSONAL_LICENSE}
            LICENSETYPE : string = 'Personal License';
            {$ELSE}
              {$IFDEF TRIAL}
              LICENSETYPE : string = 'Trial License';
              {$ELSE}
              LICENSETYPE : string = 'Edited license string => Trial license';
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

{ TDBDataSet flags }          
  dbfOpened     = 0;
  dbfPrepared   = 1;
  dbfExecSQL    = 2;
  dbfTable      = 3;
  dbfFieldList  = 4;
  dbfIndexList  = 5;
  dbfStoredProc = 6;
  dbfExecProc   = 7;
  dbfProcDesc   = 8;
  dbfDatabase   = 9;
  dbfProvider   = 10;

{ FieldType Mappings }

const
  {$IFDEF FPC}
  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16,  //0..4
    fldBOOL, fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, //5..11
    fldBYTES, fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, //12..18
    fldBLOB, fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, //19..24
    fldINT64, fldADT, fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, //25..31
    fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldZSTRING, fldDATETIME, fldBCD,
    fldZSTRING, fldBLOB);

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY, 0, fldstFIXED, fldstUNICODE,
    0, 0, 0, 0, 0, fldstHBINARY, fldstHMEMO, 0, 0, 0, 0, 0, 0,
    fldstFIXED, fldstMEMO);

  {$ELSE}

  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16,  //0..4
    fldBOOL, fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, //5..11
    fldBYTES, fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, //12..18
    fldBLOB, fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, //19..24
    fldINT64, fldADT, fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, //25..31
    fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, fldZSTRING
    {$IFDEF DELPHI_6}, fldDATETIME, fldBCD{$ENDIF} //26..37
    {$IFDEF DELPHI_10}, fldZSTRING, fldBLOB, fldDATETIME, fldINT32{$ENDIF} //38..41
    {$IFDEF DELPHI_12}, fldINT32, fldINT16, fldUNKNOWN, fldFLOATIEEE, fldUNKNOWN, fldUNKNOWN, fldUNKNOWN{$ENDIF} //42..48
    {$IFDEF DELPHI_14}, fldUNKNOWN, fldUNKNOWN, fldUNKNOWN{$ENDIF} //49..52
    );

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY, 0, fldstFIXED, fldstUNICODE,
    0, 0, 0, 0, 0, fldstHBINARY, fldstHMEMO, 0, 0, 0, 0{$IFDEF DELPHI_6} , 0, 0{$ENDIF}
    {$IFDEF DELPHI_10}, fldstFIXED, fldstMEMO, 0, 0 {$ENDIF} //38..41
    {$IFDEF DELPHI_12}, 0, 0, 0, 0, 0, 0, 0{$ENDIF} //42..48
    {$IFDEF DELPHI_14}, 0, 0, 0{$ENDIF} //49..52
    );
  {$ENDIF}


  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftUnknown,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet
    {$IFDEF DELPHI_6},ftTimeStamp{$ENDIF}
    {$IFDEF DELPHI_12},ftFMTBcd, ftWideString{$ENDIF});

  BlobTypeMap: array[fldstMEMO..fldstBFILE] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftBlob, ftOraClob, ftOraBlob,
    ftBlob, ftBlob);    

type

  //used for LOCK TABLE
  TPSQLLockType = (ltAccessShare, ltRowShare, ltRowExclusive, ltShareUpdateExclusive,
                   ltShare, ltShareRowExclusive, ltExclusive, ltAccessExclusive);
  

  { Forward declarations }
  TPSQLDatabase      = Class;
  TPSQLDatabaseClass = Class of TPSQLDatabase;
  TPSQLDataSet       = Class;
  TPSQLTable         = Class;
  TPSQLTableClass    = Class of TPSQLTable;
  TPSQLQuery         = Class;
  TPSQLQueryClass    = Class of TPSQLQuery;

  { Exception Classes }
  EPSQLDatabaseError =  Class(EDatabaseError)
    private
      FErrorCode: Word;
      //Error fields added by Tony Caduto 5/17/2006
      FErrorPos:string;
      FErrorContext:string;
      FErrorseverity:string;
      FErrorsqlstate:string;
      FErrorprimary:string;
      FErrordetail:string;
      FErrorhint:string;
      FErrorinternalpos:string;
      FErrorinternalquery:string;
      FErrorsourcefile:string;
      FErrorsourceline:string;
      FErrorsourcefunc:string;
    public
      constructor Create(Engine : TPSQLEngine; ErrorCode: Word);
      destructor Destroy; override;
      property ErrorCode: Word read FErrorCode;
      //Error Field properties
      property ErrorPos:string read FErrorPos;
      property ErrorContext:string read FErrorContext;
      property ErrorSeverity:string read FErrorseverity;
      property ErrorSqlState:string read FErrorsqlstate;
      property ErrorPrimary:string read FErrorprimary;
      property ErrorDetail:string read FErrordetail;
      property ErrorHint:string read FErrorhint;
      property ErrorInternalPos:string read FErrorinternalpos;
      property ErrorInternalQuery:string read FErrorinternalquery;
      property ErrorSourceFile:string read FErrorsourcefile;
      property ErrorSourceLine:string read FErrorsourceline;
      property ErrorSourceFunc:string read FErrorsourcefunc;
  end;

  ENoResultSet = class(EDatabaseError);

  TParamClass = class of TParam;

  TPSQLParam = class(TParam)
  private
    FDataTypeOID: cardinal;
    FBinary: boolean;
    procedure SetDataTypeOID(const Value: cardinal);
  protected
    function IsEqual(Value: TParam): Boolean;
  published
    property DataTypeOID: cardinal read FDataTypeOID write SetDataTypeOID default 0;
    property Binary: boolean read FBinary write FBinary default False;
  end;

  TPSQLParams = class(TParams)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TPSQLParam;
    procedure SetItem(Index: Integer; const Value: TPSQLParam);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent); overload;
    constructor Create; overload;
    procedure AssignValues(Value: TParams);
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TParamType; const DataTypeOID: cardinal = 0; Binary: boolean = False): TPSQLParam;
    function ParamByName(const Value: string): TPSQLParam;
    function ParseSQL(SQL: string; DoCreate: Boolean): string; reintroduce;
    property Items[Index: Integer]: TPSQLParam read GetItem write SetItem; default;
  end;

  TDatabaseNoticeEvent = procedure (Sender: TPSQLDatabase; Message: string) of object;
  TBaseDatabaseLoginEvent = procedure(Database: TPSQLDatabase; LoginParams: TStrings) of object;
  TDbExceptionEvent = procedure (Sender: TObject; E: Exception) of object;

  TDBFlags = set of 0..15;

  TTransIsolation = (tiDirtyRead, tiReadCommitted, tiRepeatableRead);


  TPSQLDBDesignOption = (ddoStoreConnected, ddoStorePassword);
  TPSQLDBDesignOptions = set of TPSQLDBDesignOption;

  TPSQLDatabase =  Class(TCustomConnection)
    private
      FAbout   : TPSQLDACAbout;
      FTransIsolation: TTransIsolation;
      FKeepConnection: Boolean; //AutoStop
      FOEMConvert : Boolean;  //OEM->ANSI
      FCharSet: string;
      FCommandTimeout: cardinal;
      FEngine : TPSQLEngine; //Postgres Engine
      FTemporary: Boolean;
      FAcquiredHandle: Boolean;
      FPseudoIndexes: Boolean;
      FHandleShared: Boolean;
      FExclusive: Boolean;
      FReadOnly: Boolean;
      FRefCount: Integer;
      FHandle: HDBIDB;
      FParams: TStrings;
      FStmtList: TList;
      FOwner: string;
      FIsTemplate: boolean;
      FTablespace: string;
      FDatabaseID: cardinal;
      FComment: string;
      FServerVersion: string;
      FDesignOptions: TPSQLDBDesignOptions;//design time info for DB
      FOnAdd: TNotifyEvent;
      FOnLogin: TBaseDatabaseLoginEvent;
      FOnNotice: TDatabaseNoticeEvent;
      FNotifyList: TList;  //List of notify
      FDirectQueryList : TList;
      FCheckIfActiveOnParamChange: boolean;
      FSSLMode: TSSLMode;
      FErrorVerbosity: TErrorVerbosity;
      FOnException: TDbExceptionEvent;
      FUseSingleLineConnInfo: boolean;
      function GetNotifyItem(Index: Integer): TObject;
      function GetNotifyCount: Integer;
      procedure FillAddonInfo;
      procedure CheckActive;
      procedure CheckInactive;
      procedure ClearStatements;
      procedure EndTransaction(TransEnd: EXEnd);
      function GetInTransaction: Boolean;
      function GetTransactionStatus: TTransactionStatusType;
      function GetServerVersionAsInt: integer;
      procedure Login(LoginParams: TStrings);
      procedure ParamsChanging(Sender: TObject);
      procedure SetDatabaseFlags;
      procedure SetDatabaseName(const Value: string);
      procedure SetUserName(const Value: string);
      procedure SetUserPassword(const Value: string);
      procedure SetServerPort(const Value: Cardinal);
      procedure SetConnectionTimeout(const Value: cardinal);
      procedure SetCommandTimeout(const Value: cardinal);
      procedure SetHost(const Value : string);
      procedure SetKeepConnection(Value: Boolean);
      procedure SetExclusive(Value: Boolean);
      procedure SetHandle(Value: HDBIDB);
      procedure SetParams(Value: TStrings);
      procedure SetReadOnly(Value: Boolean);
      procedure SetDummyStr(Value: string);
      procedure SetDummyBool(Value: boolean);
      procedure SetDummyInt(Value: cardinal);
      procedure SetSSLMode(const Value: TSSLMode);
      procedure SetErrorVerbosity(const Value: TErrorVerbosity);
      function GetDatabaseID: cardinal;
      function GetIsTemplate: boolean;
      function GetDBOwner: string;
      function GetTablespace: string;
      function GetIsUnicodeUsed: Boolean;
      function GetDatabaseComment: string;
      function GetIsSSLUsed: Boolean;
      procedure SetUseSSL(Reader: TReader);
      function GetUsrName: string;
      function GetUserPassword: string;
      function GetDatabaseName: string;
      function GetSSLOption(Index: integer): string;
      procedure SetSSLOption(Index: integer; Value: string);
      function GetConnectionTimeout: cardinal;
      function GetServerPort: Cardinal;
      function GetHost: string;
    protected
      procedure DefineProperties(Filer: TFiler); override; //deal with old missing properties
      procedure CloseDatabaseHandle;
      procedure CloseDatabase(Database: TPSQLDatabase);
      procedure DoConnect; override;
      procedure DoDisconnect; override;
      function GetConnected: Boolean; override;
      function GetStoreConnected: boolean;
      function GetDataSet(Index: Integer): TPSQLDataSet; reintroduce;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure InitEngine; //Init SQL Engine
      procedure AddDatabase(Value : TPSQLDatabase);
      procedure RemoveDatabase(Value : TPSQLDatabase);
      property CheckIfActiveOnParamChange: boolean read FCheckIfActiveOnParamChange write FCheckIfActiveOnParamChange;
      procedure WriteState(Writer: TWriter); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      function  Engine : TPSQLEngine;
      function Execute(const SQL: string; Params: TParams = nil; Cache: Boolean = FALSE; Cursor: phDBICur = nil): Integer;
      function GetBackendPID: Integer;

      function SelectString(aSQL: string; var IsOk: boolean; aFieldName: string): string; overload;
      function SelectString(aSQL: string; var IsOk: boolean; aFieldNumber: integer = 0): string; overload;
      function SelectStringDef(aSQL: string; aDefaultValue: string; aFieldName: string): string; overload;
      function SelectStringDef(aSQL: string; aDefaultValue: string; aFieldNumber: integer = 0): string; overload;

      function Ping: TPingStatus; overload;
      function Ping(ConnectionParams: TStrings): TPingStatus; overload;

      procedure SelectStrings(aSQL: string; aList: TStrings; aFieldName: string); overload;
      procedure SelectStrings(aSQL: string; aList: TStrings; aFieldNumber: integer = 0); overload;

      procedure AddNotify(AItem: TObject);
      procedure ApplyUpdates(const DataSets: array of TPSQLDataSet);
      procedure CancelBackend(PID : Integer);
      procedure CloseDataSets;
      procedure CloseNotify;
      procedure Commit;
      procedure GetCharsets(List: TStrings);
      procedure GetDatabases(Pattern: string;List : TStrings);
      procedure GetSchemaNames(Pattern: string; SystemSchemas: Boolean; List: TStrings);
      procedure GetStoredProcNames(Pattern: string; List: TStrings);
      procedure GetTableNames(Pattern: string; SystemTables: Boolean; List: TStrings);
      procedure GetTablespaces(Pattern: string; List: TStrings);
      procedure GetUserNames(Pattern: string; List: TStrings);
      procedure RegisterDirectQuery(aDirectQuery : TObject);
      procedure RemoveNotify(AItem: TObject);
      procedure Reset;
      procedure Rollback;
      procedure SetCharSet(CharSet: string);
      procedure StartTransaction;
      procedure UnregisterDirectQuery(aDirectQuery : TObject);

      procedure Savepoint(const Name: string);
      procedure ReleaseSavepoint(const Name: string);
      procedure RollbackToSavepoint(const Name: string);

      property DataSets[Index: Integer]: TPSQLDataSet read GetDataSet;
      property Handle: HDBIDB read FHandle write SetHandle;
      property InTransaction: Boolean read GetInTransaction;
      property IsUnicodeUsed: Boolean read GetIsUnicodeUsed;
      property IsSSLUsed: Boolean read GetIsSSLUsed;
      property Notifies[Index: Integer]: TObject read GetNotifyItem;
      property NotifyCount: Integer read GetNotifyCount;
      property ServerVersionAsInt: integer read GetServerVersionAsInt;
      property Temporary: Boolean read FTemporary write FTemporary;
      property TransactionStatus: TTransactionStatusType read GetTransactionStatus;
      property UseSingleLineConnInfo: boolean read FUseSingleLineConnInfo write FUseSingleLineConnInfo;
    published
      property About : TPSQLDACAbout read FAbout write FAbout;
      property AfterConnect;
      property AfterDisconnect;
      property BeforeConnect;
      property BeforeDisconnect;
      property CharSet: string read FCharSet write SetCharSet;
      property CommandTimeout: cardinal read FCommandTimeout write SetCommandTimeout default 0;
      property Comment: string read GetDatabaseComment write SetDummyStr stored False;
      property Connected stored GetStoreConnected;
      property DatabaseID: cardinal read GetDatabaseID write SetDummyInt stored False;
      property DesignOptions: TPSQLDBDesignOptions read FDesignOptions write FDesignOptions default [ddoStoreConnected, ddoStorePassword];
      property ErrorVerbosity: TErrorVerbosity read FErrorVerbosity write SetErrorVerbosity default evDEFAULT;
      property Exclusive: Boolean read FExclusive write SetExclusive default False;
      property HandleShared: Boolean read FHandleShared write FHandleShared default False;
      property IsTemplate: boolean read GetIsTemplate write SetDummyBool stored False;
      property KeepConnection: Boolean read FKeepConnection write SetKeepConnection default True;
      property LoginPrompt;
      property OEMConvert: Boolean read FOEMConvert write FOEMConvert default False;
      property OnAdd: TNotifyEvent read FOnAdd;
      property OnException: TDbExceptionEvent read FOnException write FOnException;
      property OnLogin: TBaseDatabaseLoginEvent read FOnLogin write FOnLogin;
      property OnNotice: TDatabaseNoticeEvent read FOnNotice write FOnNotice;
      property Owner: string read GetDBOwner write SetDummyStr stored False;
      property ReadOnly: Boolean read FReadOnly write SetReadOnly default FALSE;
      property ServerVersion: string read FServerVersion write SetDummyStr stored False;
      property Tablespace: string read GetTablespace write SetDummyStr stored False;
      property TransIsolation: TTransIsolation read FTransIsolation write FTransIsolation default tiReadCommitted;
      //connection parameters
      property ConnectionTimeout: cardinal read GetConnectionTimeout write SetConnectionTimeout stored False default 15;
      property DatabaseName: string read GetDatabaseName write SetDatabaseName stored False;
      property Host: string read GetHost write SetHost stored False;
      property Params: TStrings read FParams write SetParams;
      property Port : Cardinal read GetServerPort write SetServerPort stored False default PSQL_PORT;
      property UserName : string read GetUsrName write SetUserName stored False; //method name changed due to bug in ILINK32
      property UserPassword : string read GetUserPassword write SetUserPassword stored False;
      property SSLMode: TSSLMode read FSSLMode write SetSSLMode default sslPrefer;
      property SSLCert: string  index 0 read GetSSLOption write SetSSLOption stored False;
      property SSLKey: string index 1 read GetSSLOption write SetSSLOption stored False;
      property SSLRootCert: string index 2 read GetSSLOption write SetSSLOption stored False;
      property SSLCRL: string index 3 read GetSSLOption write SetSSLOption stored False;
  end;

  { TPSQLBDECallBack }
  TPSQLBDECallbackEvent = function(CBInfo: Pointer): CBRType of Object;

  TPSQLBDECallBack = Class(TObject)
    Private
      FHandle: hDBICur;
      FOwner: TObject;
      FCBType: CBType;
      FOldCBData: Longint;
      FOldCBFunc: pfDBICallBack;
      FCallbackEvent: TPSQLBDECallbackEvent;
      FEngine : TPSQLEngine;
    protected
      function Invoke(CallType: CBType; CBInfo: Pointer): CBRType;
    public
      constructor Create(Engine : TPSQLEngine; AOwner: TObject; Handle: hDBICur; CBType: CBType;
        CBBuf: Pointer; CBBufSize: Integer; CallbackEvent: TPSQLBDECallbackEvent;Chain: Boolean);
      destructor Destroy; override;
  end;



    { TLocale }

  TLocale = Pointer;

  
  TRecNoStatus = (rnDbase, rnParadox, rnNotSupported);

  TPSQLSQLUpdateObject = class(TComponent)
  protected
     function GetDataSet: TPSQLDataSet; virtual; abstract;
     procedure SetDataSet(ADataSet: TPSQLDataSet); virtual; abstract;
     procedure Apply(UpdateKind: TUpdateKind); virtual; abstract;
     function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
     property DataSet: TPSQLDataSet read GetDataSet write SetDataSet;
  end;

 TKeyIndex = (kiLookup, kiRangeStart, kiRangeEnd, kiCurRangeStart,
    kiCurRangeEnd, kiSave);

 PKeyBuffer = ^TKeyBuffer;
 TKeyBuffer = packed record
     Modified: Boolean;
     Exclusive: Boolean;
     FieldCount: Integer;
 end;

 PRecInfo = ^TRecInfo;
 TRecInfo = packed record
      RecordNumber: Longint;
      UpdateStatus: TUpdateStatus;
      BookmarkFlag: TBookmarkFlag;
 end;

 TBlobDataArray = array of TBlobData;

 TPSQLDataSet = Class(TDataSet)
  private
    FAbout : TPSQLDACAbout;
    FRecProps: RecProps; //Record properties
    FExprFilter: HDBIFilter; //Filter expression
    FFuncFilter: HDBIFilter; // filter function
    FFilterBuffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; // filter buffer
    FIndexFieldMap: DBIKey; //Index field map
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
    FCacheBlobs: Boolean;
    FKeySize: integer;
    FUpdateCBBuf: PDELAYUPDCbDesc;
    FUpdateCallback: TPSQLBDECallBack;
    FKeyBuffers: array[TKeyIndex] of PKeyBuffer;
    FKeyBuffer: PKeyBuffer;
    FRecNoStatus: TRecNoStatus;
    FIndexFieldCount: Integer;
    FRecordSize: integer;
    FBookmarkOfs: integer;
    FRecInfoOfs: integer;
    FBlobCacheOfs: integer;
    FRecBufSize: integer;
    FBlockBufOfs: Integer;
    FLastParentPos: Integer;
    FBlockReadBuf: PAnsiChar;
    FBlockBufSize: Integer;
    FBlockBufCount: Integer;
    FBlockReadCount: Integer;
    FOldBuffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
    FParentDataSet: TPSQLDataSet;
    FUpdateObject: TPSQLSQLUpdateObject;
    {$IFNDEF FPC}
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    {$ENDIF}
    FAutoRefresh: Boolean;
    FDBFlags: TDBFlags;
    FUpdateMode: TUpdateMode;
    FDatabase: TPSQLDatabase;
    FAllowSequenced : Boolean;  //Add by Nicolas Ring
    FSortFieldNames: string;
    FOptions: TPSQLDatasetOptions;
    procedure ClearBlobCache(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
    function GetActiveRecBuf(var RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): Boolean;
    function GetBlobData(Field: TField; Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): TBlobData;
    function GetOldRecord: PAnsiChar;
    procedure InitBufferPointers(GetProps: Boolean);
    function RecordFilter(RecBuf: Pointer; RecNo: Integer): Smallint; stdcall;
    procedure SetBlobData(Field: TField; Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Value: TBlobData);
    function GetDBHandle: HDBIDB;
    procedure SetUpdateMode(const Value: TUpdateMode);
    procedure SetAutoRefresh(const Value: Boolean);
    procedure SetDatabase(Value : TPSQLDatabase);
    function GetDatabase:TPSQLDatabase;
    {$IFNDEF DELPHI_4}{$IFNDEF FPC}
    procedure SetupAutoRefresh;
    {$ENDIF}{$ENDIF}
    function GetStmtHandle: HDBIStmt;
    procedure SetSortFieldNames(const Value: string);
    function GetSortFieldNames: string;
    procedure ReadByteaOpt(Reader: TReader); //deal with old missing properties
    procedure ReadOIDOpt(Reader: TReader); //deal with old missing properties
    function GetStoreActive: boolean;
  protected
    FHandle: HDBICur;  //cursor handle // to make it visible to PSQLUser
    procedure DefineProperties(Filer: TFiler); override;
    { IProviderSupport }
    {$IFNDEF FPC}
    procedure PSEndTransaction(Commit: Boolean); override;
{$IFDEF DELPHI_17}
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      var ResultSet: TDataSet): Integer; override;
{$ELSE}
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
{$ENDIF DELPHI_17}
    procedure PSGetAttributes(List: TList); override;
    function PSGetQuoteChar: string; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSStartTransaction; override;
    procedure PSReset; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    {$ENDIF}
  protected
    function  Engine : TPSQLEngine; Virtual; Abstract;
    {$IFNDEF FPC}
    procedure SetBlockReadSize(Value: Integer); override;
    procedure BlockReadNext; override;
    {$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActivateFilters;
    procedure AddFieldDesc(FieldDescs: TFLDDescList; var DescNo: Integer;
      var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
    procedure AllocCachedUpdateBuffers(Allocate: Boolean);
    procedure AllocKeyBuffers;
    function  AllocRecordBuffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; override;
    function  CachedUpdateCallBack(CBInfo: Pointer): CBRType;
    procedure CheckCachedUpdateMode;
    procedure CheckSetKeyMode;
    procedure ClearCalcFields(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}); override;
    procedure CloseCursor; override;
    procedure CreateFields; override;
    procedure CloseBlob(Field: TField); override;
    function  CreateExprFilter(const Expr: string;
      Options: TFilterOptions; Priority: Integer): HDBIFilter;
    function  CreateFuncFilter(FilterFunc: Pointer;
      Priority: Integer): HDBIFilter;
    function  CreateHandle: HDBICur; virtual;
    function  CreateLookupFilter(Fields: TList; const Values: Variant;
      Options: TLocateOptions; Priority: Integer): HDBIFilter;
    {$IFDEF DELPHI_17}
    procedure DataConvert(Field: TField; Source, Dest: TValueBuffer; ToNative: Boolean); override;
    {$ELSE}
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    {$ENDIF}
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF}); override;
    procedure DeactivateFilters;
    procedure DestroyHandle; virtual;
    procedure DestroyLookupCursor; virtual;
    function  FindRecord(Restart, GoForward: Boolean): Boolean; override;
    function  ForceUpdateCallback: Boolean;
    procedure FreeKeyBuffers;
    procedure FreeRecordBuffer(var Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}); override;
    procedure GetBookmarkData(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Data: Pointer); override;
    {$IFDEF DELPHI_17}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    {$ENDIF DELPHI_17}
    function  GetBookmarkFlag(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): TBookmarkFlag; override;
    function  GetRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InitRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalInitRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}); override;
    procedure InternalSetToRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}); override;
    function  GetCanModify: Boolean; override;
    {$IFNDEF FPC}
    function  GetFieldFullName(Field: TField): string; override;
    {$ENDIF}
    function  GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function  GetIndexField(Index: Integer): TField;
    function  GetIndexFieldCount: Integer;
    function  GetIsIndexField(Field: TField): Boolean; override;
    function  GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
    function  GetKeyExclusive: Boolean;
    function  GetKeyFieldCount: Integer;
    function  GetLookupCursor(const KeyFields: string; CaseInsensitive: Boolean): HDBICur; Virtual;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    function  GetRecordSize: integer; reintroduce; virtual; 
    {$IFNDEF FPC}
    function  GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    procedure GetObjectTypeNames(Fields: TFields);
    {$ENDIF}
    function  GetUpdatesPending: Boolean;
    {$IFNDEF FPC}
    function  GetUpdateRecordSet: TUpdateRecordTypes;
    {$ENDIF}
    function  InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;

    procedure InternalInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    function  IsCursorOpen: Boolean; override;
    function  LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    function LocateFilteredRecord(const KeyFields: string;
                                            const KeyValues: Variant;
                                            Options: TLocateOptions;
                                            SyncCursor: Boolean): Word;
    function  LocateNearestRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Word;
    procedure PostKeyBuffer(Commit: Boolean);
    procedure PrepareCursor; Virtual;
    function  ProcessUpdates(UpdCmd: DBIDelayedUpdCmd): Word;
    function  ResetCursorRange: Boolean;
    procedure SetBookmarkData(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Data: Pointer); override;
    {$IFDEF DELPHI_17}
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    {$ENDIF DELPHI_17}
    procedure SetBookmarkFlag(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Value: TBookmarkFlag); override;
    procedure SetCachedUpdates(Value: Boolean);
    function  SetCursorRange: Boolean;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    {$IFDEF DELPHI_17}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    {$ENDIF}
    procedure SetFilterData(const Text: string; Options: TFilterOptions);
    procedure SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetIndexField(Index: Integer; Value: TField);
    procedure SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
    procedure SetKeyExclusive(Value: Boolean);
    procedure SetKeyFieldCount(Value: Integer);
    procedure SetKeyFields(KeyIndex: TKeyIndex; const Values: array of const);
    procedure SetLinkRanges(MasterFields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF});
    {$IFNDEF FPC}
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); override;
    {$ENDIF}
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    {$IFNDEF FPC}
    procedure SetOnUpdateError(UpdateEvent: TUpdateErrorEvent);
    {$ENDIF}
    procedure SetOptions(const Value: TPSQLDatasetOptions); virtual;
    procedure SetRecNo(Value: Integer); override;
    procedure SetupCallBack(Value: Boolean);
    {$IFNDEF FPC}
    procedure SetUpdateRecordSet(RecordTypes: TUpdateRecordTypes);
    {$ENDIF}
    procedure SetUpdateObject(Value: TPSQLSQLUpdateObject);
    procedure SwitchToIndex(const IndexName, TagName: string);
    function  UpdateCallbackRequired: Boolean;
    procedure Disconnect; Virtual;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; virtual;
    function GetHandle: HDBICur;
    property DBFlags: TDBFlags read FDBFlags;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;
    property StmtHandle: HDBIStmt read GetStmtHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetLastInsertID(const FieldNum: integer): integer;
    function GetFieldTypeOID(const FieldNum: integer): cardinal;
    procedure ApplyUpdates;
    function  BookmarkValid(Bookmark: TBookmark): Boolean; override;
    procedure Cancel; override;
    procedure CancelUpdates;
    property  CacheBlobs: Boolean read FCacheBlobs write FCacheBlobs default TRUE;
    function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    procedure CommitUpdates;
    procedure FetchAll;
    procedure FlushBuffers;
    function GetCurrentRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): Boolean; override;
    {$IFNDEF FPC}
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    {$ENDIF}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; {$IFNDEF FPC}override;{$ENDIF}
    {$IFDEF DELPHI_17}
    function GetFieldData(FieldNo: Integer; Buffer: TValueBuffer): Boolean; override;
    function GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean; override;
    {$ENDIF}
    procedure GetIndexInfo;
    function  Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function  Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function  IsSequenced: Boolean; override;
    procedure Post; override;
    procedure RevertRecord;
    function  UpdateStatus: TUpdateStatus; override;
    function CheckOpen(Status: Word): Boolean;
    procedure CloseDatabase(Database: TPSQLDatabase);
    procedure GetDatabaseNames(List: TStrings);
    property DBHandle: HDBIDB read GetDBHandle;
    property Handle: HDBICur read GetHandle;
    property ExpIndex: Boolean read FExpIndex;
    property KeySize: integer read FKeySize;
    property UpdateObject: TPSQLSQLUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read GetUpdatesPending;
    {$IFNDEF FPC}
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordSet write SetUpdateRecordSet;
    {$ENDIF}
    procedure PopulateFieldsOrigin();
    procedure SortBy(FieldNames : string; Compare : TPSQLDatasetSortCompare); overload;
    procedure SortBy(FieldNames : string); overload;
    property SortFieldNames : string read GetSortFieldNames write SetSortFieldNames;
    property RecordSize: integer read GetRecordSize;
  published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh default FALSE;
    property Database: TPSQLDatabase read GetDatabase write SetDatabase;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default False;
    property AllowSequenced : Boolean read FAllowSequenced Write FAllowSequenced default False; //Added by Nicolas Ring
    property Filter;
    property Filtered;
    property FilterOptions;
    property OnFilterRecord;
    property Active stored GetStoreActive;
    property AutoCalcFields;
    {$IFNDEF FPC}
    property ObjectView default FALSE;
    {$ENDIF}
    property Options: TPSQLDatasetOptions read FOptions write SetOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    {$IFNDEF FPC}
    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write SetOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
    {$ENDIF}
  end;

//////////////////////////////////////////////////////////
//Class       : TPSQLTable
//Description : TPSQLTable class
//////////////////////////////////////////////////////////
  TTableType = (ttDefault, ttParadox, ttDBase, ttFoxPro, ttASCII);
  TIndexName = type string;

  TValCheckList = array of VCHKDesc;

  TPSQLTable = Class(TPSQLDataSet)
  Private
    FStoreDefs: Boolean;
    FIndexDefs: TIndexDefs;
    FMasterLink: TMasterDataLink;
    FDefaultIndex: Boolean;
    FExclusive: Boolean;
    FReadOnly: Boolean;
    FFieldsIndex: Boolean;
    FTableName: TFileName;
    FIndexName: TIndexName;
    FLookupHandle: HDBICur;
    FLookupKeyFields: string;
    FTableLevel: Integer;
    FLookupCaseIns: Boolean;
    FNativeTableName: DBITBLNAME;
    FLimit : Integer;
    FOffset: Integer;
    FShowSystemTables: boolean;
    FHasOIDs: boolean;
    FTableID: cardinal;
    FComment: string;
    FOwner: string;
    FTablespace: string;
    procedure SetLimit(const Value : Integer);
    function GetLimit: integer;
    procedure CheckMasterRange;
    procedure DecodeIndexDesc(const IndexDesc: IDXDesc;
      var Source, Name, FieldExpression, DescFields: string;
      var Options: TIndexOptions);
    function FieldDefsStored: Boolean;
    function GetExists: Boolean;
    function GetIndexFieldNames: string;
    function GetIndexName: string;
    procedure GetIndexParams(const IndexName: string; FieldsIndex: Boolean;
      var IndexedName, IndexTag: string);
    function GetMasterFields: string;
    function GetTableLevel: Integer;
    function IndexDefsStored: Boolean;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetDataSource(Value: TDataSource);
    procedure SetExclusive(Value: Boolean);
    procedure SetIndexDefs(Value: TIndexDefs);
    procedure SetIndex(const Value: string; FieldsIndex: Boolean);
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetMasterFields(const Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetTableName(const Value: TFileName);
    function GetTableName: TFileName;
    procedure UpdateRange;
    function GetBatchModify: Boolean;
    procedure SetBatchModify(const Value : Boolean);
    procedure SetShowSystemTables(const Value: boolean);
    function GetOffset: Integer;
    procedure SetOffset(const Value: Integer);
    procedure SetDummyBool(const Value: boolean);
    procedure SetDummyInt(const Value: cardinal);
    procedure SetDummyStr(const Value: string);
    function GetTableSpace: string;
  Protected
    { IProviderSupport }
    {$IFNDEF FPC}
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetKeyFields: string; override;
    function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}
    function CreateHandle: HDBICur; override;
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF}); override;
    {$IFNDEF FPC}
    procedure DefChanged(Sender: TObject); override;
    {$ENDIF}
    procedure DestroyHandle; override;
    procedure DestroyLookupCursor; override;
    procedure DoOnNewRecord; override;
    procedure EncodeFieldDesc(var FieldDesc: FLDDesc;
      const Name: string; DataType: TFieldType; Size, Precision: Integer);
    procedure EncodeIndexDesc(var IndexDesc: IDXDesc;
      const Name, FieldExpression: string; Options: TIndexOptions;
      const DescFields: string = '');
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetHandle(const IndexName, IndexTag: string): HDBICur;
    function GetLanguageDriverName: string;
    function GetLookupCursor(const KeyFields: string;
      CaseInsensitive: Boolean): HDBICur; override;
    procedure InitFieldDefs; override;
    function GetFileName: string;
    function GetTableType: TTableType;
    function NativeTableName: PAnsiChar;
    procedure PrepareCursor; override;
    procedure UpdateIndexDefs; override;
    procedure SetOptions(const Value: TPSQLDatasetOptions); override;
    property MasterLink: TMasterDataLink read FMasterLink;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  Engine : TPSQLEngine; override;
    function  CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; override;
    function  IsSequenced: Boolean; override;
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    procedure ApplyRange;
    procedure CancelRange;
    procedure CreateTable;
    procedure DeleteIndex(const Name: string);
    procedure EditKey;
    procedure EditRangeEnd;
    procedure EditRangeStart;
    procedure EmptyTable;
    function FindKey(const KeyValues: array of const): Boolean;
    procedure FindNearest(const KeyValues: array of const);
    {$IFNDEF FPC}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF}); override;
    {$ENDIF}
    procedure GetIndexNames(List: TStrings);
    procedure GotoCurrent(Table: TPSQLTable);
    function GotoKey: Boolean;
    procedure GotoNearest;
    Procedure LockTable(LockType: TPSQLLockType; NoWait: boolean);
    procedure SetKey;
    procedure SetRange(const StartValues, EndValues: array of const);
    procedure SetRangeEnd;
    procedure SetRangeStart;
    property Exists: Boolean read GetExists;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;
    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;
    property KeyFieldCount: Integer read GetKeyFieldCount write SetKeyFieldCount;
    property TableLevel: Integer read GetTableLevel write FTableLevel;
    property BatchModify : Boolean read GetBatchModify write SetBatchModify default False;
  published
    property DefaultIndex: Boolean read FDefaultIndex write FDefaultIndex default TRUE;
    property Exclusive: Boolean read FExclusive write SetExclusive default FALSE;
    property FieldDefs stored FieldDefsStored;
    property IndexDefs: TIndexDefs read FIndexDefs write SetIndexDefs stored IndexDefsStored;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default FALSE;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default FALSE;
    property ShowSystemTables: boolean read FShowSystemTables write SetShowSystemTables default False; 
    property TableName: TFileName read GetTableName write SetTableName;
    property UpdateMode;
    property UpdateObject;
    property Limit : Integer read GetLimit write SetLimit default 0;
    property Offset : Integer read GetOffset write SetOffset default 0;
    property Owner: string read FOwner write SetDummyStr stored False;
    property HasOIDs: boolean read FHasOIDs write SetDummyBool stored False;
    property Tablespace: string read GetTableSpace write SetDummyStr stored False;
    property TableID: cardinal read FTableID write SetDummyInt stored False;
    property Comment: string read FComment write SetDummyStr stored False;
    property SortFieldNames;
  end;


//////////////////////////////////////////////////////////
//Class       : TPSQLQuery
//Description : Component TPSQLQuery
//////////////////////////////////////////////////////////
    TPSQLQuery = Class(TPSQLDataSet)
    Private
      FSQL: TStrings;
      FPrepared: Boolean;
      FParams: TPSQLParams;
      FText: string;
      FDataLink: TDataLink;
      FLocal: Boolean;
      FRowsAffected: Integer;
      FUniDirectional: Boolean;
      FRequestLive: Boolean;
      FSQLBinary: PChar;
      FParamCheck: Boolean;
      FExecSQL: Boolean;
      FCheckRowsAffected: Boolean;
      function CreateCursor(GenHandle: Boolean): HDBICur;
      function GetQueryCursor(GenHandle: Boolean): HDBICur;
      function GetRowsAffected: Integer;
      procedure PrepareSQL(Value: PChar);
      procedure QueryChanged(Sender: TObject);
      procedure ReadBinaryData(Stream: TStream);
      procedure ReadParamData(Reader: TReader);
      procedure RefreshParams;
      procedure SetDataSource(Value: TDataSource);
      procedure SetQuery(Value: TStrings);
      function GetQuery:TStrings;
      procedure SetParamsList(Value: TPSQLParams);
      procedure SetParamsFromCursor;
      procedure SetPrepared(Value: Boolean);
      procedure SetPrepare(Value: Boolean);
      procedure WriteBinaryData(Stream: TStream);
      procedure WriteParamData(Writer: TWriter);
      procedure SetRequestLive(const Value : Boolean);
      function GetRequestLive : Boolean;
    protected
      { IProviderSupport }
    {$IFNDEF FPC}
      procedure PSExecute; override;
      function PSGetDefaultOrder: TIndexDef; override;
      function PSGetParams: TParams; override;
      function PSGetTableName: string; override;
      procedure PSSetCommandText(const CommandText: string); override;
      procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}
      function CreateHandle: HDBICur; override;
      procedure DefineProperties(Filer: TFiler); override;
      procedure Disconnect; override;
      function GetDataSource: TDataSource; override;
      function GetParamsCount: integer;
      function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
      procedure SetOptions(const Value: TPSQLDatasetOptions); override;
      procedure GetStatementHandle(SQLText: PChar); virtual;
      property DataLink: TDataLink read FDataLink;
    Public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function  Engine : TPSQLEngine; override;
      function  CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; override;
      function  IsSequenced: Boolean; override;
      procedure ExecSQL;
    {$IFNDEF FPC}
      procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    {$ENDIF}
      function ParamByName(const Value: string): TPSQLParam;
      procedure Prepare;
      procedure UnPrepare;
      property Prepared: Boolean read FPrepared write SetPrepare;
      property ParamCount: integer read GetParamsCount;
      property Local: Boolean read FLocal;
      property Text: string read FText;
      property RowsAffected: Integer read GetRowsAffected;
      property SQLBinary: PChar read FSQLBinary write FSQLBinary;
    published
      property DataSource: TDataSource read GetDataSource write SetDataSource;
      property ParamCheck: Boolean read FParamCheck write FParamCheck default TRUE;
      property RequestLive: Boolean read GetRequestLive write SetRequestLive default FALSE;
      property SQL: TStrings read GetQuery write SetQuery;
      property Params: TPSQLParams read FParams write SetParamsList stored FALSE;
      property UniDirectional: Boolean read FUniDirectional write FUniDirectional default FALSE;
      property UpdateMode;
      property UpdateObject;
      property SortFieldNames;
  end;


  TRecordChangeCompleteEvent = procedure(DataSet: TPSQLDataSet; const Reason: TUpdateKind) of object;

  { TPSQLUpdateSQL }
  TPSQLUpdateSQL = Class(TPSQLSQLUpdateObject)
  Private
    FAbout : TPSQLDACAbout;
    FDataSet: TPSQLDataSet;
    FQueries: array[TUpdateKind] of TPSQLQuery;
    FSQLText: array[TUpdateKind] of TStrings;
    FRecordChangeCompleteEvent: TRecordChangeCompleteEvent;
    function GetQuery(UpdateKind: TUpdateKind): TPSQLQuery;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  Protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; override;
    function GetQueryClass : TPSQLQueryClass;
    function GetDataSet: TPSQLDataSet; override;
    procedure SetDataSet(ADataSet: TPSQLDataSet); override;
    procedure SQLChanged(Sender: TObject);
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind); override;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    property DataSet;
    property Query[UpdateKind: TUpdateKind]: TPSQLQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  Published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
    property OnRecordChangeComplete: TRecordChangeCompleteEvent read FRecordChangeCompleteEvent write FRecordChangeCompleteEvent;
  end;

  { TPSQLBlobStream }
  TPSQLBlobStream = Class(TStream)
  private
    FField: TBlobField;
    FDataSet: TPSQLDataSet;
    FBuffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
    FMode: TBlobStreamMode;
    FFieldNo: Integer;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FBlobData: TBlobData;
    FCached: Boolean;
    FCacheSize: Longint;
    function GetBlobSize: Longint;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Engine : TPSQLEngine;
    function PositionDataset: Boolean;
    {$IFDEF DELPHI_17}
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    {$ENDIF DELPHI_17}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;


  TParamBindMode = (pbByName, pbByNumber);

  TSPParamDescList = array of SPParamDesc;

  TPSQLStoredProc = class(TPSQLDataSet)
  private
    FParams: TPSQLParams;
    FNeedRefreshParams : boolean;
    FOverload: cardinal;
    FProcName: string;
    FBindMode: TParamBindMode;
    function GetParamsCount: integer;
    procedure SetOverload(const Value: cardinal);
    procedure SetProcName(const Value: string);
  protected
    { IProviderSupport }
    {$IFNDEF FPC}
    procedure PSExecute; override;
    function PSGetTableName: string; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}
    function CreateHandle: HDBICur;override;
    function CreateCursor(IsExecProc : boolean): HDBICur;
    procedure DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF}); override;
    procedure CloseCursor; override;
    procedure SetProcedureName(const Value: string);
    function GetParamsList: TPSQLParams;
    procedure SetParamsList(const Value: TPSQLParams);
    function GetCanModify: Boolean; override;
	public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; override;
    function Engine : TPSQLEngine; override;
    function DescriptionsAvailable: Boolean;
    function ParamByName(const Value: string): TPSQLParam;

    procedure ExecProc;
    procedure RefreshParams;
    procedure SetNeedRefreshParams;

    property ParamsCount : integer read GetParamsCount;
  published
    property StoredProcName: string read FProcName write SetProcName;
    property Overload: cardinal read FOverload write SetOverload default 0;
    property Params: TPSQLParams read GetParamsList write SetParamsList;
    property ParamBindMode: TParamBindMode read FBindMode write FBindMode default pbByName;
	end;

procedure Check(Engine : TPSQLEngine; Status: Word);
procedure NoticeProcessor(arg: Pointer; mes: PAnsiChar); cdecl;

var
   DBList : TList;

implementation

uses
  DBConsts,
  {$IFDEF DELPHI_10}DBClient, {$ENDIF}
  PSQLDirectQuery, Math, PSQLFields, PSQLNotify;

{$R DB.DCR}

//NoticeProcessor callback function
procedure NoticeProcessor(arg: Pointer; mes: PAnsiChar);
var s:string;
begin
 if Assigned(TPSQLDatabase(Arg).FOnNotice) then
  begin
   if TPSQLDatabase(Arg).IsUnicodeUsed then
     S := UTF8ToString(Mes)
   else
     S := string(Mes);
   TPSQLDatabase(Arg).FOnNotice(TPSQLDatabase(Arg), S);
  end;
end;

{ TPSQLQueryDataLink }
type
  TPSQLQueryDataLink =  Class(TDetailDataLink)
    Private
      FQuery: TPSQLQuery;
    Protected
      procedure ActiveChanged; override;
      procedure RecordChanged(Field: TField); override;
      function GetDetailDataSet: TDataSet; override;
      procedure CheckBrowseMode; override;
    Public
      constructor Create(AQuery: TPSQLQuery);
  end;

procedure TDbiError(Engine : TPSQLEngine; ErrorCode: Word);
begin
  Raise EPSQLDatabaseError.Create(Engine, ErrorCode);
end;

procedure Check(Engine : TPSQLEngine; Status: Word);
begin
  if Status <> 0 then TDbiError(Engine, Status);
end;

function GetIntProp(Engine : TPSQLEngine; const Handle: Pointer; PropName: Integer): Integer;
Var
  Length : integer;
  Value  : Integer;
begin
  Value := 0;
  if (Engine.GetEngProp(HDBIObj(Handle), PropName, @Value, SizeOf(Value), Length) = DBIERR_NONE) then
    Result := Value else
    Result := 0;
end;

function SetBoolProp(Engine : TPSQLEngine; const Handle: Pointer; PropName: Integer; Value: Boolean) : Boolean;
begin
  Result := Engine.SetEngProp(HDBIObj(Handle), PropName, Abs(Integer(Value))) = DBIERR_NONE;
end;

{ EPSQLDatabaseError }
constructor EPSQLDatabaseError.Create(Engine : TPSQLEngine; ErrorCode : Word);

  function GetErrorString: string;
  var
    Msg1 : string;
    Msg2 : string;
    Err  : Integer;
  begin
    Msg1 := Engine.MessageStatus;
    Err := Engine.Status;
    if (Msg1 <> '') and (Err >0) then  Msg1 := Format('PostgreSQL Error Code: (%s)',[IntToStr(Err)])+#13#10+Msg1 else
    begin
       Msg2 := GetBDEErrorMessage(ErrorCode);
       Msg1 := Format('DBI Error Code: (%s)'+#13#10+'%s '+#13#10+'%s',[IntToStr(ErrorCode),Msg1,Msg2]);
    end;
    Result := Msg1
  end;

begin
  //FreeTimer(TRUE);
  FErrorCode := ErrorCode;
  FErrorPos                :=engine.errorpos;
  FErrorContext            :=engine.errorcontext;
  FErrorseverity           :=engine.Errorseverity;
  FErrorsqlstate           :=engine.Errorsqlstate;
  FErrorprimary            :=engine.Errorprimary;
  FErrordetail             :=engine.Errordetail;
  FErrorhint               :=engine.Errorhint;
  FErrorinternalpos        :=engine.Errorinternalpos;
  FErrorinternalquery      :=engine.Errorinternalquery;
  FErrorsourcefile         :=engine.Errorsourcefile;
  FErrorsourceline         :=engine.Errorsourceline;
  FErrorsourcefunc         :=engine.Errorsourcefunc;
  Message := GetErrorString;
  if Message <> '' then
     Message := Copy(Message, 1, Length(Message)) else
     Message := Format('PSQLDAC Interface Error: (%d)',[ErrorCode]);
end;

destructor EPSQLDatabaseError.Destroy;
begin
  Inherited Destroy;
end;


{ TPSQLDatabase }
procedure TPSQLDatabase.InitEngine;
begin
  try
    if FEngine = nil then FEngine := TPSQLEngine.Create(nil, nil);
  except
    raise EDatabaseError.Create('The Engine is not initialized');
  end;
end;

procedure TPSQLDatabase.AddDatabase(Value : TPSQLDatabase);
begin
   DBList.Add(Value);
end;

procedure TPSQLDatabase.RemoveDatabase(Value : TPSQLDatabase);
begin
   while DataSetCount <> 0  do
      TPSQLDataSet(DataSets[DataSetCount - 1]).FDatabase := nil;
   DBList.Remove(Value);

  while FDirectQueryList.Count > 0 do
    TPSQLDirectQuery(FDirectQueryList[FDirectQueryList.Count - 1]).Database := nil;
end;

procedure TPSQLDatabase.WriteState(Writer: TWriter);
var OldPwd: string;
begin
  if not (ddoStorePassword in FDesignOptions) then
  begin
   OldPwd := GetUserPassword();
   SetUserPassword('');
  end;

  inherited;

  if not (ddoStorePassword in FDesignOptions) then
   SetUserPassword(OldPwd);
end;

constructor TPSQLDatabase.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FParams := TStringList.Create;
  TStringList(FParams).OnChanging := ParamsChanging;
  FKeepConnection := TRUE;
  FOEMConvert := False;
  SetConnectionTimeout(15);
  SetServerPort(PSQL_PORT);
  SetSSLMode(sslPrefer);
  FTransIsolation := tiReadCommitted;
  AddDatabase(Self);
  FNotifyList := TList.Create;
  FDirectQueryList := TList.Create;
  FCheckIfActiveOnParamChange := True; //SSH Tunneling stuff
  SetConnectionTimeout(15);
  FDatabaseID := InvalidOid;
  FDesignOptions := [ddoStoreConnected, ddoStorePassword];
  FErrorVerbosity := evDEFAULT;
end;

destructor TPSQLDatabase.Destroy;
begin
  Destroying;
  Close;
  RemoveDatabase(Self);
  FEngine.Free;
  FNotifyList.Free;
  FDirectQueryList.Free;
  FParams.Free;
  FStmtList.Free;
  inherited Destroy;
end;


procedure TPSQLDatabase.ApplyUpdates(const DataSets: array of TPSQLDataSet);
var
  I  : Integer;
  DS : TPSQLDataSet;
begin
  StartTransaction;
  try
    for I := 0 to High(DataSets) do
    begin
      DS := DataSets[I];
      if (DS.Database <> Self) then
        DatabaseError(Format(SUpdateWrongDB, [DS.Name, Name]));
      DataSets[I].ApplyUpdates;
    end;
    Commit;
  except
    Rollback;
    raise;
  end;
  for I := 0 to High(DataSets) do DataSets[I].CommitUpdates;
end;

type
  PStmtInfo = ^TStmtInfo;
  TStmtInfo = packed record
    HashCode: Integer;
    StmtHandle: HDBIStmt;
    SQLText: string;
  end;

procedure TPSQLDatabase.ClearStatements;
var
  i: Integer;
begin
  if Assigned(FStmtList) then
  begin
    for i := 0 to FStmtList.Count - 1 do
    begin
      Engine.QFree(PStmtInfo(FStmtList[i]).StmtHandle);
      Dispose(PStmtInfo(FStmtList[i]));
    end;
    FStmtList.Clear;
  end;
end;                             

function TPSQLDatabase.Execute(const SQL: string; Params: TParams = nil;
  Cache: Boolean = FALSE; Cursor: phDBICur = nil): Integer;

  function GetStmtInfo(SQL: PChar): PStmtInfo;

    function GetHashCode(Str: PChar): Integer;
    var
      Off, Len, Skip, I: Integer;
    begin
      Result := 0;
      Off := 1;
      Len := StrLen(Str);
      if Len < 16 then
        for I := (Len - 1) downto 0 do
        begin
          Result := (Result * 37) + Ord(Str[Off]);
          Inc(Off);
        end
      else
      begin
        { Only sample some characters }
        Skip := Len div 8;
        I := Len - 1;
        while I >= 0 do
        begin
          Result := (Result * 39) + Ord(Str[Off]);
          Dec(I, Skip);
          Inc(Off, Skip);
        end;
      end;
    end;

  var
    HashCode, i: Integer;
    Info: PStmtInfo;
  begin
    if not Assigned(FStmtList) then
      FStmtList := TList.Create;
    Result := nil;
    HashCode := GetHashCode(SQL);
    for i := 0 to FStmtList.Count - 1 do
    begin
      Info := PStmtInfo(FStmtList[i]);
      if (Info.HashCode = HashCode) and
         AnsiSameText(PChar(Info.SQLText), SQL) then
      begin
        Result := Info;
        break;
      end;
    end;
    if not Assigned(Result) then
    begin
      New(Result);
      FStmtList.Add(Result);
      FillChar(Result^, SizeOf(Result^), 0);
      Result.HashCode := HashCode;
    end;
  end;

  function GetStatementHandle: HDBIStmt;
  var
    Info: PStmtInfo;
    Status: Word;
  begin
    Info   := nil;
    Result := nil;
    if Cache then
    begin
      Info := GetStmtInfo(PChar(SQL));
      Result := Info.StmtHandle;
    end;
    if not Assigned(Result) then
     begin
      Check(Engine, Engine.QAlloc(Handle, Result));
      if Cursor <> nil then
        Check(Engine, Engine.SetEngProp(hDbiObj(Result), stmtLIVENESS, Ord(wantCanned)));
      SetBoolProp(Engine, Result, stmtUNIDIRECTIONAL, TRUE);
      while TRUE do
      begin
        Status := Engine.QPrepare(Result, SQL);
        case Status of
          DBIERR_NONE: break;
          DBIERR_NOTSUFFTABLERIGHTS: TDbiError(Engine, Status);
        end;
      end;
      if Assigned(Info) then
      begin
        Info.SQLText    := SQL;
        Info.StmtHandle := Result;
      end;
     end;
  end;

var
  StmtHandle : HDBIStmt;
  Len        : integer;
begin
  StmtHandle := nil;
  Result := 0;
  Open;
  if Assigned(Params) and (Params.Count > 0) then
    begin
      StmtHandle := GetStatementHandle;
      try
        Check(Engine, Engine.QuerySetParams(StmtHandle, Params, SQL));
        Check(Engine, Engine.QExec(StmtHandle, Cursor, Result));
      finally
        if not Cache then  Engine.QFree(StmtHandle);
      end;
    end
  else
    Check(Engine, Engine.QExecDirect(Handle, SQL, Cursor, Result));
  if Result = 0 then
     if (Cursor = nil) and (Engine.GetEngProp(hDBIObj(StmtHandle), stmtROWCOUNT,@Result, SizeOf(Result), Len) <> 0) then
        Result := 0;
end;

procedure TPSQLDatabase.CheckActive;
begin
  if FHandle = nil then DatabaseError(SDatabaseClosed);
end;

procedure TPSQLDatabase.CheckInactive;
begin
  if FHandle <> nil then
     if csDesigning in ComponentState then
        Close else
        DatabaseError(SDatabaseOpen, Self);
end;

procedure TPSQLDatabase.CloseDatabaseHandle;
begin
   Engine.CloseDatabase(FHandle);
end;

procedure TPSQLDatabase.CloseDatabase(Database: TPSQLDatabase);
begin
  with Database do
  begin
    if FRefCount <> 0 then Dec(FRefCount);
    if (FRefCount = 0) and not KeepConnection then Close;
  end;
end;

procedure TPSQLDatabase.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('UseSSL', SetUseSSL, nil, False); //missing
end;

function TPSQLDatabase.GetSSLOption(Index: integer): string;
begin
  Result := FParams.Values[SSLOpts[Index]];
end;

procedure TPSQLDatabase.SetSSLOption(Index: integer; Value: string);
begin
  FParams.Values[SSLOpts[Index]] := Value;
end;

procedure TPSQLDatabase.DoDisconnect;
begin
  if FHandle <> nil then
  begin
    ClearStatements;
    CloseDataSets;
    if not FAcquiredHandle then
      CloseDatabaseHandle else
      FAcquiredHandle := FALSE;
    FHandle := nil;
    FRefCount := 0;
    FDatabaseID := 0;
    FIsTemplate := False;
    FTablespace := '';
    FComment := '';
    FServerVersion := '';
    FOwner := '';
  end;
end;

procedure TPSQLDatabase.CloseDataSets;
begin
  while DataSetCount <> 0  do
    TPSQLDataSet(DataSets[DataSetCount - 1]).Disconnect;
end;

procedure TPSQLDatabase.Commit;
begin
  CheckActive;
  EndTransaction(xendCOMMIT);
end;

procedure TPSQLDatabase.Rollback;
begin
  CheckActive;
  EndTransaction(xendABORT);
end;

procedure TPSQLDatabase.StartTransaction;
var
  TransHandle:  HDBIXAct;
begin
  CheckActive;
  Check(Engine, Engine.BeginTran(FHandle, EXILType(FTransIsolation),TransHandle));
end;

procedure TPSQLDatabase.EndTransaction(TransEnd : EXEnd);
begin
  Check(Engine, Engine.EndTran(FHandle, nil, TransEnd));
end;

procedure TPSQLDatabase.Savepoint(const Name: string);
begin
  CheckActive();
  Execute('SAVEPOINT ' + Name);
end;

procedure TPSQLDatabase.ReleaseSavepoint(const Name: string);
begin
  CheckActive();
  Execute('RELEASE SAVEPOINT ' + Name);
end;

procedure TPSQLDatabase.RollbackToSavepoint(const Name: string);
begin
  CheckActive();
  Execute('ROLLBACK TO SAVEPOINT ' + Name);
end;

function TPSQLDatabase.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

function TPSQLDatabase.GetConnectionTimeout: cardinal;
begin
  Result := StrToUIntDef(FParams.Values['connect_timeout'], 15);
end;

function TPSQLDatabase.GetStoreConnected: Boolean;
begin
  Result := Connected and
   (ddoStoreConnected in FDesignOptions);
end;

function TPSQLDatabase.GetDataSet(Index : Integer) : TPSQLDataSet;
begin
  Result := inherited GetDataSet(Index) as TPSQLDataSet;
end;

procedure TPSQLDatabase.SetDatabaseFlags;
var
  Length: integer;
  Buffer: DBINAME;
begin
  Check(Engine, Engine.GetEngProp(HDBIOBJ(FHandle), dbDATABASETYPE, @Buffer, SizeOf(Buffer), Length));
  FPseudoIndexes := FALSE;
end;

function TPSQLDatabase.GetInTransaction: Boolean;
var
  TranInfo : XInfo;
begin
  Result := (Handle <> nil) and
            (Engine.GetTranInfo(Handle, nil, @TranInfo) = DBIERR_NONE) and
            ( (TranInfo.exState = xsActive));
end;

function TPSQLDatabase.GetServerPort: Cardinal;
begin
  Result := StrToUIntDef(FParams.Values['port'], PSQL_PORT);
end;

function TPSQLDatabase.GetServerVersionAsInt: integer;
begin
 if Assigned(Handle) then
   Result := TNativeConnect(Handle).GetserverVersionAsInt
 else
   Result := 0;
end;

function TPSQLDatabase.GetTransactionStatus: TTransactionStatusType;
begin
 if Handle <> nil then
   Engine.GetTranStatus(Handle,Result)
 else
   Result := trstUnknown;
end;

procedure TPSQLDatabase.Loaded;

  procedure ChangeOldParameter(const OldName, NewName: string);
  var I: integer;
      V: string;
  begin
    I := FParams.IndexOfName(OldName);
    if I = -1 then Exit;
    V := Copy(FParams[I], Length(OldName) + 2, MaxInt);
    FParams.Delete(I);
    FParams.Values[NewName] := V;
  end;

begin
  inherited Loaded;
  if not StreamedConnected then InitEngine;
  ChangeOldParameter('UID', 'user');
  ChangeOldParameter('PWD', 'password');
  ChangeOldParameter('DatabaseName', 'dbname');
  ChangeOldParameter('ConnectionTimeout', 'connect_timeout');
  ChangeOldParameter('Port', 'port');
  ChangeOldParameter('SSLMode', 'sslmode');
  ChangeOldParameter('Host', 'host');
  if FParams.Values['host'] > '' then
    SetHost(FParams.Values['host']); //make sure correct keyword used depending on IP or host name
end;

procedure TPSQLDatabase.Notification(AComponent : TComponent; Operation : TOperation);
begin
  Inherited Notification(AComponent, Operation);
end;

procedure TPSQLDatabase.Login(LoginParams: TStrings);
begin
  if Assigned(FOnLogin) then
    FOnLogin(Self, LoginParams)
  else
    DatabaseErrorFmt(SLoginError, [DatabaseName]);
end;

procedure TPSQLDatabase.DoConnect;
const
  OpenModes: array[Boolean] of DbiOpenMode = (dbiReadWrite, dbiReadOnly);
  ShareModes: array[Boolean] of DbiShareMode = (dbiOpenShared, dbiOpenExcl);
begin
  if FHandle = nil then
  begin
    InitEngine;
    if LoginPrompt then Login(FParams);
    OEMConv := FOEMConvert;
    Check(Engine, Engine.OpenDatabase(FParams, FUseSingleLineConnInfo, FHandle));
    Check(Engine, Engine.GetServerVersion(FHandle, FServerVersion));
    Check(Engine, Engine.SetCharacterSet(FHandle, FCharSet));
    Check(Engine, Engine.SetCommandTimeout(FHandle, FCommandTimeout));
    if FErrorVerbosity <> evDEFAULT then
      Check(Engine, Engine.SetErrorVerbosity(FHandle, FErrorVerbosity));
    if Assigned(FHandle) then
      PQSetNoticeProcessor(TNativeConnect(FHandle).Handle, NoticeProcessor, Self);
    SetBoolProp(Engine, FHandle, dbUSESCHEMAFILE,        TRUE);
    SetBoolProp(Engine, FHandle, dbPARAMFMTQMARK,        TRUE);
    SetBoolProp(Engine, FHandle, dbCOMPRESSARRAYFLDDESC, TRUE);
    SetDatabaseFlags;
  end;
end;

procedure TPSQLDatabase.ParamsChanging(Sender: TObject);
begin
 if FCheckIfActiveOnParamChange then CheckInactive; //SSH tunneling
end;

function TPSQLDatabase.Ping(ConnectionParams: TStrings): TPingStatus;
begin
  Check(Engine, Engine.Ping(ConnectionParams, Result));
end;

function TPSQLDatabase.Ping: TPingStatus;
begin
  Check(Engine, Engine.Ping(FParams, Result));
end;

procedure TPSQLDatabase.SetDatabaseName(const Value : string);
begin
  if not (csReading in ComponentState) then
    if FCheckIfActiveOnParamChange then
      CheckInactive; //SSH tunneling
  FParams.Values['dbname'] := Value;
end;

procedure TPSQLDatabase.SetServerPort(const Value : Cardinal);
begin
  if not (csReading in ComponentState) then
    if FCheckIfActiveOnParamChange then
      CheckInactive; //SSH tunneling
  FParams.Values['port'] := UIntToStr(Value);
end;

procedure TPSQLDatabase.SetConnectionTimeout(const Value : Cardinal);
begin
   if not (csReading in ComponentState) then
     if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
   FParams.Values['connect_timeout'] := IntToStr(Value);
end;

procedure TPSQLDatabase.SetCommandTimeout(const Value : Cardinal);
begin
  if FCommandTimeout <> Value then
    begin
      FCommandTimeout := Value;
      if Connected then
       Check(Engine, Engine.SetCommandTimeout(FHandle,FCommandTimeout));
    end;
end;

procedure TPSQLDatabase.SetHost(const Value : string);
begin
    if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
    if IsValidIP(Value) then
     begin
      FParams.Values['hostaddr'] := Value;
      FParams.Values['host'] := '';
     end
    else
     begin
      FParams.Values['hostaddr'] := '';
      FParams.Values['host'] := Value;
     end;
end;

procedure TPSQLDatabase.SetUseSSL(Reader: TReader);
begin
 Reader.ReadBoolean(); //just ignore old property
end;

procedure TPSQLDatabase.SetUserName(const Value : string);
begin
  if FCheckIfActiveOnParamChange then
    CheckInactive; //SSH tunneling
  FParams.Values['user'] := Value;
end;

procedure TPSQLDatabase.SetUserPassword(const Value : string);
begin
  if FCheckIfActiveOnParamChange then
    CheckInactive; //SSH tunneling

  FParams.Values['password'] := Value;
end;

procedure TPSQLDatabase.SetHandle(Value: HDBIDB);
begin
  if Connected then Close;
  if Value <> nil then
  begin
    FHandle := Value;
    SetDatabaseFlags;
    FAcquiredHandle := TRUE;
  end;
end;

procedure TPSQLDatabase.SetKeepConnection(Value: Boolean);
begin
  if FKeepConnection <> Value  then
    FKeepConnection := Value;
end;

procedure TPSQLDatabase.SetParams(Value: TStrings);
begin
  if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
  FParams.Assign(Value);
end;

procedure TPSQLDatabase.SetDummyStr(Value: string);
begin
//dummy method for published
end;

procedure TPSQLDatabase.SetDummyBool(Value: boolean);
begin
//dummy method for published
end;

procedure TPSQLDatabase.SetDummyInt(Value: cardinal);
begin
//dummy method for published
end;

procedure TPSQLDatabase.SetExclusive(Value: Boolean);
begin
  if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
  FExclusive := Value;
end;

procedure TPSQLDatabase.SetReadOnly(Value: Boolean);
begin
 if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
  FReadOnly := Value;
end;

function TPSQLDatabase.Engine : TPSQLEngine;
begin
  Result := FEngine;
end;

procedure TPSQLDatabase.GetStoredProcNames(Pattern: string; List: TStrings);
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    Check(Engine, Engine.OpenStoredProcList(Handle, Pattern, List));
  finally
    List.EndUpdate;
  end;
end;

procedure TPSQLDatabase.GetTableNames(Pattern: string; SystemTables: Boolean; List: TStrings);
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    Check(Engine, Engine.OpenTableList(Handle, Pattern, SystemTables, List));
  finally
    List.EndUpdate;
  end;
end;

procedure TPSQLDatabase.GetSchemaNames(Pattern: string; SystemSchemas: Boolean; List: TStrings);
begin
  if not Assigned(List) then Exit;
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    Check(Engine, Engine.OpenSchemaList(Handle, Pattern, SystemSchemas, List));
  finally
    List.EndUpdate;
  end;
end;

function TPSQLDatabase.GetUsrName: string;
begin
  Result := FParams.Values['user'];
end;

procedure TPSQLDatabase.GetUserNames(Pattern: string; List: TStrings);
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    Check(Engine, Engine.OpenUserList(Handle,Pattern, List));
  finally
    List.EndUpdate;
  end;
end;

function TPSQLDatabase.GetUserPassword: string;
begin
  Result := FParams.Values['password'];
end;

procedure TPSQLDatabase.GetCharsets(List: TStrings);
begin
 List.BeginUpdate;
 try
   Check(Engine,Engine.GetCharacterSets(Handle,List));
 finally
   List.EndUpdate;
 end;
end;

procedure TPSQLDatabase.SetCharSet(CharSet: string);
begin
  if FCharSet <> CharSet then
  begin
   FCharSet := CharSet;
   if Connected then
     Engine.SetCharacterSet(Handle,CharSet)
  end;
end;

procedure TPSQLDatabase.GetDatabases(Pattern: string; List : TStrings);
var
   OldConn : Boolean;
   OldDbName : string;
begin
   OldConn := Connected;
   OldDbName := '';
   if not Connected then
   begin
      OldDbName := DatabaseName;
      DatabaseName := 'template1';
   end;
   if Handle = nil then Connected := True;
   if Pattern <> '' then
      Check(Engine, Engine.GetDatabases(Handle, Pattern, List)) else
      Check(Engine, Engine.GetDatabases(Handle, '', List));
   Connected := OldConn;
   if not Connected then
     DatabaseName := OldDbName;
end;

function TPSQLDatabase.GetNotifyItem(Index: Integer): TObject;
begin
  Result := FNotifyList[Index];
end;

function TPSQLDatabase.GetNotifyCount: Integer;
begin
  Result := FNotifyList.Count;
end;

procedure TPSQLDatabase.AddNotify(AItem: TObject);
begin
  if FNotifyList.IndexOf(AItem) >= 0 then Exit;
  FNotifyList.Add(AItem);
end;

procedure TPSQLDatabase.RemoveNotify(AItem: TObject);
var
  I: Integer;
begin
  I := FNotifyList.IndexOf(AItem);
  if I >= 0 then
  begin
     try
       TPSQLNotify(FNotifyList[I]).CloseNotify;
     finally
       FNotifyList.Delete(I);
     end;
  end;
end;

procedure TPSQLDatabase.CloseNotify;
var
  I: Integer;
begin
  for I := 0 to FNotifyList.Count-1 do
    try
      TPSQLNotify(FNotifyList[I]).CloseNotify;
    except
    end;
end;

function TPSQLDatabase.GetBackendPID:Integer;
begin
  Result := InvalidOID;
  if Connected then
    Engine.GetBackendPID(Handle, Result);
end;


//////////////////////////////////////////////////////////
//Constructor : TPSQLBDECallBack.Create
//Description : TPSQLBDECallBack
//////////////////////////////////////////////////////////
//Input       : Engine: TPSQLEngine
//              AOwner: TObject
//              Handle: hDBICur
//              CBType: CBType
//              CBBuf: Pointer
//              CBBufSize: Integer
//              CallbackEvent: TPSQLBDECallbackEvent
//              Chain: Boolean
//////////////////////////////////////////////////////////
constructor TPSQLBDECallBack.Create(Engine : TPSQLEngine; AOwner: TObject; Handle: hDBICur; CBType: CBType;
  CBBuf: Pointer; CBBufSize: Integer; CallbackEvent: TPSQLBDECallbackEvent;
  Chain: Boolean);
begin
  FEngine := Engine;
  FOwner  := AOwner;
  FHandle := Handle;
  FCBType := CBType;
  FCallbackEvent := CallbackEvent;
end;

//////////////////////////////////////////////////////////
//Destructor  : TPSQLBDECallBack.Destroy
//Description : Destroy TPSQLBDECallback
//////////////////////////////////////////////////////////
destructor TPSQLBDECallBack.Destroy;
begin
  Inherited Destroy;
end;

//////////////////////////////////////////////////////////
//function    : TPSQLBDECallBack.Invoke
//Description : Invoke method TPSQLBDECallback
//////////////////////////////////////////////////////////
//Input       : CallType: CBType
//              CBInfo: Pointer
//Output      : Result: CBRType
//////////////////////////////////////////////////////////
function TPSQLBDECallBack.Invoke(CallType: CBType; CBInfo: Pointer): CBRType;
begin
  if (CallType = FCBType) then
    Result := FCallbackEvent(CBInfo)
  else
    Result := cbrUSEDEF;
  if Assigned(FOldCBFunc) then
    Result := FOldCBFunc(CallType, FOldCBData, CBInfo);
end;


procedure TPSQLDatabase.GetTablespaces(Pattern: string; List: TStrings);
begin
  if not Assigned(List) then Exit;
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    Check(Engine, Engine.OpenTablespaceList(Handle, Pattern, List));
  finally
    List.EndUpdate;
  end;
end;

procedure TPSQLDatabase.SetErrorVerbosity(const Value: TErrorVerbosity);
begin
  if FErrorVerbosity <> Value then
   begin
     FErrorVerbosity := Value;
     if Connected then
       Engine.SetErrorVerbosity(Handle, Value)
   end;
end;

procedure TPSQLDatabase.SetSSLMode(const Value: TSSLMode);
begin
  if FSSLMode <> Value then
   begin
     if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
     FSSLMode := Value;
     FParams.Values['sslmode'] := SSLConsts[FSSLMode];
   end;
end;

procedure TPSQLDatabase.Reset;
begin
  CheckActive;
  Check(Engine, Engine.Reset(FHandle));
end;

procedure TPSQLDatabase.CancelBackend(PID: Integer);
begin
  Check(Engine,Engine.CancelBackend(Handle,PID));
end;

procedure TPSQLDatabase.RegisterDirectQuery(aDirectQuery: TObject);
begin
  FDirectQueryList.Add(aDirectQuery);
end;

procedure TPSQLDatabase.UnregisterDirectQuery(aDirectQuery: TObject);
var
  i : integer;
begin
  i := FDirectQueryList.IndexOf(aDirectQuery);
  if i <> -1 then
    FDirectQueryList.Delete(i);
end;

function TPSQLDatabase.SelectString(aSQL: string; var IsOk: boolean;
  aFieldNumber: integer): string;
begin
  DoConnect;
  Check(Engine, Engine.SelectStringDirect(FHandle, PChar(aSQL), IsOk, Result, aFieldNumber));
end;

function TPSQLDatabase.SelectString(aSQL: string; var IsOk: boolean;
  aFieldName: string): string;
begin
  DoConnect;
  Check(Engine, Engine.SelectStringDirect(FHandle, PChar(aSQL), IsOk, Result, aFieldName));
end;

procedure TPSQLDatabase.SelectStrings(aSQL: string; aList: TStrings; aFieldName: string);
begin
  DoConnect;
  Check(Engine, Engine.SelectStringsDirect(FHandle, PChar(aSQL), aList, aFieldName));
end;

procedure TPSQLDatabase.SelectStrings(aSQL: string; aList: TStrings; aFieldNumber: integer = 0);
begin
  DoConnect;
  Check(Engine, Engine.SelectStringsDirect(FHandle, PChar(aSQL), aList, aFieldNumber));
end;

function TPSQLDatabase.SelectStringDef(aSQL, aDefaultValue: string;
  aFieldNumber: integer): string;
var
  IsOk : boolean;
begin
  Result := SelectString(aSQL, IsOk, aFieldNumber);
  if not IsOk then
    Result := aDefaultValue;
end;

function TPSQLDatabase.SelectStringDef(aSQL, aDefaultValue,
  aFieldName: string): string;
var
  IsOk : boolean;
begin
  Result := SelectString(aSQL, IsOk, aFieldName);
  if not IsOk then
    Result := aDefaultValue;
end;

procedure TPSQLDatabase.FillAddonInfo;
begin
  if not Connected or (FDatabaseID > 0) then Exit;
  Engine.GetDBProps(FHandle,GetDatabaseName(), FOwner, FTablespace,
        FIsTemplate,FDatabaseId, FComment);
end;

function TPSQLDatabase.GetDatabaseComment: string;
begin
 FillAddonInfo;
 Result := FComment;
end;

function TPSQLDatabase.GetDatabaseID: cardinal;
begin
 FillAddonInfo;
 Result := FDatabaseID;
end;

function TPSQLDatabase.GetDatabaseName: string;
begin
  Result := FParams.Values['dbname'];
end;

function TPSQLDatabase.GetIsSSLUsed: Boolean;
begin
 if Assigned(FHandle) then
  Result := TNativeConnect(FHandle).IsSSLUsed
 else
  Result := False;
end;

function TPSQLDatabase.GetIsTemplate: boolean;
begin
 FillAddonInfo;
 Result := FIsTemplate;
end;

function TPSQLDatabase.GetIsUnicodeUsed: Boolean;
begin
 if Assigned(FHandle) then
  Result := TNativeConnect(FHandle).IsUnicodeUsed
 else
  Result := False;
end;

function TPSQLDatabase.GetDbOwner: string;
begin
 FillAddonInfo;
 Result := FOwner;
end;

function TPSQLDatabase.GetHost: string;
begin
  Result := FParams.Values['hostaddr'];
  if Result = '' then
    Result := FParams.Values['host'];
end;

function TPSQLDatabase.GetTablespace: string;
begin
 FillAddonInfo;
 Result := FTablespace;
end;

{ TPSQLDataSet }
procedure TPSQLDataSet.ReadByteaOpt(Reader: TReader); //deal with old missing properties
begin
 if Reader.ReadBoolean then Include(FOptions, dsoByteaAsEscString);
end;

procedure TPSQLDataSet.ReadOIDOpt(Reader: TReader); //deal with old missing properties
begin
 if Reader.ReadBoolean then Include(FOptions, dsoOIDAsInt);
end;

procedure TPSQLDataSet.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ByteaAsEscString', ReadByteaOpt, nil, False); //missing
  Filer.DefineProperty('OIDAsInt', ReadOIDOpt, nil, False); //missing
end;

constructor TPSQLDataSet.Create(AOwner : TComponent);
var I: integer;
begin
  Inherited Create(AOwner);
  FCacheBlobs := False;
  FAutoRefresh := FALSE;
  FAllowSequenced := False; //Added by Nicolas Ring

  FOptions := [dsoUseGUIDField];

  if (csDesigning in ComponentState) and Assigned(AOwner) and (DBList.Count > 0) then
   begin
    for I := DBList.Count - 1 downto 0 do
      if TCustomConnection(DBList[I]).Owner = AOwner then
      begin
         Database := TPSQLDatabase(DBList[I]);
         Break;
      end;
      if not Assigned(Database) then
        Database := TPSQLDatabase(DBList[DBList.Count - 1]);
   end;
end;

destructor TPSQLDataSet.Destroy;
begin
  Inherited Destroy;
  if FBlockReadBuf <> nil then
  begin
    FreeMem(FBlockReadBuf);
    FBlockReadBuf := nil;
  end;
  SetUpdateObject(nil);
end;

//////////////////////////////////////////////////////////
//procedure   : TPSQLDataSet.OpenCursor
//Description : Open cursor
//////////////////////////////////////////////////////////
//Input       : InfoQuery: Boolean
//////////////////////////////////////////////////////////
procedure TPSQLDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Database=nil then raise EDatabaseError.Create('Property Database not set!');
  if FHandle = nil then
     FHandle := CreateHandle;
  if FHandle = nil then
    raise ENoResultSet.Create(SHandleError);
  SetDBFlag(dbfOpened, TRUE);
  Inherited OpenCursor(InfoQuery);
  SetUpdateMode(FUpdateMode);
  {$IFNDEF FPC}
  SetupAutoRefresh;
  {$ENDIF}
end;

//////////////////////////////////////////////////////////
//procedure   : TPSQLDataSet.CloseCursor
//Description : Close cursor
//////////////////////////////////////////////////////////
procedure TPSQLDataSet.CloseCursor;
begin
  Inherited CloseCursor;
  if FHandle <> nil then
  begin
    DestroyHandle;
    FHandle := nil;
  end;
  FParentDataSet := nil;
  SetDBFlag(dbfOpened, FALSE);
end;

procedure TPSQLDataset.CreateFields;
var F: TField;
    I: integer;
begin
 F := nil; //make compiler happy
 inherited CreateFields;
 if FieldDefs.Count > Fields.Count then
   for I := 0 to FieldDefs.Count - 1 do
      if (FieldDefs[I].DataType = ftUnknown) and
        not ((faHiddenCol in FieldDefs[I].Attributes) and not FieldDefs.HiddenFields) then
         begin
          case GetFieldTypeOID(I) of
            FIELD_TYPE_POINT: F := TPSQLPointField.Create(Self);
            FIELD_TYPE_CIRCLE: F := TPSQLCircleField.Create(Self);
            FIELD_TYPE_BOX: F := TPSQLBoxField.Create(Self);
            FIELD_TYPE_LSEG: F := TPSQLLSegField.Create(Self);
          else
            Continue;
          end;
          try
            F.FieldName := FieldDefs[I].Name;
            F.SetFieldType(ftADT);
            F.Required := faRequired in FieldDefs[I].Attributes;
            F.ReadOnly := faReadonly in FieldDefs[I].Attributes;
            F.DataSet := FieldDefs.DataSet;
            F.Index := I;
          except
            F.Free;
            raise;
          end;
         end;
end;

//////////////////////////////////////////////////////////
//function    : TPSQLDataSet.CreateHandle
//Description : Virtual method Create Handle will be overwritten
//              in TPSQLQuery and TPSQLTable
//////////////////////////////////////////////////////////
//Output      : Result: HDBICur
//////////////////////////////////////////////////////////
function TPSQLDataSet.CreateHandle: HDBICur;
begin
  Result := nil;
end;

procedure TPSQLDataSet.DestroyHandle;
begin
  Engine.CloseCursor(FHandle);
end;

procedure TPSQLDataSet.InternalInitFieldDefs;
var
  I, FieldID: Integer;
  FieldDescs: TFLDDescList;
  ValCheckDesc: VCHKDesc;
  RequiredFields: TBits;
  CursorProps: CurProps;
  FldDescCount,
  MaxFieldID,
  HiddenFieldCount: Integer;
begin
  Engine.GetCursorProps(FHandle, CursorProps);
  FldDescCount := CursorProps.iFields;
  HiddenFieldCount := 0;
  if FieldDefs.HiddenFields then
  begin
    if SetBoolProp(Engine, Handle, curGETHIDDENCOLUMNS, TRUE) then
    begin
      Engine.GetCursorProps(FHandle, CursorProps);
      HiddenFieldCount := CursorProps.iFields - FldDescCount;
      FldDescCount := CursorProps.iFields;
    end;
  end;
  RequiredFields := TBits.Create;
  try
    MaxFieldID := GetIntProp(Engine, Handle, curMAXFIELDID);
    if MaxFieldID > 0 then
      RequiredFields.Size := MaxFieldID + 1 else
      RequiredFields.Size := FldDescCount + 1;
    for I := 1 to CursorProps.iValChecks do
    begin
      Engine.GetVChkDesc(FHandle, I, ValCheckDesc);
      if ValCheckDesc.bRequired and not ValCheckDesc.bHasDefVal then
        RequiredFields[ValCheckDesc.iFldNum] := True;
    end;
    SetLength(FieldDescs, FldDescCount);
    Engine.GetFieldDescs(FHandle, FieldDescs);
    FieldID := {$IFNDEF FPC}FieldNoOfs{$ELSE}1{$ENDIF};
    I := FieldID - 1;
    FieldDefs.Clear;
    while I < FldDescCount do
     begin
      FieldID := FieldDescs[I].iFldNum;
      AddFieldDesc(FieldDescs, I, FieldID, RequiredFields, FieldDefs);
     end;
    if FieldDefs.HiddenFields then
    begin
      SetBoolProp(Engine, Handle, curGETHIDDENCOLUMNS, False);
      if HiddenFieldCount > 0 then
        for I := FldDescCount - HiddenFieldCount to FldDescCount - 1 do
          FieldDefs[I].Attributes := FieldDefs[I].Attributes + [faHiddenCol];
    end;
  finally
    RequiredFields.Free;
  end;
end;

{$IFNDEF FPC}
procedure TPSQLDataSet.GetObjectTypeNames(Fields: TFields);
var
  Len: integer;
  I: Integer;
  TypeDesc: ObjTypeDesc;
  ObjectField: TObjectField;
begin
  for I := 0 to Fields.Count-1 do
    if Fields[I] is TObjectField then
    begin
      ObjectField := TObjectField(Fields[I]);
      TypeDesc.iFldNum := ObjectField.FieldNo;
      if (Engine.GetEngProp(hDBIObj(Handle), curFIELDTYPENAME, @TypeDesc,
        SizeOf(TypeDesc), Len) = DBIERR_NONE) and (Len > 0) then
        ObjectField.ObjectType := string(TypeDesc.szTypeName);
      with ObjectField do
        if DataType in [ftADT, ftArray] then
        begin
          if (DataType = ftArray) and SparseArrays and
             (Fields[0].DataType = ftADT) then
            GetObjectTypeNames(TObjectField(Fields[0]).Fields) else
            GetObjectTypeNames(Fields);
        end;
    end
end;
{$ENDIF}

procedure TPSQLDataSet.InternalOpen;
var
  CursorProps: CurProps;
begin
  Engine.GetCursorProps(FHandle, CursorProps);
  FRecordSize := CursorProps.iRecBufSize;
  BookmarkSize := CursorProps.iBookmarkSize;
  FCanModify := (CursorProps.eOpenMode = dbiReadWrite) and not CursorProps.bTempTable;
  FRecNoStatus := TRecNoStatus(CursorProps.ISeqNums);
  FieldDefs.Updated := FALSE;
  FieldDefs.Update;
  GetIndexInfo;
  if DefaultFields or (dsoForceCreateFields in FOptions) then
    CreateFields;
  BindFields(TRUE);
  {$IFNDEF FPC}
  if ObjectView then GetObjectTypeNames(Fields);
  {$ENDIF}
  if (dsoPopulateFieldsOrigin in FOptions) then PopulateFieldsOrigin();
  InitBufferPointers(FALSE);
  AllocKeyBuffers;
  Engine.SetToBegin(FHandle);
  PrepareCursor;
  if Filter <> '' then FExprFilter := CreateExprFilter(Filter, FilterOptions, 0);
  if Assigned(OnFilterRecord) then FFuncFilter := CreateFuncFilter(@TPSQLDataSet.RecordFilter, 1);
  if Filtered then ActivateFilters;
end;

procedure TPSQLDataSet.InternalClose;
begin
  FFuncFilter := nil;
  FExprFilter := nil;
  FreeKeyBuffers;
  BindFields(FALSE);
  if DefaultFields then DestroyFields;
  FIndexFieldCount := 0;
  FKeySize := 0;
  FExpIndex := FALSE;
  FCaseInsIndex := FALSE;
  FCanModify := FALSE;
end;

procedure TPSQLDataSet.PrepareCursor;
begin
end;

function TPSQLDataSet.IsCursorOpen: Boolean;
begin
  Result := Handle <> nil;
end;

procedure TPSQLDataSet.InternalHandleException;
var
  O: TObject;
begin
  if not Assigned(FDatabase) then Exit;
  O := ExceptObject;
  if not Assigned(O) then Exit;
  if (O is Exception) and not (O is EAbort) and Assigned(FDatabase.FOnException) then
    FDatabase.FOnException(Self, Exception(O))
end;

////////////////////////////////////////////////////////////
//                Record functions                        //
////////////////////////////////////////////////////////////
procedure TPSQLDataSet.InitBufferPointers(GetProps: Boolean);
var
  CursorProps: CurProps;
begin
  if GetProps then
  begin
    Check(Engine, Engine.GetCursorProps(FHandle, CursorProps));
    BookmarkSize := CursorProps.iBookmarkSize;
    FRecordSize  := CursorProps.iRecBufSize;
  end;
  FBlobCacheOfs := FRecordSize   + CalcFieldsSize;
  FRecInfoOfs   := FBlobCacheOfs + BlobFieldCount * SizeOf(Pointer);
  FBookmarkOfs  := FRecInfoOfs   + SizeOf(TRecInfo);
  FRecBufSize   := FBookmarkOfs  + BookmarkSize;
end;

function TPSQLDataSet.AllocRecordBuffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
begin
   Result := AllocMem(FRecBufSize);
end;

procedure TPSQLDataSet.FreeRecordBuffer(var Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
begin
  Engine.CheckBuffer(FHandle, Buffer); //pasha_golub 10.08.06
  ClearBlobCache(Buffer);
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TPSQLDataSet.InternalInitRecord(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
begin
  Engine.InitRecord(FHandle, Buffer);
end;

procedure TPSQLDataSet.ClearBlobCache(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
var
  I: Integer;
begin
  if FCacheBlobs then
    for I := 0 to Pred(BlobFieldCount) do
      TBlobDataArray(Buffer + FBlobCacheOfs)[ I ] := {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF};
end;

procedure TPSQLDataSet.ClearCalcFields(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
begin
 {$IFDEF DELPHI_12}
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0)
 {$ELSE}
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
 {$ENDIF}
end;

procedure TPSQLDataSet.InitRecord(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
begin
  Inherited InitRecord(Buffer);
  ClearBlobCache(Buffer);
  with PRecInfo(Buffer + FRecInfoOfs)^ do
  begin
    UpdateStatus := TUpdateStatus(usInserted);
    BookMarkFlag := bfInserted;
    RecordNumber := -1;
  end;
end;

function TPSQLDataSet.GetRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Status: DBIResult;
begin
  case GetMode of
    gmCurrent: Status := Engine.GetRecord(FHandle, dbiNoLock, Buffer, @FRecProps);
    gmNext:    Status := Engine.GetNextRecord(FHandle, dbiNoLock, Buffer, @FRecProps);
    gmPrior:   Status := Engine.GetPriorRecord(FHandle, dbiNoLock, Buffer, @FRecProps);
  else
    Status := DBIERR_NONE;
  end;
  case Status of
    DBIERR_NONE:
      begin
        with PRecInfo(Buffer + FRecInfoOfs)^ do
        begin
          UpdateStatus := TUpdateStatus(FRecProps.iRecStatus);
          BookmarkFlag := bfCurrent;
          case FRecNoStatus of
            rnParadox: RecordNumber := FRecProps.iSeqNum;
            rnDBase: RecordNumber := FRecProps.iPhyRecNum;
          else
            RecordNumber := -1;
          end;
        end;
        ClearBlobCache(Buffer);
        GetCalcFields(Buffer);
        Check(Engine, Engine.GetBookmark(FHandle, Buffer + FBookmarkOfs));
        Result := grOK;
      end;
    DBIERR_BOF: Result := grBOF;
    DBIERR_EOF: Result := grEOF;
  else
    Result := grError;
    if DoCheck then Check(Engine, Status);
  end;
end;

function TPSQLDataSet.GetCurrentRecord(Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    Result := (Engine.GetRecord(FHandle, dbiNoLock, Buffer, nil) = DBIERR_NONE);
  end else
    Result := FALSE;
end;

function TPSQLDataSet.GetOldRecord: PAnsiChar;
begin
  UpdateCursorPos();

  if SetBoolProp(Engine, Handle, curDELAYUPDGETOLDRECORD, TRUE) then
  try
    AllocCachedUpdateBuffers(True);
    Check(Engine, Engine.GetRecord(FHandle, dbiNoLock, FUpdateCBBuf.pOldRecBuf, nil));
    Result := PAnsiChar(FUpdateCBBuf.pOldRecBuf);
    AllocCachedUpdateBuffers(False);
  finally
    SetBoolProp(Engine, Handle, curDELAYUPDGETOLDRECORD, FALSE);
  end else
    Result := nil;
end;

procedure TPSQLDataSet.FetchAll;
begin
  if not EOF then
  begin
    CheckBrowseMode;
    Check(Engine, Engine.SetToEnd(Handle));
    Check(Engine, Engine.GetPriorRecord(FHandle, dbiNoLock, nil, nil));
    CursorPosChanged;
  end;
end;

procedure TPSQLDataSet.FlushBuffers;
begin
  CheckBrowseMode;
end;

function TPSQLDataSet.GetRecordCount: Integer;
begin
  CheckActive;
  if Engine.GetRecordCount(FHandle, Result) <> DBIERR_NONE then
    Result := -1;
end;

function TPSQLDataSet.GetRecNo: Integer;
var
  BufPtr: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
begin
  CheckActive;
  if (State = dsCalcFields) then
    BufPtr := CalcBuffer
  else
    BufPtr := ActiveBuffer;
  Result := PRecInfo(BufPtr + FRecInfoOfs).RecordNumber;
end;

procedure TPSQLDataSet.SetRecNo(Value : Integer);
begin
  CheckBrowseMode;
  if (FRecNoStatus = rnParadox) and (Value <> RecNo) then
  begin
    DoBeforeScroll;
    if Engine.SetToSeqNo(Handle, Value) = DBIERR_NONE then
    begin
      Resync([rmCenter]);
      DoAfterScroll;
    end;
  end;
end;

function TPSQLDataSet.GetRecordSize: integer;
begin
  Result := FRecordSize;
end;

function TPSQLDataSet.GetActiveRecBuf(var RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}): Boolean;
begin
  case State of
    dsBlockRead:
      {$IFDEF DELPHI_12}
      RecBuf := TRecordBuffer(FBlockReadBuf + (FBlockBufOfs * FRecordSize));
      {$ELSE}
      RecBuf := FBlockReadBuf + (FBlockBufOfs * FRecordSize);
      {$ENDIF}

    dsBrowse: if IsEmpty then
                 RecBuf := nil
              else
                 RecBuf := ActiveBuffer;

    dsEdit, dsInsert:
      {$IFDEF DELPHI_12}
      RecBuf := TRecordBuffer(ActiveBuffer);
      {$ELSE}
      RecBuf := ActiveBuffer;
      {$ENDIF}

    dsSetKey:
      {$IFDEF DELPHI_12}
      RecBuf := TRecordBuffer(PByte(FKeyBuffer) + SizeOf(TKeyBuffer));
      {$ELSE}
      RecBuf := PAnsiChar(FKeyBuffer) + SizeOf(TKeyBuffer);
      {$ENDIF}

    dsCalcFields: RecBuf := CalcBuffer;

    dsFilter:
      {$IFDEF DELPHI_12}
      RecBuf := TRecordBuffer(FFilterBuffer);
      {$ELSE}
      RecBuf := FFilterBuffer;
      {$ENDIF}

    dsNewValue: if FInUpdateCallback then
                   RecBuf := FUpdateCBBuf.pNewRecBuf
                else
                   RecBuf := ActiveBuffer;

    dsOldValue: if FInUpdateCallback then
                   RecBuf := FUpdateCBBuf.pOldRecBuf
                else
                   {$IFDEF DELPHI_12}
                    RecBuf := TRecordBuffer(GetOldRecord);
                   {$ELSE}
                    RecBuf := GetOldRecord;
                   {$ENDIF}

  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TPSQLDataSet.AddFieldDesc(FieldDescs: TFLDDescList; var DescNo: Integer;
  var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
var
  FType: TFieldType;
  FSize: integer;
  FRequired: Boolean;
  FPrecision, I: Integer;
  FieldName, FName: string;
  FieldDesc: FLDDesc;
begin
  FieldDesc := FieldDescs[DescNo];
  Inc(DescNo);
  with FieldDesc do
  begin
    FieldName := szName;
    I := 0;
    FName := FieldName;
    while FieldDefs.IndexOf(string(FName)) >= 0 do
    begin
      Inc(I);
      FName := Format('%s_%d', [string(FieldName), I]);
    end;
    if iFldType < MAXLOGFLDTYPES then
      FType := DataTypeMap[iFldType]
    else
      FType := ftUnknown;
    FSize := 0;
    FPrecision := 0;
    if RequiredFields.Size > FieldID then
      FRequired := RequiredFields[FieldID] else
      FRequired := False;
    case iFldType of
      fldZSTRING, fldBYTES, fldVARBYTES, fldADT, fldArray, fldRef:
        begin
          FSize := iUnits1;
        end;
      fldINT16, fldUINT16:
        if iLen <> 2 then FType := ftUnknown;
      fldINT32:
        if iSubType = fldstAUTOINC then
        begin
          FType := ftAutoInc;
          FRequired := False;
        end;
      fldFLOAT:                                                                                   
        if iSubType = fldstMONEY then FType := ftCurrency;
      fldBCD:
        begin
          FSize := Abs(iUnits2);
          FPrecision := iUnits1;
        end;
      fldBLOB:
        begin
          FSize := iUnits1;
          if (iSubType >= fldstMEMO) and (iSubType <= fldstBFILE) then
            FType := BlobTypeMap[iSubType];
        end;
      fldUUID:
        begin
          FSize := PSQLTypes.UUIDLEN;
          FType := ftGuid;
        end;
      fldPOINT:
        begin
          FSize := SizeOf(PSQLTypes.TPSQLPoint);
        end;
      fldCIRCLE:
        begin
          FSize := SizeOf(PSQLTypes.TPSQLCircle);
        end;
      fldBOX:
        begin
          FSize := SizeOf(PSQLTypes.TPSQLBox);
        end;
      fldLSEG:
        begin
          FSize := SizeOf(PSQLTypes.TPSQLLSeg);
        end;
    end;

    //pg: Unicode playing
    {$IFDEF DELPHI_12}
    if TNativeConnect(FDatabase.Handle).IsUnicodeUsed then
     case FType of
      ftString: FType := ftWideString;
      ftMemo:   FType := ftWideMemo;
      ftFixedChar: FType := ftFixedWideChar;
     end;
    {$ENDIF}


    with FieldDefs.AddFieldDef do
    begin
      {$IFNDEF FPC}FieldNo := FieldID;{$ENDIF}
      Inc(FieldID);
      Name := FName;
      DataType := FType;
      Size := FSize;
      Precision := FPrecision;
      if FRequired then
        Attributes := [faRequired];
      if efldrRights = fldrREADONLY then
        Attributes := Attributes + [faReadonly];
      if iSubType = fldstFIXED then
        Attributes := Attributes + [faFixed];
      InternalCalcField := bCalcField;
      case FType of
        ftADT:
          begin
            if iSubType = fldstADTNestedTable then
              Attributes := Attributes + [faUnNamed];
            for I := 0 to iUnits1 - 1 do
              AddFieldDesc(FieldDescs, DescNo, FieldID, RequiredFields, {$IFNDEF FPC}ChildDefs{$ELSE}nil{$ENDIF});
          end;
        ftArray:
          begin
            I := FieldID;
            FieldDescs[DescNo].szName := FieldDesc.szName + '[0]';
            AddFieldDesc(FieldDescs, DescNo, I, RequiredFields, {$IFNDEF FPC}ChildDefs{$ELSE}nil{$ENDIF});
            Inc(FieldID, iUnits2);
          end;
      end;
    end;
  end;
end;

{$IFNDEF FPC}
function TPSQLDataSet.GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
var
  RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
  Status: DBIResult;
  DoCheck: Boolean;
begin
  Result := 0;
  DoCheck := (BlockReadSize = 0);
  if (BlockReadSize > 0) then
    {$IFDEF DELPHI_12}
    RecBuf := TRecordBuffer(FBlockReadBuf + (FBlockBufOfs * FRecordSize))
    {$ELSE}
    RecBuf := FBlockReadBuf + (FBlockBufOfs * FRecordSize)
    {$ENDIF}
  else
    if not GetActiveRecBuf(RecBuf) then Exit;
  Status := Engine.OpenBlob(FHandle, RecBuf, FieldNo, dbiReadOnly);
  if (Status <> DBIERR_NONE) then
    Exit;
  try
    Status := Engine.GetBlobSize(FHandle, RecBuf, FieldNo, Result);
    if (Status <> DBIERR_NONE) or (Result = 0) then Exit;
    if  (High(Buffer) <= Result)  then
      SetLength(Buffer, Trunc(Result + Result div 4));
    Status := Engine.GetBlob(FHandle, RecBuf, FieldNo, 0, Result, Buffer, Result);
  finally
    if (Status  <> DBIERR_NONE) then
      Result := 0;
    Engine.FreeBlob(FHandle, RecBuf, FieldNo);
    if DoCheck then
      Check(Engine, Status)
  end;
end;
{$ENDIF}

function TPSQLDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
var
  IsBlank: Boolean;
  RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
  Status: DBIResult;
begin
{$IFNDEF FPC}
  if (BlockReadSize > 0) then
  begin
    Status := Engine.GetField(FHandle, FieldNo, FBlockReadBuf +
      (FBlockBufOfs * FRecordSize), Buffer, IsBlank);
    Result := (Status = DBIERR_NONE) and not IsBlank;
  end
  else
{$ENDIF}
  begin
    Result := GetActiveRecBuf(RecBuf);
    if Result then
    begin
      Check(Engine, Engine.GetField(FHandle, FieldNo, RecBuf, Buffer, IsBlank));
      Result := not IsBlank;
    end
  end;
end;


function TPSQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
begin
  if (Field.FieldNo > 0) then
    Result := GetFieldData(Field.FieldNo, Buffer)
  else
  begin
    if (State = dsBlockRead) then
    begin
      RecBuf := TempBuffer;
      Result := TRUE;
    end
    else
      Result := GetActiveRecBuf(RecBuf);
    if Result and (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead]) then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[ 0 ]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

{$IFDEF DELPHI_17}
function TPSQLDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
var
  RecBuf: PByte;
begin
  if Field.FieldNo > 0 then
    Result := GetFieldData(Field.FieldNo, Buffer)
  else
  begin
    if State = dsBlockRead then
    begin
      RecBuf := @TempBuffer[0];
      Result := True;
    end else
      Result := GetActiveRecBuf(RecBuf);
    if Result and (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead]) then
    begin
      Result := Boolean(RecBuf[FRecordSize + Field.Offset]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[FRecordSize + Field.Offset + 1], Buffer[0], Field.DataSize);
    end;
  end;
end;

function TPSQLDataSet.GetFieldData(FieldNo: Integer; Buffer: TValueBuffer): Boolean;
var
  IsBlank: Boolean;
  RecBuf: PByte;
  Status: DBIResult;
begin
  if BlockReadSize > 0 then
  begin
    { Optimized for speed.  If error, just return false }
    Status := Engine.GetField(FHandle, FieldNo, PByte(FBlockReadBuf) +
      (FBlockBufOfs * FRecordSize), Buffer, IsBlank);
    Result := (Status = DBIERR_NONE) and not IsBlank;
  end else
  begin
    Result := GetActiveRecBuf(RecBuf);
    if Result then
    begin
      Check(Engine, Engine.GetField(FHandle, FieldNo, RecBuf, Buffer, IsBlank));
      Result := not IsBlank;
    end
  end;
end;
{$ENDIF}

{$IFDEF DELPHI_17}
procedure TPSQLDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  RecBuf: PByte;
begin
  if not (State in dsWriteModes) then DatabaseError(SNotEditing, Self);
  if (State = dsSetKey) and ((Field.FieldNo < 0) or (FIndexFieldCount > 0) and
    not Field.IsIndexField) then DatabaseErrorFmt(SNotIndexField, [Field.DisplayName]);
  GetActiveRecBuf(RecBuf);
  if Field.FieldNo > 0 then
  begin
    if State = dsCalcFields then DatabaseError(SNotEditing, Self);
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
    Field.Validate(Buffer);
    if Field.FieldKind <> fkInternalCalc then
      Check(Engine, Engine.PutField(FHandle, Field.FieldNo, RecBuf, @Buffer[0]));
  end
  else {fkCalculated, fkLookup}
  begin
    Boolean(RecBuf[FRecordSize + Field.Offset]) := NativeUInt(Buffer) > 0; //was LongBool(Buffer)
    if Boolean(RecBuf[FRecordSize + Field.Offset]) then
      Move(Buffer[0], RecBuf[FRecordSize + Field.Offset + 1], Field.DataSize);
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;
{$ENDIF}

procedure TPSQLDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
{$IFDEF DELPHI_17}
  AValueBuffer: TValueBuffer;
{$ENDIF}
begin
  with Field do
  begin
    if not (State in dsWriteModes) then
      DatabaseError(SNotEditing, Self);
    if (State = dsSetKey) and ((FieldNo < 0) or (FIndexFieldCount > 0) and
      not IsIndexField) then
        DatabaseErrorFmt(SNotIndexField, [DisplayName], Self);
    GetActiveRecBuf(RecBuf);
    if (FieldNo > 0) then
    begin
      if (State = dsCalcFields) then DatabaseError(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
    {$IFDEF DELPHI_17}
      AValueBuffer := Buffer;
      Validate(AValueBuffer);
    {$ELSE}
      Validate(Buffer);
    {$ENDIF}
      if FieldKind <> fkInternalCalc then
        Check(Engine, Engine.PutField(FHandle, FieldNo, RecBuf, Buffer));
    end
    else {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[ 0 ]) then
        Move(Buffer^, RecBuf[ 1 ], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;

function TPSQLDataSet.GetBlobData(Field : TField; Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}) : TBlobData;
begin
  Result := TBlobDataArray(Buffer + FBlobCacheOfs)[ Field.Offset ];
end;

procedure TPSQLDataSet.SetBlobData(Field : TField; Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Value : TBlobData);
begin
  if (Buffer = ActiveBuffer) then
    TBlobDataArray(PAnsiChar(Buffer) + FBlobCacheOfs)[ Field.Offset ] := Value;
end;

procedure TPSQLDataSet.CloseBlob(Field: TField);
begin
  Engine.FreeBlob(Handle, ActiveBuffer, Field.FieldNo);
end;

{$IFNDEF FPC}
function TPSQLDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var Param: TPSQLParam;
begin
  CheckCachedUpdateMode;
  if  State = dsOldValue then
   begin
    Param := TPSQLParam.Create(nil);
    try
      Engine.GetFieldOldValue(Handle, Field.FieldName, Param);
      Result := Param.Value;
    finally
     Param.Free;
    end;
   end
  else
    Result := Inherited GetStateFieldValue(State, Field);
end;

procedure TPSQLDataSet.SetStateFieldValue(State: TDataSetState; Field: TField; Const Value: Variant);
begin
  CheckCachedUpdateMode;
  Inherited SetStateFieldValue(State, Field, Value);
end;

function TPSQLDataSet.GetFieldFullName(Field : TField) : string;
begin
    Result := inherited GetFieldFullName(Field);
end;
{$ENDIF}

procedure TPSQLDataSet.InternalFirst;
begin
  Check(Engine, Engine.SetToBegin(FHandle));
end;

procedure TPSQLDataSet.InternalLast;
begin
  Check(Engine, Engine.SetToEnd(FHandle));
end;

procedure TPSQLDataSet.InternalEdit;
begin
  FOldBuffer := AllocRecordBuffer;
  Move(ActiveBuffer^,FOldBuffer[0],FRecBufSize);
  Check(Engine, Engine.GetRecord(FHandle, {dbiNoLock}dbiWriteLock, ActiveBuffer, nil)); //locking stuff need attention
  ClearBlobCache(ActiveBuffer);
end;

procedure TPSQLDataSet.InternalInsert;
begin
  SetBoolProp(Engine, Handle, curMAKECRACK, TRUE);
  CursorPosChanged;
end;

procedure TPSQLDataSet.InternalPost;
begin
  if Assigned(FUpdateObject) then
   begin
     if State = dsEdit then
       FUpdateObject.Apply(ukModify)
     else
       if State = dsInsert then
         FUpdateObject.Apply(ukInsert);
   end //if assigned
  else
    begin
      {$IFDEF DELPHI_6}
      inherited; //mi:2008-02-13 Moved from begining of the method. Actually there is only CheckRequiredFields there.
                 //              So we don't need it if dataset is being updated by UpdateObject
      {$ENDIF}//pasha_golub 10.08.06
      
      if State = dsEdit then
        Check(Engine, Engine.ModifyRecord(FHandle,FOldBuffer, ActiveBuffer, TRUE,RecNo))
      else
        if State = dsInsert then
          Check(Engine, Engine.InsertRecord(FHandle, dbiNoLock, ActiveBuffer));
    end; //else
  if assigned(fOldBuffer) then  FreeRecordBuffer(FOldBuffer);
end;

procedure TPSQLDataSet.InternalDelete;
var
  Result: Word;
begin
  if not Assigned(FUpdateObject) then
   begin
    Result := Engine.DeleteRecord(FHandle, ActiveBuffer);
    if (Result <> DBIERR_NONE) then Check(Engine, Result);
   end
  else
   FUpdateObject.Apply(ukDelete);
end;

function TPSQLDataSet.IsSequenced: Boolean;
begin
  Result := (FRecNoStatus = rnParadox) and (not Filtered);
end;

function TPSQLDataSet.GetCanModify: Boolean;
begin
  Result := FCanModify or ForceUpdateCallback;
end;

procedure TPSQLDataSet.InternalRefresh;
begin
    Check(Engine, Engine.ForceReread(FHandle));
end;

procedure TPSQLDataSet.Post;
begin
  Inherited Post;
  if (State = dsSetKey) then
    PostKeyBuffer(TRUE);
end;

procedure TPSQLDataSet.Cancel;
begin
  Inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(FALSE);
end;

procedure TPSQLDataSet.InternalCancel;
begin
  if State = dsEdit then
    Engine.RelRecordLock(FHandle, FALSE);
  if assigned(fOldBuffer) then
    FreeRecordBuffer(FOldBuffer);
end;

procedure TPSQLDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then
    Check(Engine, Engine.AppendRecord(FHandle, Buffer))  else
    Check(Engine, Engine.InsertRecord(FHandle, dbiNoLock, Buffer));
end;

procedure TPSQLDataSet.InternalGotoBookmark(Bookmark : Pointer);
begin
  Check(Engine, Engine.SetToBookmark(FHandle, Bookmark));
end;

procedure TPSQLDataSet.InternalSetToRecord(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF});
begin
  InternalGotoBookmark(Buffer + FBookmarkOfs);
end;

function TPSQLDataSet.GetBookmarkFlag(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}) : TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

procedure TPSQLDataSet.SetBookmarkFlag(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Value : TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TPSQLDataSet.GetBookmarkData(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Data : Pointer);
begin
  Move(Buffer[FBookmarkOfs], Data^, BookmarkSize);
end;

{$IFDEF DELPHI_17}
procedure TPSQLDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  Move(Buffer[FBookmarkOfs], Data[0], BookmarkSize);
end;
{$ENDIF DELPHI_17}

procedure TPSQLDataSet.SetBookmarkData(Buffer : {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF}; Data : Pointer);
begin
  Move(Data^, Buffer[FBookmarkOfs], BookmarkSize);
end;

{$IFDEF DELPHI_17}
procedure TPSQLDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  Move(Data[0], Buffer[FBookmarkOfs], BookmarkSize);
end;
{$ENDIF DELPHI_17}

function TPSQLDataSet.CompareBookmarks(Bookmark1, Bookmark2 : TBookmark) : Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,CMPLess),(CMPGtr,CMPEql));
begin
  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if (Result = 2) then
  begin
    if (Handle <> nil) then
      Check(Engine, Engine.CompareBookmarks(Handle, Bookmark1, Bookmark2, Result));
    if (Result = CMPKeyEql) then
      Result := CMPEql;
  end;
end;

function TPSQLDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Handle <> nil);
  if Result then
  begin
    CursorPosChanged;
    Result := (Engine.SetToBookmark(FHandle, Bookmark) = DBIERR_NONE) and
      (Engine.GetRecord(FHandle, dbiNOLOCK, nil, nil) = DBIERR_NONE)
  end;
end;

{$IFNDEF FPC}
procedure TPSQLDataSet.SetBlockReadSize(Value: Integer);

  function CanBlockRead: Boolean;
  var
    i: Integer;
  begin
    Result := (BufferCount <= 1) and (DataSetField = nil);
    if Result then
      for i := 0 to FieldCount - 1 do
        if (Fields[i].DataType in [ftDataSet, ftReference]) then
        begin
          Result := False;
          break;
        end;
  end;

  procedure FreeBuffer;
  begin
    if FBlockReadBuf <> nil then
    begin
      FreeMem(FBlockReadBuf);
      FBlockReadBuf := nil;
    end;
  end;

const
  DEFBLOCKSIZE  = 64 * 1024;
var
  Size: Integer;
begin
  if Value <> BlockReadSize then
  begin
    if Value > 0 then
    begin
      if EOF or not CanBlockRead then Exit;
      FreeBuffer;
      UpdateCursorPos;

//mi:2008-03-25 #0766 curMAKECRACK flag set native dataset to tsEmpty mode. But we need to set it in tsFirst mode!
//      Engine.SetEngProp(HDBIObj(FHandle), curMAKECRACK, 0);
      TNativeDataSet(FHandle).RecordState := tsFirst;

      if Value = MaxInt then
        Size := DEFBLOCKSIZE
      else
        Size := Value * FRecordSize;

      FBlockReadBuf := AllocMem(Size);
      FBlockBufSize := Size div FRecordSize;
      FBlockBufOfs := FBlockBufSize; { Force read of data }
      FBlockBufCount := FBlockBufSize;
      FBlockReadCount := 0;

      inherited;

      BlockReadNext();
    end
    else
    begin
      inherited;
//      CursorPosChanged;
//      Resync([]);
      FreeBuffer;
    end;
  end;
end;

procedure TPSQLDataSet.BlockReadNext;
var
  Status: DbiResult;
begin
  if FBlockBufOfs >= FBlockBufCount - 1 then
  begin
    if FBlockBufCount < FBlockBufSize then
      Last()
    else
    begin
      Status := Engine.ReadBlock(FHandle, FBlockBufCount, FBlockReadBuf);

      if (Status <> DBIERR_NONE) and (Status <> DBIERR_EOF) then
        Check(Engine,Status);

      if (FBlockBufCount = 0) and (Status = DBIERR_EOF) then
        Last();
      Inc(FBlockReadCount, FBlockBufCount);
      FBlockBufOfs := 0;
    end
  end
  else
    Inc(FBlockBufOfs);

  if CalcFieldsSize > 0 then
    GetCalcFields(TempBuffer);

  DataEvent(deDataSetScroll, -1);
end;
{$ENDIF}

procedure TPSQLDataSet.GetIndexInfo;
var
  IndexDesc: IDXDesc;
begin
  if Engine.GetIndexDesc(FHandle, 0, IndexDesc) = DBIERR_NONE then
  begin
    FExpIndex := IndexDesc.bExpIdx;
    FCaseInsIndex := IndexDesc.bCaseInsensitive;
    if not ExpIndex then
    begin
      FIndexFieldCount := IndexDesc.iFldsInKey;
      FIndexFieldMap := IndexDesc.aiKeyFld;
    end;
    FKeySize := IndexDesc.iKeyLen;
  end;
end;


procedure TPSQLDataSet.SwitchToIndex(const IndexName, TagName : string);
var
  Status: DBIResult;
begin
  ResetCursorRange;
  UpdateCursorPos;
  Status := Engine.SwitchToIndex(FHandle, IndexName, TagName, 0, TRUE);
  if (Status = DBIERR_NOCURRREC) then
    Status := Engine.SwitchToIndex(FHandle, IndexName, TagName, 0, FALSE);
  Check(Engine, Status);
  FKeySize := 0;
  FExpIndex := FALSE;
  FCaseInsIndex := FALSE;
  FIndexFieldCount := 0;
  SetBufListSize(0);
  InitBufferPointers(TRUE);
  try
    SetBufListSize(BufferCount + 1);
  except
    SetState(dsInactive);
    CloseCursor;
    raise;
  end;
  GetIndexInfo;
end;

function TPSQLDataSet.GetIndexField(Index : Integer): TField;
var
  FieldNo: Integer;
begin
  if (Index < 0) or (Index >= FIndexFieldCount) then DatabaseError(SFieldIndexError, Self);
  FieldNo := FIndexFieldMap[Index];
  Result := FieldByNumber(FieldNo);
  if Result = nil then   DatabaseErrorFmt(SIndexFieldMissing, [ FieldDefs[FieldNo - 1].Name ], Self);
end;

procedure TPSQLDataSet.SetIndexField(Index : Integer; Value : TField);
begin
  GetIndexField(Index).Assign(Value);
end;

function TPSQLDataSet.GetIndexFieldCount: Integer;
begin
  Result := FIndexFieldCount;
end;

procedure TPSQLDataSet.AllocKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  try
    for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
      FKeyBuffers[KeyIndex] := InitKeyBuffer(AllocMem(SizeOf(TKeyBuffer) + FRecordSize));
  except
    FreeKeyBuffers;
    raise;
  end;
end;

procedure TPSQLDataSet.FreeKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
    DisposeMem(FKeyBuffers[ KeyIndex ], SizeOf(TKeyBuffer) + FRecordSize);
end;

function TPSQLDataSet.InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
begin
  FillChar(Buffer^, SizeOf(TKeyBuffer) + FRecordSize, 0);
  Engine.InitRecord(FHandle, PAnsiChar(Buffer) + SizeOf(TKeyBuffer));
  Result := Buffer;
end;

procedure TPSQLDataSet.CheckSetKeyMode;
begin
  if (State <> dsSetKey) then DatabaseError(SNotEditing, Self);
end;

function TPSQLDataSet.SetCursorRange: Boolean;
var
  RangeStart, RangeEnd: PKeyBuffer;
  StartKey, EndKey: PAnsiChar;
  IndexBuffer: PAnsiChar;
  UseStartKey, UseEndKey, UseKey: Boolean;
begin
   Result := FALSE;
   if not (BuffersEqual(FKeyBuffers[kiRangeStart], FKeyBuffers[kiCurRangeStart],SizeOf(TKeyBuffer) + FRecordSize) and
          BuffersEqual(FKeyBuffers[kiRangeEnd], FKeyBuffers[kiCurRangeEnd],SizeOf(TKeyBuffer) + FRecordSize)) then
  begin
    IndexBuffer := AllocMem(KeySize * 2);
    try
      UseStartKey := TRUE;
      UseEndKey := TRUE;
      RangeStart := FKeyBuffers[kiRangeStart];
      if RangeStart.Modified then
      begin
        StartKey := PAnsiChar(RangeStart) + SizeOf(TKeyBuffer);
        UseStartKey := Engine.ExtractKey(Handle, StartKey, IndexBuffer) = 0;
      end
      else
        StartKey := nil;
      RangeEnd := FKeyBuffers[kiRangeEnd];
      if RangeEnd.Modified then
      begin
        EndKey := PAnsiChar(RangeEnd) + SizeOf(TKeyBuffer);
        UseEndKey := (Engine.ExtractKey(Handle, EndKey, IndexBuffer + KeySize) = 0);
      end
      else
        EndKey := nil;
      UseKey := UseStartKey and UseEndKey;
      if UseKey then
      begin
        if (StartKey <> nil) then
          StartKey := IndexBuffer;
        if (EndKey <> nil) then
          EndKey := IndexBuffer + KeySize;
      end;
      Check(Engine, Engine.SetRange(FHandle, UseKey,
        RangeStart.FieldCount, 0, StartKey, not RangeStart.Exclusive,
        RangeEnd.FieldCount, 0, EndKey, not RangeEnd.Exclusive));
      Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiCurRangeStart]^,
        SizeOf(TKeyBuffer) + FRecordSize);
      Move(FKeyBuffers[kiRangeEnd]^, FKeyBuffers[kiCurRangeEnd]^,
        SizeOf(TKeyBuffer) + FRecordSize);
      DestroyLookupCursor;
      Result := TRUE;
    finally
      FreeMem(IndexBuffer, KeySize * 2);
    end;
  end;
end;

function TPSQLDataSet.ResetCursorRange: Boolean;
begin
  Result := FALSE;
  if FKeyBuffers[kiCurRangeStart].Modified or
    FKeyBuffers[kiCurRangeEnd].Modified then
  begin
    Check(Engine, Engine.ResetRange(FHandle));
    InitKeyBuffer(FKeyBuffers[kiCurRangeStart]);
    InitKeyBuffer(FKeyBuffers[kiCurRangeEnd]);
    DestroyLookupCursor;
    Result := TRUE;
  end;
end;

procedure TPSQLDataSet.SetLinkRanges(MasterFields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF});
var
  I: Integer;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[kiRangeStart]);
    FKeyBuffer^.Modified := TRUE;
    for I := 0 to Pred(MasterFields.Count) do
      GetIndexField(I).Assign(TField(MasterFields[I]));
    FKeyBuffer^.FieldCount := MasterFields.Count;
  finally
    RestoreState(SaveState);
  end;
  Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiRangeEnd]^,
    SizeOf(TKeyBuffer) + FRecordSize);
end;

function TPSQLDataSet.GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
begin
  Result := FKeyBuffers[KeyIndex];
end;

procedure TPSQLDataSet.SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  Move(FKeyBuffer^, FKeyBuffers[kiSave]^, SizeOf(TKeyBuffer) + FRecordSize);
  if Clear then InitKeyBuffer(FKeyBuffer);
  SetState(dsSetKey);
  SetModified(FKeyBuffer.Modified);
  DataEvent(deDataSetChange, 0);
end;

procedure TPSQLDataSet.PopulateFieldsOrigin();
var I: integer;
begin
 for I := 0 to Fields.Count -1 do
   Fields[I].Origin := Engine.GetFieldOrigin(FHandle, Fields[I].FieldNo)
end;

procedure TPSQLDataSet.PostKeyBuffer(Commit: Boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  //++ pasha_golub 27.10.2005 GotoNearest stuff
  if FKeyBuffer^.FieldCount = 0 then
     FKeyBuffer^.FieldCount := FIndexFieldCount;
  //
  if Commit then
    FKeyBuffer.Modified := Modified
  else
    Move(FKeyBuffers[kiSave]^, FKeyBuffer^, SizeOf(TKeyBuffer) + FRecordSize);
  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;

function TPSQLDataSet.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.Exclusive;
end;

procedure TPSQLDataSet.SetKeyExclusive(Value: Boolean);
begin
  CheckSetKeyMode;
  FKeyBuffer.Exclusive := Value;
end;

function TPSQLDataSet.GetKeyFieldCount: Integer;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.FieldCount;
end;

procedure TPSQLDataSet.SetKeyFieldCount(Value: Integer);
begin
  CheckSetKeyMode;
  FKeyBuffer.FieldCount := Value;
end;

procedure TPSQLDataSet.SetKeyFields(KeyIndex: TKeyIndex;
  const Values: array of const);
var
  I: Integer;
  SaveState: TDataSetState;
begin
  if ExpIndex then
    DatabaseError(SCompositeIndexError, Self);
  if (FIndexFieldCount = 0) then
    DatabaseError(SNoFieldIndexes, Self);
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[KeyIndex]);
    for I := 0 to High(Values) do
      GetIndexField(I).AssignValue(Values[I]);
    FKeyBuffer^.FieldCount := High(Values) + 1;
    FKeyBuffer^.Modified := Modified;
  finally
    RestoreState(SaveState);
  end;
end;

function TPSQLDataSet.GetIsIndexField(Field: TField): Boolean;
var
  I: Integer;
begin
  Result := FALSE;
  with Field do
    if (FieldNo > 0) then
      for I := 0 to Pred(FIndexFieldCount) do
       if (FIndexFieldMap[I] = FieldNo) then
        begin
          Result := TRUE;
          Exit;
        end;
end;

procedure TPSQLDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    Close;
    FDatabase := nil;
  end;
end;

procedure TPSQLDataSet.ActivateFilters;
begin
  if FExprFilter <> nil then
  begin
    if Engine.ActivateFilter(FHandle, FExprFilter) <> DBIERR_NONE then
    begin
      Engine.DropFilter(FHandle, FExprFilter);
      FExprFilter := CreateExprFilter(Filter, FilterOptions, 0);
      Check(Engine, Engine.ActivateFilter(FHandle, FExprFilter));
    end;
  end;
  if FFuncFilter <> nil then
  begin
    if (Engine.ActivateFilter(FHandle, FFuncFilter) <> DBIERR_NONE) then
    begin
      Engine.DropFilter(FHandle, FFuncFilter);
      FFuncFilter := CreateFuncFilter(@TPSQLDataSet.RecordFilter, 1);
      Check(Engine, Engine.ActivateFilter(FHandle, FFuncFilter));
    end;
  end;
end;

procedure TPSQLDataSet.DeactivateFilters;
begin
  if FFuncFilter <> nil then Check(Engine, Engine.DeactivateFilter(FHandle, FFuncFilter));
  if FExprFilter <> nil then Check(Engine, Engine.DeactivateFilter(FHandle, FExprFilter));
end;

function TPSQLDataSet.CreateExprFilter(const Expr: string;
  Options: TFilterOptions; Priority: Integer): HDBIFilter;
var
  Parser: TExprParser;
begin
  Parser := TExprParser.Create(Self, Expr, Options, [], '', nil, FldTypeMap);
  try
    Check(Engine, Engine.AddFilter(FHandle, 0, Priority, FALSE, PCANExpr(Parser.FilterData), nil, Result));
  finally
    Parser.Free;
  end;
end;

function TPSQLDataSet.CreateFuncFilter(FilterFunc: Pointer;Priority: Integer): HDBIFilter;
begin
  Check(Engine, Engine.AddFilter(FHandle, Integer(Self), Priority, FALSE, nil, PFGENFilter(FilterFunc), Result));
end;

function TPSQLDataSet.CreateLookupFilter(Fields: TList; const Values: Variant;
  Options: TLocateOptions; Priority: Integer): HDBIFilter;
var
  I: Integer;
  Filter: TFilterExpr;
  Expr, Node: PExprNode;
  FilterOptions: TFilterOptions;
begin
  Node := nil;
  Expr := nil;
  if loCaseInsensitive in Options then
    FilterOptions := [foNoPartialCompare, foCaseInsensitive]
  else
    FilterOptions := [foNoPartialCompare];
  Filter := TFilterExpr.Create(Self, FilterOptions, [], '', nil, FldTypeMap);
  try
    if (Fields.Count = 1) and not VarIsArray(Values) then
    begin
      Node := Filter.NewCompareNode(TField(Fields[0]), coEQ, Values);
      Expr := Node;
    end
    else
      for I := 0 to Fields.Count-1 do
      begin
        Node := Filter.NewCompareNode(TField(Fields[I]), coEQ, Values[I]);
        if I = 0 then
          Expr := Node else
          Expr := Filter.NewNode(enOperator, coAND, Unassigned, Expr, Node);
      end;
    if loPartialKey in Options then Node^.FPartial := TRUE;
    Check(Engine, Engine.AddFilter(FHandle, 0, Priority, FALSE, PCANExpr(Filter.GetFilterData(Expr)), nil, Result));
  finally
    Filter.Free;
  end;
end;

procedure TPSQLDataSet.SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
begin
  if Filtered then
  begin
    CursorPosChanged;
    DestroyLookupCursor;
    Engine.SetToBegin(FHandle);
    if Filter <> nil then Engine.DropFilter(FHandle, Filter);
    Filter := Value;
    if Filter <> nil then Engine.ActivateFilter(FHandle, Filter);
  end else
  begin
    if Filter <> nil then Engine.DropFilter(FHandle, Filter);
    Filter := Value;
  end;
end;

procedure TPSQLDataSet.SetFilterData(const Text: string; Options: TFilterOptions);
var
  HFilter: HDBIFilter;
begin
  if Active then
  begin
    CheckBrowseMode;
    if (Filter <> Text) or (FilterOptions <> Options) then
    begin
      if Text <> '' then
        HFilter := CreateExprFilter(Text, Options, 0) else
        HFilter := nil;
      SetFilterHandle(FExprFilter, HFilter);
    end;
  end;
  Inherited SetFilterText(Text);
  Inherited SetFilterOptions(Options);
  if Active and Filtered then First;
end;

procedure TPSQLDataSet.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;

procedure TPSQLDataSet.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      DestroyLookupCursor;
      Engine.SetToBegin(FHandle);
      if Value then
        ActivateFilters
      else
        DeactivateFilters;
      inherited SetFiltered(Value);
    end;
    First;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TPSQLDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

procedure TPSQLDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
var
  Filter: HDBIFilter;
begin
  if Active then
  begin
    CheckBrowseMode;
    if Assigned(OnFilterRecord) <> Assigned(Value) then
    begin
      if Assigned(Value) then
        Filter := CreateFuncFilter(@TPSQLDataSet.RecordFilter, 1)  else
        Filter := nil;
      SetFilterHandle(FFuncFilter, Filter);
    end;
    Inherited SetOnFilterRecord(Value);
    if Filtered then
      First;
  end 
  else
    Inherited SetOnFilterRecord(Value);
end;

function TPSQLDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Status: Word;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(FALSE);
  UpdateCursorPos;
  CursorPosChanged;
  if not Filtered then ActivateFilters;
  try
    if GoForward then
    begin
      if Restart then Check(Engine, Engine.SetToBegin(FHandle));
      Status := Engine.GetNextRecord(FHandle, dbiNoLock, nil, nil);
    end
    else
    begin
      if Restart then Check(Engine, Engine.SetToEnd(FHandle));
      Status := Engine.GetPriorRecord(FHandle, dbiNoLock, nil, nil);
    end;
  finally
    if not Filtered then
      DeactivateFilters;
  end;
  if Status = DBIERR_NONE then
  begin
    Resync([rmExact, rmCenter]);
    SetFound(TRUE);
  end;
  Result := Found;
  if Result then DoAfterScroll;
end;

function TPSQLDataSet.RecordFilter(RecBuf: Pointer; RecNo: Integer): Smallint;
var
  Accept: Boolean;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  try
    Accept := TRUE;
    OnFilterRecord(Self, Accept);
  except
    {$IFNDEF FPC}
    InternalHandleException();
    {$ENDIF}
  end;
  RestoreState(SaveState);
  Result := Ord(Accept);
end;

function TPSQLDataSet.LocateRecord(const KeyFields: string;
                                    const KeyValues: Variant;
                                    Options: TLocateOptions;
                                    SyncCursor: Boolean): Boolean;
var
  Fields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF};
  CaseInsensitive: boolean;
  Flds  : array of integer;
  SFlds : array of string;
  i, FieldCount, R : integer;
  aPartial : boolean;
  Status : Word;
begin
  if Self.Filtered then
  begin
    //mi:2009-07-31 we have to respect filters
    Status := LocateFilteredRecord(KeyFields, KeyValues, Options, SyncCursor);
    Result := Status = DBIERR_NONE;
    Exit;
  end;

  CheckBrowseMode();
  CursorPosChanged();
  DoBeforeScroll();

  Result := False;

  Fields := TList{$IFDEF DELPHI_17}<TField>{$ENDIF}.Create;
  try
    GetFieldList(Fields, KeyFields);
    CaseInsensitive := loCaseInsensitive in Options;

    FieldCount := Fields.Count;

    SetLength(Flds, FieldCount);
    SetLength(SFlds, FieldCount);

    if FieldCount = 1 then
    begin
      Flds[0] := TField(Fields.First).FieldNo - 1;
      if VarIsArray(KeyValues) then
        SFlds[0] := VarToStr(KeyValues[0])  //mi:2009-12-22 #1270 thanks to Matija Vidmar
      else
        SFlds[0] := VarToStr(KeyValues);
    end
    else
      for i := 0 to FieldCount - 1 do
      begin
        Flds[i] := TField(Fields[i]).FieldNo - 1;
        SFlds[i] := VarToStr(KeyValues[i])
      end;

    aPartial := (loPartialKey in Options) and (TField(Fields.Last).DataType in [ftString, ftWideString]);

    R := TNativeDataSet(FHandle).FindRows(Flds, SFlds, not CaseInsensitive, 0, not aPartial);

    if R <> -1 then
    begin
      Result := True;

      if SyncCursor then
      begin
        TNativeDataSet(FHandle).InitRecord(ActiveBuffer);
        TNativeDataSet(FHandle).SetToRecord(R);
        Resync([rmExact, rmCenter]);
        DoAfterScroll();
      end;
    end;
  finally
    Fields.Free();
  end;

end;

function TPSQLDataSet.LocateFilteredRecord(const KeyFields: string;
                                            const KeyValues: Variant;
                                            Options: TLocateOptions;
                                            SyncCursor: Boolean): Word;
var
  Fields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF};
  Filter: HDBIFilter;
  Status: DBIResult;
  I: Integer;
  Filter1: TFilterExpr;
  Expr, Node: PExprNode;
  fo: TFilterOptions;
  pos : int64;
begin
  CheckBrowseMode();
  CursorPosChanged();
  DoBeforeScroll();

  pos := TNativeDataSet(FHandle).RecordNumber;

  Fields := TList{$IFDEF DELPHI_17}<TField>{$ENDIF}.Create();
  try
    GetFieldList(Fields, KeyFields);
    Check(Engine, Engine.SetToBegin(FHandle));

    fo := [foNoPartialCompare];

    //mi:2010-02-07
    if loCaseInsensitive in Options then
      fo := fo + [foCaseInsensitive];

    Filter1 := TFilterExpr.Create(Self, fo, [], '', nil, FldTypeMap);

    try
      Node := nil;
      Expr := nil;
      if Fields.Count = 1 then
      begin
        if VarIsArray(KeyValues) then
          Node := Filter1.NewCompareNode(TField(Fields[0]), coEQ, KeyValues[0])
        else
          Node := Filter1.NewCompareNode(TField(Fields[0]), coEQ, KeyValues);

        Expr := Node;
      end
      else
      begin
        for i := 0 to Fields.Count - 1 do
        begin
          Node := Filter1.NewCompareNode(TField(Fields[I]), coEQ, KeyValues[I]);

          if I = 0 then
            Expr := Node
          else
            Expr := Filter1.NewNode(enOperator, coAND, Unassigned, Expr, Node);
        end;
      end;

      if loPartialKey in Options then
        Node^.FPartial := TRUE;

      Check(Engine, Engine.AddFilter(FHandle, 0, 2, FALSE, PCANExpr(Filter1.GetFilterData(Expr)), nil, Filter));
    finally
      Filter1.Free();
    end;

    Engine.ActivateFilter(FHandle, Filter);
    Status := Engine.GetNextRecord(FHandle, dbiNoLock, ActiveBuffer, nil);
    Engine.DropFilter(FHandle, Filter);
  finally
    Fields.Free();
  end;

  Result := Status;

  if SyncCursor then
  begin
    if Result = DBIERR_NONE then
    begin
      Resync([rmExact, rmCenter]);
    end
    else
    begin
//      TNativeDataSet(FHandle).InitRecord(ActiveBuffer);
      TNativeDataSet(FHandle).SetToRecord(pos);
    end;

    DoAfterScroll();
  end;
end;

function TPSQLDataSet.LocateNearestRecord(const KeyFields: string;const KeyValues: Variant;Options: TLocateOptions;SyncCursor: Boolean): Word;
var
  Buffer: {$IFDEF DELPHI_12}TRecordBuffer{$ELSE}PAnsiChar{$ENDIF};
  Fields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF};
  Filter: HDBIFilter;
  Status: DBIResult;
  I: Integer;
  Filter1: TFilterExpr;
  Expr, Node: PExprNode;
  FilterOptions: TFilterOptions;

begin
  Expr := nil; //make compiler happy
  Node := nil; //make compiler happy
  CheckBrowseMode;
  CursorPosChanged;
  Buffer := TempBuffer;
  Fields := TList{$IFDEF DELPHI_17}<TField>{$ENDIF}.Create;
  try
    GetFieldList(Fields, KeyFields);
    Check(Engine, Engine.SetToBegin(FHandle));
    FilterOptions := [foNoPartialCompare];
    Filter1 := TFilterExpr.Create(Self, FilterOptions, [], '', nil, FldTypeMap);
    try
      if Fields.Count = 1 then
      begin
         Node := Filter1.NewCompareNode(TField(Fields[0]), coGE, KeyValues);
         Expr := Node;
      end
      else
        for I := 0 to Fields.Count-1 do
        begin
          Node := Filter1.NewCompareNode(TField(Fields[I]), coGE, KeyValues[I]);
          if I = 0 then
            Expr := Node else
            Expr := Filter1.NewNode(enOperator, coAND, Unassigned, Expr, Node);
        end;
      if loPartialKey in Options then Node^.FPartial := TRUE;
      Check(Engine, Engine.AddFilter(FHandle, 0, 2, FALSE, PCANExpr(Filter1.GetFilterData(Expr)), nil,Filter));
    finally
      Filter1.Free;
    end;
    Engine.ActivateFilter(FHandle, Filter);
    Status := Engine.GetNextRecord(FHandle, dbiNoLock, Buffer, nil);
    Engine.DropFilter(FHandle, Filter);
  finally
    Fields.Free;
  end;
  Result := Status;
end;

function TPSQLDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Null;

  if VarIsNull(KeyValues) then
    Exit;
  DoBeforeScroll();
  if LocateRecord(KeyFields, KeyValues, [], True) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TPSQLDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll();
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
end;

function TPSQLDataSet.GetLookupCursor(const KeyFields: string; CaseInsensitive: Boolean): HDBICur;
begin
  Result := nil;
end;

procedure TPSQLDataSet.DestroyLookupCursor;
begin
end;

procedure TPSQLDataSet.AllocCachedUpdateBuffers(Allocate: Boolean);
begin
  if Allocate then
  begin
    FUpdateCBBuf := AllocMem(SizeOf(DELAYUPDCbDesc));
    FUpdateCBBuf.pNewRecBuf := AllocMem(FRecBufSize);
    FUpdateCBBuf.pOldRecBuf := AllocMem(FRecBufSize);
    FUpdateCBBuf.iRecBufSize := FRecordSize;
  end
  else
  begin
    if Assigned(FUpdateCBBuf) then
    begin
      FreeMem(FUpdateCBBuf.pNewRecBuf);
      FreeMem(FUpdateCBBuf.pOldRecBuf);
      DisposeMem(FUpdateCBBuf, SizeOf(DELAYUPDCbDesc));
    end;
  end;
end;

procedure TPSQLDataSet.CheckCachedUpdateMode;
begin
end;

function TPSQLDataSet.UpdateCallbackRequired: Boolean;
begin
{$IFDEF FPC}
  Result := False;
{$ELSE}
  Result := FCachedUpdates  and (Assigned(FOnUpdateError) or
    Assigned(FOnUpdateRecord) or Assigned(FUpdateObject));
{$ENDIF}
end;

function TPSQLDataSet.ForceUpdateCallback: Boolean;
begin
  Result := True{FCachedUpdates} and ({$IFNDEF FPC}Assigned(FOnUpdateRecord) or{$ENDIF}
    Assigned(FUpdateObject));
end;

procedure TPSQLDataSet.SetCachedUpdates(Value: Boolean);

  procedure ReAllocBuffers;
  begin
    FreeFieldBuffers;
    FreeKeyBuffers;
    SetBufListSize(0);
    try
      InitBufferPointers(TRUE);
      SetBufListSize(BufferCount + 1);
      AllocKeyBuffers;
    except
      SetState(dsInactive);
      CloseCursor;
      raise;
    end;
  end;

begin
  if (State = dsInActive) or (csDesigning in ComponentState) then
    FCachedUpdates := Value
  else
  if (FCachedUpdates <> Value) then
  begin
    CheckBrowseMode;
    UpdateCursorPos;
    FCachedUpdates := Value;
    ReAllocBuffers;
    AllocCachedUpdateBuffers(Value);
    SetupCallBack(UpdateCallBackRequired);
    Resync([]);
  end;
end;

procedure TPSQLDataSet.SetupCallBack(Value: Boolean);
begin
  if Value then
  begin
    if (csDesigning in ComponentState) then
      Exit;
    if not Assigned(FUpdateCallback) then
      FUpdateCallback := TPSQLBDECallBack.Create(Engine, Self, Self.Handle, cbDELAYEDUPD,
        FUpdateCBBuf, SizeOf(DELAYUPDCbDesc), CachedUpdateCallBack, TRUE);
  end
  else
  begin
    if Assigned(FUpdateCallback) then
    begin
      FUpdateCallback.Free;
      FUpdateCallback := nil;
    end;
  end;
end;

function TPSQLDataSet.ProcessUpdates(UpdCmd: DBIDelayedUpdCmd): Word;
begin
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Result :=0;
//  Resync([]); //NEW
end;

procedure TPSQLDataSet.ApplyUpdates;
var
  Status: Word;
begin
  if (State <> dsBrowse) then Post;
  Status := ProcessUpdates(dbiDelayedUpdPrepare);
  if (Status <> DBIERR_NONE) then
    if (Status = DBIERR_UPDATEABORT) then SysUtils.Abort else TDbiError(Engine,Status);
end;

procedure TPSQLDataSet.CommitUpdates;
begin
  Check(Engine, ProcessUpdates(dbiDelayedUpdCommit));
end;

procedure TPSQLDataSet.CancelUpdates;
begin
  Cancel;
  ProcessUpdates(dbiDelayedUpdCancel);
end;

procedure TPSQLDataSet.RevertRecord;
var
  Status: Word;
begin
  if State in dsEditModes then Cancel;
  Status := ProcessUpdates(dbiDelayedUpdCancelCurrent);
  if not ((Status = DBIERR_NONE) or (Status = DBIERR_NOTSUPPORTED)) then
    Check(Engine, Status);
end;


function TPSQLDataSet.UpdateStatus: TUpdateStatus;
begin
   Result := usUnModified;
end;

function TPSQLDataSet.CachedUpdateCallBack(CBInfo: Pointer): CBRType;
const
  CBRetCode: array[TUpdateAction] of CBRType = (cbrAbort, cbrAbort,
    cbrSkip, cbrRetry, cbrPartialAssist);
var
  UpdateAction: TUpdateAction;
  UpdateKind: TUpdateKind;
begin
  FInUpdateCallBack := TRUE;
  UpdateAction := uaFail;
  UpdateKind := TUpdateKind(ord(FUpdateCBBuf.eDelayUpdOpType)-1);
  try
{$IFNDEF FPC}
    if Assigned(FOnUpdateRecord) then
      FOnUpdateRecord(Self, UpdateKind, UpdateAction)
    else
{$ENDIF}
      if Assigned(FUpdateObject) then
      begin
        FUpdateObject.Apply(UpdateKind);
        UpdateAction := uaApplied;
      end
    else
      TDbiError(Engine, FUpdateCBBuf.iErrCode);
  except
    on E: Exception do
    begin
      if E is EPSQLDatabaseError then
        FUpdateCBBuf.iErrCode := EPSQLDatabaseError(E).ErrorCode;
{$IFNDEF FPC}
      if (E is EDatabaseError) and Assigned(FOnUpdateError) then
        FOnUpdateError(Self, EDatabaseError(E), UpdateKind, UpdateAction)
      else
{$ENDIF}
      begin
        {$IFNDEF FPC}
        InternalHandleException();
        {$ENDIF}
        UpdateAction := uaAbort;
      end;
    end;
  end;
  Result := CBRetCode[UpdateAction];
  if UpdateAction = uaAbort then
    FUpdateCBBuf.iErrCode := DBIERR_UPDATEABORT;
  FInUpdateCallBack := FALSE;
end;

{$IFNDEF FPC}
function TPSQLDataSet.GetUpdateRecordSet: TUpdateRecordTypes;
begin
  if Active then
  begin
    Result := TUpdateRecordTypes(Byte(GetIntProp(Engine, FHandle,
      curDELAYUPDDISPLAYOPT)));
  end
  else
    Result := [];
end;

procedure TPSQLDataSet.SetUpdateRecordSet(RecordTypes: TUpdateRecordTypes);
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Check(Engine, Engine.SetEngProp(hDbiObj(Handle), curDELAYUPDDISPLAYOPT, Longint(Byte(RecordTypes))));
  Resync([]);
end;
{$ENDIF}

procedure TPSQLDataSet.SetUpdateObject(Value: TPSQLSQLUpdateObject);
begin
  if (Value <> FUpdateObject) then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      { if another dataset already references this updateobject, then
        remove the reference }
      if Assigned(FUpdateObject.DataSet) and (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

{$IFNDEF FPC}
procedure TPSQLDataSet.SetOnUpdateError(UpdateEvent: TUpdateErrorEvent);
begin
  if Active then SetupCallback(UpdateCallBackRequired);
  FOnUpdateError := UpdateEvent;
end;
{$ENDIF}

function TPSQLDataSet.GetUpdatesPending: Boolean;
begin
  Result := GetIntProp(Engine, FHandle, curDELAYUPDNUMUPDATES) > 0;
end;

{$IFDEF DELPHI_17}
procedure TPSQLDataSet.DataConvert(Field: TField; Source, Dest: TValueBuffer; ToNative: Boolean);
begin
  if (Field.DataType = ftDateTime) and not ToNative then //#1871 	TDateTimeField supports dates before 30/12/1899 from now
    Move(Source[0], Dest[0], SizeOf(TDateTime))
  else
   inherited;
end;
{$ELSE}
procedure TPSQLDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
begin
  if (Field.DataType = ftDateTime) and not ToNative then //#1871 	TDateTimeField supports dates before 30/12/1899 from now
    TDateTime(Dest^) := TDateTime(Source^)
  else
   inherited;
end;
{$ENDIF}
procedure TPSQLDataSet.DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF});

  procedure CheckIfParentScrolled;
  var
    ParentPosition, I: Integer;
  begin
    ParentPosition := 0;
    with FParentDataSet do
     if not IsEmpty then
       for I := 0 to BookmarkSize - 1 do
         ParentPosition := ParentPosition + Byte(ActiveBuffer[FBookmarkOfs+I]);
    if (FLastParentPos = 0) or (ParentPosition <> FLastParentPos) then
    begin
      First;
      FLastParentPos := ParentPosition;
    end
    else
    begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;

begin
  if (Event = deParentScroll) then
    CheckIfParentScrolled;
  inherited DataEvent(Event, Info);
end;

{$IFNDEF FPC}
{ TBDEDataSet.IProviderSupport}
function TPSQLDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if E is EPSQLDatabaseError then
  begin
    if Prev <> nil then
      PrevErr := Prev.ErrorCode else
      PrevErr := 0;
    with EPSQLDatabaseError(E) do
      Result := EUpdateError.Create(E.Message, '', ErrorCode, PrevErr, E);
  end
  else
    Result := inherited PSGetUpdateException(E, Prev);
end;

function TPSQLDataSet.PSIsSQLSupported: Boolean;
begin
  Result := TRUE;
end;

procedure TPSQLDataSet.PSReset;
begin
  inherited PSReset;
  if Handle <> nil then
    Engine.ForceReread(Handle);
end;
{$ENDIF}

function TPSQLDataSet.GetHandle: HDBICur;
begin
  Result := FHandle;
end;


function TPSQLDataSet.CheckOpen(Status: Word): Boolean;
begin
  case Status of
    DBIERR_NONE: Result := TRUE;
    DBIERR_NOTSUFFTABLERIGHTS: Result := FALSE;
  else
    TDbiError(Engine, Status);
    Result := FALSE;
  end;
end;

procedure TPSQLDataSet.Disconnect;
begin
  Close;
end;

function TPSQLDataSet.GetDBHandle: HDBIDB;
begin
  if FDatabase <> nil then
  begin
    if FDatabase.Handle = nil then
       FDatabase.Connected := True;
    Result := FDatabase.Handle;
  end
  else
    Result := nil;
end;

procedure TPSQLDataSet.GetDatabaseNames(List : TStrings);
var
  i     : Integer;
  Names : TStringList;
begin
  Names := TStringList.Create;
  try
    Names.Sorted := TRUE;
    for I := 0 to DBList.Count-1 do
      with TPSQLDatabase(DBList[i]) do Names.Add(DatabaseName);
    List.Assign(Names);
  finally
    Names.Free;
  end;
end;

procedure TPSQLDataSet.CloseDatabase(Database: TPSQLDatabase);
begin
  if Assigned(Database) then
    Database.CloseDatabase(Database);
end;

function TPSQLDataSet.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
begin
  Result := Flag in DBFlags;
  if Value then
  begin
    if not Result then
    begin
      if FDBFlags = [] then
      begin
        FDatabase.Open;
        Inc(FDatabase.FRefCount);
        {$IFNDEF FPC}
        FDatabase.RegisterClient(Self);
        {$ENDIF}
      end;
      Include(FDBFlags, Flag);
    end;
  end                      
  else
  begin
    if Result then
    begin
      Exclude(FDBFlags, Flag);
      if FDBFlags = [] then
      begin
        {$IFNDEF FPC}
        FDatabase.UnRegisterClient(Self);
        {$ENDIF}
        CloseDatabase(FDatabase);
      end;
    end;
  end;
end;

procedure TPSQLDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if (FHandle <> nil) and True and CanModify then
    Check(Engine, Engine.SetEngProp(hDbiObj(FHandle), curUPDLOCKMODE, Longint(Value)));
  FUpdateMode := Value;
end;

{ AutoRefresh }
procedure TPSQLDataSet.SetAutoRefresh(const Value: Boolean);
begin
  CheckInactive;
  FAutoRefresh := Value;
end;

procedure TPSQLDataSet.SetDatabase(Value: TPSQLDatabase);
begin
   if Active then Close;
   try
     {$IFNDEF FPC}
     if Assigned(FDatabase) then  FDatabase.UnRegisterClient(Self);
     {$ENDIF}
     if Assigned(Value) then FDatabase := Value;
   finally
     FDatabase := Value;
   end;
end;

function TPSQLDataSet.GetDatabase: TPSQLDatabase;
begin
   Result := TPSQLDatabase(FDatabase);
end;

{$IFNDEF FPC}
procedure TPSQLDataSet.SetupAutoRefresh;
const
  PropFlags : array[TAutoRefreshFlag] of LongInt = (0, curFIELDISAUTOINCR, curFIELDISDEFAULT);
var
  I       : Integer;
  ColDesc : ServerColDesc;
begin
  if AutoRefresh then
    Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),curAUTOREFETCH,LongInt(TRUE)));
  for I := 0 to Fields.Count - 1 do
    with Fields[I] do
      if (AutoGenerateValue <> arNone) then
      begin
        ColDesc.iFldNum    := I + 1;
        ColDesc.bServerCol := TRUE;
        Check(Engine, Engine.SetEngProp(hDbiObj(FHandle), PropFlags[ AutoGenerateValue ], LongInt(@ColDesc)));
      end;
end;

{ TPSQLDataSet.IProviderSupport }
procedure TPSQLDataSet.PSGetAttributes(List : TList);
var
  Attr: PPacketAttribute;
begin
  inherited PSGetAttributes(List); //29.11.2007
  New(Attr);
  List.Add(Attr);
  with Attr^ do
  begin
    Name := 'LCID';
    Value := Integer(-1);
    IncludeInDelta := False;
  end;
end;

function TPSQLDataSet.PSIsSQLBased: Boolean;
var
  InProvider : Boolean;
begin
  InProvider := SetDBFlag(dbfProvider, TRUE);
  try
    Result := True;
  finally
    SetDBFlag(dbfProvider, InProvider);
  end;
end;

function TPSQLDataSet.PSGetQuoteChar: string;
begin
  Result := '"';
end;

function TPSQLDataSet.PSInTransaction: Boolean;
var
  InProvider: Boolean;
begin
  if not Assigned(Database) or not Database.Connected then
    Result := FALSE
  else
  begin
    InProvider := SetDBFlag(dbfProvider, TRUE);
    try
      Result := Database.InTransaction;
    finally
      SetDBFlag(dbfProvider, InProvider);
    end;
  end;
end;

procedure TPSQLDataSet.PSStartTransaction;
begin
  SetDBFlag(dbfProvider, TRUE);
  try
    if not PSIsSQLBased then
      Database.TransIsolation := tiDirtyRead;
    Database.StartTransaction;
  except
    SetDBFlag(dbfProvider, FALSE);
    Raise;
  end;
end;

procedure TPSQLDataSet.PSEndTransaction(Commit : Boolean);
const
  EndType: array[Boolean] of eXEnd = (xendABORT, xendCOMMIT);
begin
  try
    Database.ClearStatements;
    Database.EndTransaction(EndType[ Commit ]);
  finally
    SetDBFlag(dbfProvider, FALSE);
  end;
end;

{$IFDEF DELPHI_17}
function TPSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): Integer;
var
  InProvider: Boolean;
begin
  InProvider := SetDBFlag(dbfProvider, True);
  try
    Result := Database.Execute(ASQL, AParams);
  finally
    SetDBFlag(dbfProvider, InProvider);
  end;
end;

function TPSQLDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  var ResultSet: TDataSet): Integer;
var
  InProvider: Boolean;
begin
  Result := 0; //make compiler happy
  InProvider := SetDBFlag(dbfProvider, TRUE);
  try
    ResultSet := TPSQLQuery.Create(nil);
    try
      TPSQLQuery(ResultSet).Database := Database;
      TPSQLQuery(ResultSet).SQL.Text := ASQL;
      TPSQLQuery(ResultSet).Params.Assign(AParams);
      TPSQLQuery(ResultSet).Open;
      Result := Max(TPSQLQuery(ResultSet).RowsAffected, TPSQLQuery(ResultSet).RecordCount);
    except
      FreeAndNil(ResultSet);
      raise;
    end;
  finally
    SetDBFlag(dbfProvider, InProvider);
  end;
end;
{$ELSE}
function TPSQLDataSet.PSExecuteStatement(const ASQL : string; AParams: TParams; ResultSet: Pointer = nil): Integer;
var
  InProvider: Boolean;
  Q: TPSQLQuery;
begin
  InProvider := SetDBFlag(dbfProvider, TRUE);
  try
    if Assigned(ResultSet) or Assigned(AParams) then
    begin
      {$WARNINGS OFF} //make D5 compiler happy
      Q := TPSQLQuery.Create(nil);
      {$WARNINGS ON}
      try
        Q.Database := Database;
        Q.SQL.Text := ASQL;
        Q.Params.Assign(AParams);
        // In Insert, Resolver expect the number of records affected...
        if not Assigned(ResultSet) then
           Q.ExecSQL
        else
           Q.Open;
        Result := Q.RowsAffected;
      finally
       if Assigned(ResultSet) then
         TPSQLDataset(ResultSet^) := Q
       else
         Q.Free;
      end;
    end
    else
      Result := Database.Execute(ASQL);
  finally
    SetDBFlag(dbfProvider, InProvider);
  end;
end;
{$ENDIF DELPHI_17}
{$ENDIF FPC}

/////////////////////////////////////////////////////////////////
//                    TPSQLQuery                                //
/////////////////////////////////////////////////////////////////
constructor TPSQLQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TPSQLParams.Create(Self);
  FDataLink := TPSQLQueryDataLink.Create(Self);
  RequestLive := FALSE;
  ParamCheck := TRUE;
  FRowsAffected := -1;
  CacheBlobs := False;
end;

destructor TPSQLQuery.Destroy;
begin
  Destroying;
  Disconnect;
  SQL.Free;
  FParams.Free;
  FDataLink.Free;
  StrDispose(SQLBinary);
  Inherited Destroy;
end;

function TPSQLQuery.Engine : TPSQLEngine;
begin
  Result := FDataBase.Engine;
end;

function TPSQLQuery.CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream;
begin
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode);
end;

function TPSQLQuery.IsSequenced: Boolean;
begin
  Result := FAllowSequenced and inherited IsSequenced;
end;

procedure TPSQLQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TPSQLQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare else  UnPrepare;
end;

procedure TPSQLQuery.Prepare;
begin
  if Assigned(FHandle) then
   begin
    SetDBFlag(dbfPrepared, TRUE);
    SetPrepared(TRUE);
   end;
end;

procedure TPSQLQuery.UnPrepare;
begin
  SetPrepared(FALSE);
  SetDBFlag(dbfPrepared, FALSE);
end;

procedure TPSQLQuery.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

function TPSQLQuery.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TPSQLQuery.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    Disconnect;
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

function TPSQLQuery.GetQuery:TStrings;
begin
   Result := FSQL;
end;

procedure TPSQLQuery.QueryChanged(Sender: TObject);
var
  List: TPSQLParams;
begin
  if not (csReading in ComponentState) then
  begin
    Disconnect;
    StrDispose(SQLBinary);
    SQLBinary := nil;
    if ParamCheck {or (csDesigning in ComponentState)} then
    begin
      List := TPSQLParams.Create(Self);
      try
        FText := List.ParseSQL(SQL.Text, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end else
      FText := SQL.Text;
    DataEvent(dePropertyChange, 0);
  end else
    FText := FParams.ParseSQL(SQL.Text, False);
end;

procedure TPSQLQuery.SetParamsList(Value: TPSQLParams);
begin
  FParams.AssignValues(Value);
end;

function TPSQLQuery.GetParamsCount: integer;
begin
  Result := FParams.Count;
end;

procedure TPSQLQuery.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if (Filer.Ancestor <> nil) then
      Result := not FParams.IsEqual(TPSQLQuery(Filer.Ancestor).FParams)
    else
      Result := (FParams.Count > 0);
  end;

begin
  Inherited DefineProperties(Filer);
  Filer.DefineBinaryproperty('Data', ReadBinaryData, WriteBinaryData, SQLBinary <> nil);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

procedure TPSQLQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TPSQLQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

procedure TPSQLQuery.ReadBinaryData(Stream: TStream);
begin
  SQLBinary := StrAlloc(Stream.Size);
  Stream.ReadBuffer(SQLBinary^, Stream.Size);
end;

procedure TPSQLQuery.WriteBinaryData(Stream: TStream);
begin
  Stream.WriteBuffer(SQLBinary^, StrBufSize(SQLBinary));
end;

procedure TPSQLQuery.SetRequestLive(const Value : Boolean);
begin
   if Value <> FRequestLive then
      FRequestLive := Value;
end;

function TPSQLQuery.GetRequestLive : Boolean;
begin
   Result := FRequestLive;
end;

procedure TPSQLQuery.SetPrepared(Value: Boolean);
begin
  if (FHandle <> nil) and Value then
    DatabaseError(SDataSetOpen, Self);
  if Value <> Prepared then
  begin
    if Value then
    begin
      FRowsAffected := -1;
      FCheckRowsAffected := TRUE;
      if Length(Text) > 1 then
        PrepareSQL(PChar(Text)) else
        DatabaseError(SEmptySQLStatement, Self);
    end
    else
    begin
      if FCheckRowsAffected then
        FRowsAffected := RowsAffected;
    end;
    FPrepared := Value;
  end;
end;

procedure TPSQLQuery.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if FDataLink.DataSource <> nil then
  begin
    DataSet := FDataLink.DataSource.DataSet;
    if DataSet <> nil then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        if not FParams[I].Bound then
        begin
          FParams[I].AssignField(DataSet.FieldByName(FParams[I].Name));
          FParams[I].Bound := FALSE;
        end;
    end;
  end;
end;

procedure TPSQLQuery.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and (DataSet.State <> dsSetKey) then
        begin
          TNativeDataset(FHandle).CloseTable;
          SetParamsFromCursor();
          if FParams.Count > 0 then
            TNativeDataset(FHandle).QuerySetParams(FParams, FSQL.Text);
          TNativeDataset(FHandle).OpenTable();
          First();          
        end;
    end;
  finally
    EnableControls;
  end;
end;


function TPSQLQuery.ParamByName(const Value: string): TPSQLParam;
begin
  Result := FParams.ParamByName(Value);
end;

function TPSQLQuery.CreateCursor(GenHandle: Boolean): HDBICur;
begin
  if SQL.Count > 0 then
  begin
    FExecSQL := not GenHandle;
    try
       SetPrepared(TRUE);
    finally
      FExecSQL := FALSE;
    end;
    if FDataLink.DataSource <> nil then SetParamsFromCursor;
    Result := GetQueryCursor(GenHandle);
  end
  else
  begin
    DatabaseError(SEmptySQLStatement, Self);
    Result := nil;
  end;
  FCheckRowsAffected := (Result = nil);
end;


function TPSQLQuery.CreateHandle: HDBICur;
begin
  Result := CreateCursor(TRUE)
end;


procedure TPSQLQuery.ExecSQL;
begin
  CheckInActive;
  SetDBFlag(dbfExecSQL, TRUE);
  try
    CreateCursor(FALSE);
  finally
    SetDBFlag(dbfExecSQL, FALSE);
    if FHandle <> nil then
    begin
      Check(Engine, Engine.CloseCursor(hDBICur(FHandle)));
      FHandle := nil;
    end;    
  end;
end;

function TPSQLQuery.GetQueryCursor(GenHandle: Boolean): HDBICur;
const
  DataType: array[Boolean] of LongInt = (Ord(wantCanned), Ord(wantLive));
var
  PCursor: phDBICur;
  CanLive : boolean;
begin
  Result := nil;
  if GenHandle then
    PCursor := @Result else
    PCursor := nil;
  if FParams.Count > 0 then
      Check(Engine,Engine.QuerySetParams(hDBIStmt(FHandle),Params,SQL.Text));
  Check(Engine, Engine.QExec(hDBIStmt(FHandle), PCursor, FRowsAffected));
  //pasha_golub 20.12.06
  CanLive := False;
  if FRequestLive and not ForceUpdateCallback and not FExecSQL then
    CanLive := TNativeDataSet(FHandle).CheckCanLive();
  Check(Engine, Engine.SetEngProp(hDbiObj(FHandle), stmtLIVENESS, DataType[CanLive]));
  //pasha_golub 20.12.06
end;

function TPSQLQuery.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
var
  NewConnection: Boolean;
begin
  if Value then
  begin
    NewConnection := DBFlags = [];
    Result := Inherited SetDBFlag(Flag, Value);
    if not (csReading in ComponentState) and NewConnection then
      FLocal := False;
  end
  else
  begin
    if DBFlags - [Flag] = [] then
      SetPrepared(FALSE);
    Result := Inherited SetDBFlag(Flag, Value);
  end;
end;

procedure TPSQLQuery.SetOptions(const Value: TPSQLDatasetOptions);
begin
 if Value = FOptions then Exit;
 inherited;
 if Active then
  begin
   Close;
   Open;
  end;
end;

procedure TPSQLQuery.PrepareSQL(Value: PChar);
begin
  GetStatementHandle(Value);
  if not Local then
    SetBoolProp(Engine, FHandle, stmtUNIDIRECTIONAL, FUniDirectional);
end;

procedure TPSQLQuery.GetStatementHandle(SQLText: PChar);
const
  DataType: array[Boolean] of LongInt = (Ord(wantCanned), Ord(wantLive));
var
  DBh : HDBIDB;
begin
  DBh := DBHandle;
  Check(Engine,Engine.QAlloc(DBH, hDBIStmt(FHandle)));
  try
    TNativeDataset(FHandle).Options := Options;
    if not FExecSQL then
    begin
      Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),stmtLIVENESS,
           DataType[RequestLive and not ForceUpdateCallback]));
    end;
    if Local then
    begin
      SetBoolProp(Engine,FHandle,stmtAUXTBLS,FALSE);
      SetBoolProp(Engine,FHandle,stmtCANNEDREADONLY,TRUE);
    end;
    while not CheckOpen(Engine.QPrepare(hDBIStmt(FHandle), SQLText)) do
      {Retry};
  except
    Engine.QFree(hDBIStmt(FHandle));
    FHandle := nil;
    raise;
  end;
end;


function TPSQLQuery.GetRowsAffected: Integer;
var
  Length: integer;
begin
  if Prepared then
    if Engine.GetEngProp(hDBIObj(FHandle), stmtROWCOUNT, @Result, SizeOf(Result), Length) <> 0 then
      Result := -1  else
  else
    Result := FRowsAffected;
end;


{$IFNDEF FPC}
procedure TPSQLQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

  function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if (Field <> nil) then
      List.Add(Field);
    Result := Field <> nil;
  end;

var
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

{ TPSQLQuery.IProviderSupport }
function TPSQLQuery.PSGetDefaultOrder: TIndexDef;
begin
  Result := inherited PSGetDefaultOrder;
  if not Assigned(Result) then
    Result := GetIndexForOrderBy(SQL.Text, Self);
end;

function TPSQLQuery.PSGetParams : TParams;
begin
  Result := FParams;
end;

procedure TPSQLQuery.PSSetParams(AParams : TParams);
begin
  if (AParams.Count <> 0) then
    Params.Assign(AParams);
  Close;
end;

function TPSQLQuery.PSGetTableName: string;
begin
  Result := GetTableNameFromSQL(SQL.Text);
end;

procedure TPSQLQuery.PSExecute;
begin
  ExecSQL;
end;

procedure TPSQLQuery.PSSetCommandText(const CommandText : string);
begin
  if (CommandText <> '') then
    SQL.Text := CommandText;
end;
{$ENDIF}

function TPSQLDataSet.GetLastInsertID(const FieldNum: integer): integer;
begin
 CheckActive;
 if Assigned(FHandle) then
    Check(Engine, Engine.GetLastInsertId(FHandle, FieldNum, Result))
 else
    Result := -1;
end;

function TPSQLDataSet.GetStmtHandle: HDBIStmt;
begin
  Result := hDBIStmt(GetHandle());
end;

function TPSQLDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if (FieldType = ftGuid) and (dsoUseGUIDField in FOptions) then
   Result := TPSQLGuidField
  else
    Result := inherited GetFieldClass(FieldType);
end;

procedure TPSQLDataSet.SortBy(FieldNames: string);
begin
	if Active and (RecordCount > 1) then
	begin
   try
		TNativeDataSet(FHandle).SortBy(FieldNames);
    TNativeDataset(Fhandle).SetRowPosition(-1, -1, ActiveBuffer);
    Resync([]);
   except
    FSortFieldNames := '';
    raise;
   end;
	end;
end;


procedure TPSQLDataSet.SortBy(FieldNames: string; Compare: TPSQLDatasetSortCompare);
begin
  if Active and (RecordCount > 1) then
  begin
		TNativeDataSet(FHandle).SortBy(FieldNames, Compare);
    TNativeDataset(Fhandle).SetRowPosition(-1, -1, ActiveBuffer);
  end;
end;

procedure TPSQLDataSet.SetOptions(const Value: TPSQLDatasetOptions);
begin
  FOptions := Value;
end;

procedure TPSQLDataSet.SetSortFieldNames(const Value: string);
begin
	if FSortFieldNames <> Value then
	begin
	  SortBy(Value);
 		FSortFieldNames := Value;
	end;
end;

function TPSQLDataSet.GetSortFieldNames: string;
begin
 if not Active or
    not Assigned(FHandle) or
    not TNativeDataSet(FHandle).IsSortedLocally then
  FSortFieldNames := '';
 Result := FSortFieldNames;
end;

function TPSQLDataSet.GetStoreActive: boolean;
begin
 Result := Active
            and Assigned(FDatabase)
            and (
              (ddoStoreConnected in FDatabase.DesignOptions)
               or not (csDesigning in ComponentState)
                );
end;

function TPSQLDataSet.GetFieldTypeOID(const FieldNum: integer): cardinal;
begin
 if Assigned(FHandle) then
    Result := Engine.GetFieldTypeOID(FHandle, FieldNum)
 else
    Result := InvalidOID;
end;

{ TPSQLUpdateSQL }
constructor TPSQLUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  Inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;
end;

destructor TPSQLUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if Assigned(FDataSet) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  Inherited Destroy;
end;

procedure TPSQLUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
var RN, RC: integer;
begin
  with Query[UpdateKind] do
  begin
    Prepare;
    ExecSQL;
    if Assigned(FDataSet) then
    begin
       TNativeDataset(FDataset.Handle).FreeBlobStreams(FDataset.ActiveBuffer); //30.10.2012
       RN := TNativeDataset(FDataset.Handle).RecordNumber;
       TNativeDataset(FDataset.Handle).OpenTable;
       TNativeDataset(FDataset.Handle).RecordState := tsPos;
       if UpdateKind <> ukDelete then
        begin
         if not TNativeDataset(FDataset.Handle).SetRowPosition(-1, 0, FDataset.ActiveBuffer) then
          try
           TNativeDataset(FDataset.Handle).SettoSeqNo(RN + 1);
          except
          end
        end
       else
         begin
          if Engine.GetRecordCount(FDataset.Handle, RC) <> DBIERR_NONE then
            RC := -1;
          if RN >= RC then
            RN := 0;
          try
           TNativeDataset(FDataset.Handle).SettoSeqNo(RN);
          except
          end;
         end;
       TNativeDataset(FDataset.Handle).IsLocked := False;
    end;
    if Assigned(FRecordChangeCompleteEvent) then
      FRecordChangeCompleteEvent(FDataset,UpdateKind);
  end;
end;

function TPSQLUpdateSQL.GetQueryClass : TPSQLQueryClass;
begin
  Result := TPSQLQuery;
end;

function TPSQLUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TPSQLQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := GetQueryClass.Create(Self);
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
    if FDataSet is TPSQLDataSet then
       FQueries[UpdateKind].Database := TPSQLDataSet(FDataSet).DataBase;
  end;
  Result := FQueries[UpdateKind];
end;

function TPSQLUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TPSQLUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

function TPSQLUpdateSQL.GetDataSet: TPSQLDataSet;
begin
  Result := FDataSet;
end;

procedure TPSQLUpdateSQL.SetDataSet(ADataSet: TPSQLDataSet);
begin
  FDataSet := ADataSet;
end;

procedure TPSQLUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TPSQLUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

procedure TPSQLUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Params.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TPSQLUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  I: Integer;
  Old: Boolean;
  Param: TPSQLParam;
  PName: string;
  Field: TField;
begin
  if not Assigned(FDataSet) then Exit;
  Query[UpdateKind].Database := FDataset.Database; //01.08.2008
  with Query[UpdateKind] do
  begin
    for I := 0 to Params.Count - 1 do
    begin
      Param := Params[I];
      PName := Param.Name;
      Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
      if Old and (UpdateKind in [ukInsert,ukDelete]) then
        DatabaseError(Format(SNoParameterValue,[Param.Name]));
      if Old then
        System.Delete(PName, 1, 4);
      Field := FDataSet.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then
        Check(FDataset.Engine,FDataset.Engine.GetFieldOldValue(FDataset.Handle, PName, Param))
      else
        Check(FDataset.Engine,FDataset.Engine.GetFieldValueFromBuffer(FDataset.Handle, FDataset.ActiveBuffer, PName, Param, UpdateKind <> ukModify));
      if Param.DataType = ftUnknown then
        Param.DataType := ftString;
    end;
  end;
end;

procedure TPSQLUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

///////////////////////////////////////////////////////////////////////////////
//                         TPSQLTable                                       //
///////////////////////////////////////////////////////////////////////////////
constructor TPSQLTable.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FIndexDefs := TIndexDefs.Create(Self);
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FDefaultIndex := TRUE;
  CacheBlobs := False;
  FLimit := 0;
end;

destructor TPSQLTable.Destroy;
begin
  Inherited Destroy;
  FMasterLink.Free;
  FIndexDefs.Free;
end;

function TPSQLTable.GetLimit: Integer;
begin
   Result := FLimit;
end;

procedure TPSQLTable.SetLimit(const Value : Integer);
begin
   if FLimit <> Value then
      FLimit := Value;
end;

function TPSQLTable.GetHandle(const IndexName, IndexTag: string): HDBICur;
const
  OpenModes: array[Boolean] of DbiOpenMode = (dbiReadWrite, dbiReadOnly);
  ShareModes: array[Boolean] of DbiShareMode = (dbiOpenShared, dbiOpenExcl);
var
  IndexID: Word;
  OpenMode: DbiOpenMode;
  RetCode: Word;
  DBH : HDBIDB;

  procedure FillAddonProps;
  begin
   Check(Engine,Engine.GetTableProps(DBHandle, FTableName, FOwner,
        FComment, FTablespace, FHasOIDs, FTableID));
  end;

begin
  Result := nil;
  OpenMode := OpenModes[FReadOnly];
  if DefaultIndex then
    IndexID := 0  else IndexID := NODEFAULTINDEX;
  while TRUE do
  begin
    DBH := DBHandle;
    RetCode := Engine.OpenTable(DBH, FTableName, '',
      IndexName, IndexTag, IndexID, OpenMode, ShareModes[FExclusive],
      xltField, FALSE, nil, Result, FOptions, FLimit, FOffset);
    if RetCode = DBIERR_TABLEREADONLY then
      OpenMode := dbiReadOnly    else
    FillAddonProps;
    if CheckOpen(RetCode) then  Break;
  end;
end;

function TPSQLTable.Engine : TPSQLEngine;
begin
  Result := FDataBase.Engine;
end;

function TPSQLTable.CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream;
begin
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode);
end;

function TPSQLTable.IsSequenced: Boolean;
begin
  Result := FAllowSequenced and inherited IsSequenced;
end;

function TPSQLTable.CreateHandle: HDBICur;
var
  IndexName, IndexTag: string;
begin
  if FTableName = '' then  DatabaseError(SNoTableName, Self);
  IndexDefs.Updated := FALSE;
  GetIndexParams(FIndexName, FFieldsIndex, IndexName, IndexTag);
  Result := GetHandle(IndexName, IndexTag);
  TNativeDataset(Result).Options := Options;
end;

function TPSQLTable.GetLanguageDriverName: string;
begin
  Result := '';
end;

procedure TPSQLTable.PrepareCursor;
begin
  CheckMasterRange;
end;

{$IFNDEF FPC}
procedure TPSQLTable.DefChanged(Sender: TObject);
begin
  StoreDefs := TRUE;
end;
{$ENDIF}

procedure TPSQLTable.InitFieldDefs;
var
  I, FieldID, FldDescCount: Integer;
  FieldDescs: TFLDDescList;
  FCursor: HDBICur;
  RequiredFields: TBits;
begin
  if FHandle <> nil then
     InternalInitFieldDefs else
  begin
    SetDBFlag(dbfFieldList, TRUE);
    try
      if (FTableName = '') then  DatabaseError(SNoTableName, Self);
        while not CheckOpen(Engine.OpenFieldList(DBHandle, FTableName,
          '', FALSE, FCursor)) do {Retry};
        try
          Check(Engine, Engine.GetRecordCount(FCursor, FldDescCount));
          SetLength(FieldDescs, FldDescCount);
          { Create an array of field descriptors }
          for I := 0 to FldDescCount - 1 do
            Check(Engine, Engine.GetNextRecord(FCursor, dbiNoLock, @FieldDescs[I], nil));
          { Initialize list of required fields }
          RequiredFields := TBits.Create;
          try
            if FieldDescs[FldDescCount-1].iFldNum > FldDescCount then
              RequiredFields.Size := FieldDescs[FldDescCount-1].iFldNum + 1
            else
              RequiredFields.Size := FldDescCount + 1;
            { Initialize the FieldDefs }
            FieldDefs.BeginUpdate;
            try
              FieldDefs.Clear;
              I := 0;
              FieldID := 1;
              while I < FldDescCount do
                AddFieldDesc(FieldDescs, I, FieldID, RequiredFields, FieldDefs);
            finally
              FieldDefs.EndUpdate;
            end;
          finally
            RequiredFields.Free;
          end;
        finally
          Engine.CloseCursor(FCursor);
        end;
    finally
      SetDBFlag(dbfFieldList, FALSE);
    end;
  end;
end;

procedure TPSQLTable.DestroyHandle;
begin
  DestroyLookupCursor;
  Inherited DestroyHandle;
end;

procedure TPSQLTable.DecodeIndexDesc(const IndexDesc: IDXDesc;
  var Source, Name, FieldExpression, DescFields: string;
  var Options: TIndexOptions);

  procedure ConcatField(var FieldList: string; const FieldName: string);
  begin
    if FieldList = '' then
      FieldList := FieldName else
      FieldList := Format('%s;%s', [FieldList, FieldName]);
  end;

var
  IndexOptions: TIndexOptions;
  I: Integer;
  SSource, SName: string;
  FieldName: string;
  s : string;
begin
  with IndexDesc do
  begin
    //if szTagName[0] = #0 then
    if szTagName = '' then
    begin
      SName := szName;
      Source := '';
    end
    else
    begin
      SSource := szName;
      SName := szTagName;
      S := SSource;
      //TNativeToAnsi(Engine, SSource, s);
      Source := string(s);
    end;

    //TNativeToAnsi(Engine, SName, s);
    S := SName;
    Name := ExtractFileName(string(s));
    Source := ExtractFileName(Source);
    IndexOptions := [];
    if bPrimary then Include(IndexOptions, ixPrimary);
    if bUnique then Include(IndexOptions, ixUnique);
    if bDescending then Include(IndexOptions, ixDescending);
    if bCaseInsensitive then Include(IndexOptions, ixCaseInsensitive);
    if not bMaintained then Include(IndexOptions, ixNonMaintained);
    if bExpIdx then
    begin
      //TNativeToAnsi(Engine, szKeyExp, S);
      S := szKeyExp;
      FieldExpression := string(s);
      Include(IndexOptions, ixExpression);
    end else
    begin
      FieldExpression := '';
      DescFields := '';
      for I := 0 to iFldsInKey - 1 do
      begin
        FieldName := FieldDefs[aiKeyFld[I] - 1].Name;
        ConcatField(FieldExpression, FieldName);
        if abDescending[I] then
          ConcatField(DescFields, FieldName);
      end;
      if bDescending and (DescFields = FieldExpression) then  DescFields := '';
    end;
    Options := IndexOptions;
  end;
end;

procedure TPSQLTable.EncodeIndexDesc(var IndexDesc: IDXDesc;
  const Name, FieldExpression: string; Options: TIndexOptions;
  const DescFields: string);

  function IndexFieldOfs(const FieldName: string): Integer;
  var
    FieldNo: Integer;
  begin
    FieldNo := FieldDefs.Find(FieldName).FieldNo;
    for Result := 0 to IndexDesc.iFldsInKey - 1 do
      if IndexDesc.aiKeyFld[Result] = FieldNo then
        Exit;
    DatabaseErrorFmt(SIndexFieldMissing, [FieldName], Self);
    Result := -1;
  end;

var
  Pos: Integer;
begin
  FillChar(IndexDesc, SizeOf(IndexDesc), 0);
  with IndexDesc do
  begin
    Move(Name[1], szName, Max(Length(Name), DBIMAXNAMELEN) * SizeOf(Char));
    //szName      := Copy(Name, 1, length(Name));
    bPrimary    := ixPrimary in Options;
    bUnique     := ixUnique in Options;
    bDescending := (ixDescending in Options) and (DescFields = '');
    bMaintained := not (ixNonMaintained in Options);
    Word(bCaseInsensitive) := Word(ixCaseInsensitive in Options);
    if ixExpression in Options then
    begin
      bExpIdx := TRUE;
      //TAnsiToNative(Engine, FieldExpression, szKeyExp, SizeOf(szKeyExp) - 1);
      szKeyExp := FieldExpression;
    end
    else
    begin
      Pos := 1;
      while (Pos <= Length(FieldExpression)) and (iFldsInKey < DBIMAXFLDSINKEY) do
      begin
        aiKeyFld[iFldsInKey] :=
          FieldDefs.Find(ExtractFieldName(FieldExpression, Pos)).FieldNo;
        Inc(iFldsInKey);
      end;
      if (DescFields <> '') then
      begin
        bDescending := TRUE;
        Pos := 1;
        while Pos <= Length(DescFields) do
          abDescending[IndexFieldOfs(ExtractFieldName(DescFields, Pos))] := TRUE;
      end;
    end;
  end;
end;

procedure TPSQLTable.AddIndex(const Name, Fields: string; Options: TIndexOptions;
  const DescFields: string);
var
  IndexDesc: IDXDesc;
begin
  FieldDefs.Update;
  if Active then
  begin
    EncodeIndexDesc(IndexDesc, Name, Fields, Options, DescFields);
    CheckBrowseMode;
    CursorPosChanged;
    Check(Engine, Engine.AddIndex(DBHandle, Handle, '', '', IndexDesc, ''));
  end
  else
  begin
      EncodeIndexDesc(IndexDesc, Name, Fields, Options, DescFields);
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.AddIndex(DBHandle, nil, FTableName, '', IndexDesc, ''));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
  IndexDefs.Updated := FALSE;
end;

procedure TPSQLTable.DeleteIndex(const Name: string);
var
  IndexName, IndexTag: string;
begin
  if Active then
  begin
    GetIndexParams(Name, FALSE, IndexName, IndexTag);
    CheckBrowseMode;
    Check(Engine, Engine.DeleteIndex(DBHandle, Handle, '', '', IndexName, IndexTag, 0));
  end
  else
  begin
    GetIndexParams(Name, FALSE, IndexName, IndexTag);
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.DeleteIndex(DBHandle, nil, FTableName, '',
        IndexName, IndexTag, 0));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
  FIndexDefs.Updated := FALSE;
end;

function TPSQLTable.GetIndexFieldNames: string;
begin
    if FFieldsIndex then Result := FIndexName else Result := '';
end;

function TPSQLTable.GetIndexName: string;
begin
  if FFieldsIndex then Result := '' else Result := FIndexName;
end;

procedure TPSQLTable.GetIndexNames(List: TStrings);
begin
  IndexDefs.Update;
  IndexDefs.GetItemNames(List);
end;

procedure TPSQLTable.GetIndexParams(const IndexName: string;
  FieldsIndex: Boolean; var IndexedName, IndexTag: string);
var
  IndexStr: TIndexName;
begin
  IndexStr := '';
  if IndexName <> '' then
  begin
    IndexDefs.Update;
    IndexStr := IndexName;
    if FieldsIndex then
       IndexStr := IndexDefs.FindIndexForFields(IndexName).Name;
  end;
  IndexedName := IndexStr;
  IndexTag := '';
end;

procedure TPSQLTable.SetIndexDefs(Value: TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;

procedure TPSQLTable.SetIndex(const Value: string; FieldsIndex: Boolean);
var
  IndexName, IndexTag: string;
begin
  if Active then CheckBrowseMode;
  if (FIndexName <> Value) or (FFieldsIndex <> FieldsIndex) then
  begin
    if Active then
    begin
      GetIndexParams(Value, FieldsIndex, IndexName, IndexTag);
      SwitchToIndex(IndexName, IndexTag);
      CheckMasterRange;
    end;
    FIndexName := Value;
    FFieldsIndex := FieldsIndex;
    if Active then Resync([]);
  end;
end;

procedure TPSQLTable.SetIndexFieldNames(const Value: string);
begin
    SetIndex(Value, Value <> '');
end;

procedure TPSQLTable.SetIndexName(const Value: string);
begin
  SetIndex(Value, FALSE);
end;

procedure TPSQLTable.UpdateIndexDefs;
var
  Opts: TIndexOptions;
  IdxName, Src, Flds, DescFlds: string;

  procedure UpdateFromCursor;
  var
    I: Integer;
    Cursor: HDBICur;
    CursorProps: CurProps;
    IndexDescs: TIDXDescList;
  begin
    if Handle = nil then
       Cursor := GetHandle('', '') else
       Cursor := Handle;
    try
      Engine.GetCursorProps(Cursor, CursorProps);
      if CursorProps.iIndexes > 0 then
      begin
        SetLength(IndexDescs, CursorProps.iIndexes);
        Engine.GetIndexDescs(Cursor, IndexDescs);
        for I := 0 to CursorProps.iIndexes - 1 do
        begin
          DecodeIndexDesc(IndexDescs[I], Src, IdxName, Flds, DescFlds, Opts);
          with IndexDefs.AddIndexDef do
          begin
            Name := IdxName;
            Fields := Flds;
            DescFields := DescFlds;
            Options := Opts;
            if Src <> '' then Source := Src;
          end;
        end;
      end;
    finally
      if (Cursor <> nil) and (Cursor <> Handle) then Engine.CloseCursor(Cursor);
    end;
  end;

  procedure UpdateFromIndexList;
  var
    FCursor: HDBICur;
    IndexDesc: IDXDesc;
  begin
    while not CheckOpen(Engine.OpenIndexList(DBHandle, FTableName, '', FCursor)) do {Retry};
    try
        while Engine.GetNextRecord(FCursor, dbiNoLock, @IndexDesc, nil) = 0 do
          if IndexDesc.bMaintained then
          begin
            DecodeIndexDesc(IndexDesc, Src, IdxName, Flds, DescFlds, Opts);
            with IndexDefs.AddIndexDef do
            begin
              Name := IdxName;
              Fields := Flds;
              DescFields := DescFlds;
              Options := Opts;
            end;
            Finalize(IndexDesc);
          end;
    finally
      Engine.CloseCursor(FCursor);
    end;
  end;

begin
  Inc(FDatabase.FRefCount);
  SetDBFlag(dbfIndexList, TRUE);
  try
    FieldDefs.Update;
    IndexDefs.Clear;
    if IsCursorOpen then
      UpdateFromCursor else
      UpdateFromIndexList;
  finally
    SetDBFlag(dbfIndexList, FALSE);
  end;
end;

function TPSQLTable.GetExists: Boolean;
begin
  Result := Active;
  if Result or (TableName = '') or not Assigned(Database) then Exit;
  SetDBFlag(dbfTable, TRUE);
  try
    Database.SelectString('SELECT ' + QuotedStr(TableName) + ' :: regclass', Result)
  finally
    SetDBFlag(dbfTable, FALSE);
  end;
end;

function TPSQLTable.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  Result := GotoKey;
end;

procedure TPSQLTable.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  GotoNearest;
end;

{$HINTS OFF}
function TPSQLTable.GotoKey: Boolean;
var
  KeyBuffer: PKeyBuffer;
  IndexBuffer, RecBuffer: PAnsiChar;
  UseKey: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  KeyBuffer := GetKeyBuffer(kiLookup);
  IndexBuffer := AllocMem(KeySize);
  try
    RecBuffer := PAnsiChar(KeyBuffer) + SizeOf(TKeyBuffer);
    UseKey := Engine.ExtractKey(Handle, RecBuffer, IndexBuffer) = 0;
    if UseKey then RecBuffer := IndexBuffer;
    Result := Engine.GetRecordForKey(Handle, UseKey, KeyBuffer^.FieldCount, 0, RecBuffer, nil,True) = 0;
    if Result then Resync([rmExact, rmCenter]);
    if Result then DoAfterScroll;
  finally
    FreeMem(IndexBuffer, KeySize);
  end;
end;

procedure TPSQLTable.GotoNearest;
var
  SearchCond: DBISearchCond;
  KeyBuffer: PKeyBuffer;
  IndexBuffer, RecBuffer: PAnsiChar;
  UseKey: Boolean;
begin
  CheckBrowseMode;
  CursorPosChanged;
  KeyBuffer := GetKeyBuffer(kiLookup);
  if KeyBuffer^.Exclusive then
    SearchCond := keySEARCHGT else
    SearchCond := keySEARCHGEQ;
  IndexBuffer := AllocMem(KeySize);
  try
    RecBuffer := PAnsiChar(KeyBuffer) + SizeOf(TKeyBuffer);
    UseKey := Engine.ExtractKey(Handle,RecBuffer,IndexBuffer) = 0;
    if UseKey then RecBuffer := IndexBuffer;

    if Engine.GetRecordForKey(Handle, {SearchCond,} UseKey, KeyBuffer^.FieldCount, 0,RecBuffer, nil, False) = 0
       then  Resync([rmCenter]);
  finally
    FreeMem(IndexBuffer, KeySize);
  end;
end;
{$HINTS ON}

procedure TPSQLTable.SetKey;
begin
  SetKeyBuffer(kiLookup, TRUE);
end;

procedure TPSQLTable.EditKey;
begin
  SetKeyBuffer(kiLookup, FALSE);
end;

procedure TPSQLTable.ApplyRange;
begin
  CheckBrowseMode;
  if SetCursorRange then  First;
end;

procedure TPSQLTable.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  if ResetCursorRange then   Resync([]);
end;

procedure TPSQLTable.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiRangeStart, StartValues);
  SetKeyFields(kiRangeEnd, EndValues);
  ApplyRange;
end;

procedure TPSQLTable.SetRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, TRUE);
end;

procedure TPSQLTable.SetRangeStart;
begin
  SetKeyBuffer(kiRangeStart, TRUE);
end;

procedure TPSQLTable.EditRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, FALSE);
end;

procedure TPSQLTable.EditRangeStart;
begin
  SetKeyBuffer(kiRangeStart, FALSE);
end;

procedure TPSQLTable.UpdateRange;
begin
  SetLinkRanges(FMasterLink.Fields);
end;

function TPSQLTable.GetBatchModify: Boolean;
var
  Len : integer;
begin
   if FHandle <> nil then
      Engine.GetEngProp(hDBIObj(FHandle), curAUTOREFETCH,@Result, SizeOf(Result),Len);
end;

procedure TPSQLTable.SetBatchModify(const Value : Boolean);
begin
   if FHandle = nil then Exit;
   if Value then
      Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),curAUTOREFETCH,LongInt(TRUE))) else
      begin
         Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),curAUTOREFETCH,LongInt(FALSE)));
         Refresh;
      end;
end;


function TPSQLTable.GetLookupCursor(const KeyFields: string;
  CaseInsensitive: Boolean): HDBICur;
var
  IndexFound, FieldsIndex: Boolean;
  KeyIndexName, IndexName, IndexTag: string;
  KeyIndex: TIndexDef;
begin
  if (KeyFields <> FLookupKeyFields) or
     (CaseInsensitive <> FLookupCaseIns) then
  begin
    DestroyLookupCursor;
    IndexFound := FALSE;
    FieldsIndex := FALSE;
    { if a range is active, don't use a lookup cursor }
    if not FKeyBuffers[kiCurRangeStart].Modified and
       not FKeyBuffers[kiCurRangeEnd].Modified then
    begin
      if Database.FPseudoIndexes then
      begin
        if not CaseInsensitive then
        begin
          KeyIndexName := KeyFields;
          FieldsIndex := TRUE;
          IndexFound := TRUE;
        end;
      end
      else
      begin
        KeyIndex := IndexDefs.GetIndexForFields(KeyFields, CaseInsensitive);
        if (KeyIndex <> nil) and
           (CaseInsensitive = (ixCaseInsensitive in KeyIndex.Options)) then
        begin
          KeyIndexName := KeyIndex.Name;
          FieldsIndex := FALSE;
          IndexFound := TRUE;
        end;
      end;
      if IndexFound and (Length(KeyFields) < DBIMAXMSGLEN) then
      begin
        Check(Engine, Engine.CloneCursor(Handle, True, False, FLookupHandle));
        GetIndexParams(KeyIndexName, FieldsIndex, IndexName, IndexTag);
        Check(Engine, Engine.SwitchToIndex(FLookupHandle, IndexName, IndexTag, 0, FALSE));
      end;
      FLookupKeyFields := KeyFields;
      FLookupCaseIns := CaseInsensitive;
    end;
  end;
  Result := FLookupHandle;
end;

procedure TPSQLTable.DestroyLookupCursor;
begin
  if FLookupHandle <> nil then
  begin
    Engine.CloseCursor(FLookupHandle);
    FLookupHandle := nil;
    FLookupKeyFields := '';
  end;
end;

procedure TPSQLTable.GotoCurrent(Table: TPSQLTable);
begin
  CheckBrowseMode;
  Table.CheckBrowseMode;
  if (AnsiCompareText(FDatabase.DatabaseName, Table.Database.DatabaseName) <> 0) or
     (AnsiCompareText(TableName, Table.TableName) <> 0) then  DatabaseError(STableMismatch, Self);
  Table.UpdateCursorPos;
  DoBeforeScroll;
  Resync([rmExact, rmCenter]);
  DoAfterScroll;
end;

{$IFNDEF FPC}
procedure TPSQLTable.GetDetailLinkFields(MasterFields, DetailFields: TList{$IFDEF DELPHI_17}<TField>{$ENDIF});
var
  i: Integer;
  Idx: TIndexDef;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and (Self.MasterFields <> '') then
  begin
    Idx := nil;
    MasterSource.DataSet.GetFieldList(MasterFields, Self.MasterFields);
    UpdateIndexDefs;
    if IndexName <> '' then
      Idx := IndexDefs.Find(IndexName)
    else
      if IndexFieldNames <> '' then
        Idx := IndexDefs.GetIndexForFields(IndexFieldNames, FALSE)
      else
        for i := 0 to IndexDefs.Count - 1 do
          if ixPrimary in IndexDefs[i].Options then
          begin
            Idx := IndexDefs[i];
            break;
          end;
    if Idx <> nil then GetFieldList(DetailFields, Idx.Fields);
  end;
end;
{$ENDIF}

procedure TPSQLTable.CheckMasterRange;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    SetLinkRanges(FMasterLink.Fields);
    SetCursorRange;
  end;
end;

procedure TPSQLTable.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  UpdateRange;
  ApplyRange;
end;

procedure TPSQLTable.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;

function TPSQLTable.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TPSQLTable.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FMasterLink.DataSource := Value;
end;

function TPSQLTable.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TPSQLTable.SetMasterFields(const Value: string);
begin
  FMasterLink.FieldNames := Value;
end;

procedure TPSQLTable.DoOnNewRecord;
var
  I: Integer;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    for I := 0 to Pred(FMasterLink.Fields.Count) do
      IndexFields[I] := TField(FMasterLink.Fields[I]);
  Inherited DoOnNewRecord;
end;

// pg: 01.03.2011
procedure TPSQLTable.CreateTable;

  function CreateSQLForCreateTable:string;
  var j : Integer;
  begin
      Result := Format('CREATE TABLE %s ( ',[TableName]);
      for j := 0 to FieldDefs.Count - 1 do
        begin
            Result := Result + BDETOPSQLStr(FieldDefs[j]);
            if j < FieldDefs.Count - 1 then
              Result := Result + ', '
            else
              Result := Result + '); ';
        end;
  end;

var
  i: integer;
begin
  CheckInactive;
  SetDBFlag(dbfTable, True);
  try
    Check(Engine,Engine.QExecDirect(DBHandle, CreateSQLForCreateTable, nil, I));
    //indexes
    for I := 0 to IndexDefs.Count - 1 do
     AddIndex(IndexDefs[I].Name, IndexDefs[I].Fields, IndexDefs[i].Options);
  finally
    SetDBFlag(dbfTable, False);
  end;
end;

procedure TPSQLTable.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    Check(Engine, Engine.EmptyTable(DBHandle, Handle, '', ''));
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end else
  begin
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.EmptyTable(DBHandle, nil, FTableName, ''));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
end;

procedure TPSQLTable.LockTable(LockType: TPSQLLockType; NoWait: boolean);
begin
  CheckActive;
  if not Database.InTransaction then
    DatabaseError('LOCK TABLE can not be used outside the transaction.');
  Check(Engine, Engine.AcqTableLock(Handle, Word(LockType), NoWait));
end;

procedure TPSQLTable.EncodeFieldDesc(var FieldDesc: FLDDesc;
  const Name: string; DataType: TFieldType; Size, Precision: Integer);
begin
  with FieldDesc do
  begin
    //TAnsiToNative(Engine, Name, szName, SizeOf(szName) - 1);
    szName := Name;
    iFldType := FldTypeMap[DataType];
    iSubType := FldSubTypeMap[DataType];
    case DataType of
      ftString, ftFixedChar, ftBytes, ftVarBytes, ftBlob..ftTypedBinary:
        iUnits1 := Size;
      ftBCD:
        begin
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then
            iUnits1 := Precision
          else
            iUnits1 := 32;
          iUnits2 := Size;
        end;
    end;
  end;
end;

procedure TPSQLTable.DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF});
begin
  if Event = depropertyChange then
     IndexDefs.Updated := FALSE;
  Inherited DataEvent(Event, Info);
end;

function TPSQLTable.GetCanModify: Boolean;
begin
  Result := Inherited GetCanModify and not ReadOnly;
end;

function TPSQLTable.GetTableLevel: Integer;
begin
  if Handle <> nil then
    Result := GetIntProp(Engine, Handle, curTABLELEVEL) else
    Result := FTableLevel;
end;

function TPSQLTable.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

function TPSQLTable.IndexDefsStored: Boolean;
begin
  Result := StoreDefs and (IndexDefs.Count > 0);
end;

function TPSQLTable.GetFileName: string;
var
  FDb: Boolean;
begin
  FDb := SetDBFlag(dbfDatabase, TRUE);
  try
      Result := Result + TableName;
  finally
    SetDBFlag(dbfDatabase, FDb);
  end;
end;

function TPSQLTable.GetTableType: TTableType;
begin
  Result := ttDefault;
end;

function TPSQLTable.NativeTableName: PAnsiChar;
begin
  Result := PAnsiChar(AnsiString(FTableName));
end;

procedure TPSQLTable.SetExclusive(Value: Boolean);
begin
  CheckInactive;
  FExclusive := Value;
end;

procedure TPSQLTable.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

procedure TPSQLTable.SetTableName(const Value: TFileName);
begin
  if csReading in ComponentState then
    FTableName := Value
  else
    if FTableName <> Value then
    begin
      CheckInactive;
      //changed by pasha_golub 23.12.04
      FTableName := QuoteIdentifier(Value);
      FNativeTableName[0] := #0;
      DataEvent(dePropertyChange, 0);
    end;
end;

function TPSQLTable.GetTableName: TFileName;
begin
   Result := FTableName;
end;

{ TTable.IProviderSupport }
{$IFNDEF FPC}
function TPSQLTable.PSGetDefaultOrder: TIndexDef;

  function GetIdx(IdxType : TIndexOption) : TIndexDef;
  var
    i: Integer;
    L: TList{$IFDEF DELPHI_17}<TField>{$ENDIF};
  begin
    Result := nil;
    L := nil;
    for i := 0 to IndexDefs.Count - 1 do
      if IdxType in IndexDefs[i].Options then
      try
        Result := IndexDefs[ i ];
        GetFieldList(L, Result.Fields);
        break;
      except
        Result := nil;
      end;
  end;

var
  DefIdx: TIndexDef;
  L: TList{$IFDEF DELPHI_17}<TField>{$ENDIF};
begin
  DefIdx := nil;
  L := nil;
  IndexDefs.Update;
  try
    if (IndexName <> '') then
      DefIdx := IndexDefs.Find(IndexName)
    else
      if (IndexFieldNames <> '') then
        DefIdx := IndexDefs.FindIndexForFields(IndexFieldNames);
    if Assigned(DefIdx) then
      GetFieldList(L, DefIdx.Fields);
  except
    DefIdx := nil;
  end;
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixPrimary);
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixUnique);
  if Assigned(DefIdx) then
  begin
    Result := TIndexDef.Create(nil);
    Result.Assign(DefIdx);
  end
  else
    Result := nil;
end;

function TPSQLTable.PSGetIndexDefs(IndexTypes : TIndexOptions): TIndexDefs;
begin
  Result := GetIndexDefs(IndexDefs, IndexTypes);
end;

function TPSQLTable.PSGetTableName: string;
begin
  Result := TableName;
end;

procedure TPSQLTable.PSSetParams(AParams: TParams);

  procedure AssignFields;
  var
    I: Integer;
  begin
    for I := 0 to AParams.Count - 1 do
      if (AParams[ I ].Name <> '') then
        FieldByName(AParams[ I ].Name).Value := AParams[ I ].Value
      else
        IndexFields[ I ].Value := AParams[ I ].Value;
  end;

begin
  if (AParams.Count > 0) then
  begin
    Open;
    SetRangeStart;
    AssignFields;
    SetRangeEnd;
    AssignFields;
    ApplyRange;
  end
  else
    if Active then
      CancelRange;
  PSReset;
end;

procedure TPSQLTable.PSSetCommandText(const CommandText : string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;

function TPSQLTable.PSGetKeyFields: string;
var
  i, Pos: Integer;
  IndexFound: Boolean;
begin
  Result := inherited PSGetKeyFields;
  if  Result = '' then
  begin
    if not Exists then  Exit;
    IndexFound := FALSE;
    IndexDefs.Update;
    for i := 0 to IndexDefs.Count - 1 do
      if ixUnique in IndexDefs[I].Options then
      begin
        Result := IndexDefs[ I ].Fields;
        IndexFound := (FieldCount = 0);
        if not IndexFound then
        begin
          Pos := 1;
          while (Pos <= Length(Result)) do
          begin
            IndexFound := (FindField(ExtractFieldName(Result, Pos)) <> nil);
            if not IndexFound then
              Break;
          end;
      	end;
        if IndexFound then Break;
      end;
    if not IndexFound then Result := '';
  end;
end;
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
//                         TPSQLBlobStream                                  //
///////////////////////////////////////////////////////////////////////////////
constructor TPSQLBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
var
  OpenMode: DbiOpenMode;
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TPSQLDataSet;
  FFieldNo := FField.FieldNo;

  if not FDataSet.GetActiveRecBuf(FBuffer) then Exit;

  if FDataSet.State = dsFilter then
    DatabaseErrorFmt(SNoFieldAccess, [FField.DisplayName], FDataSet);

  if not FField.Modified then
  begin
    if Mode = bmRead then
    begin
      FCached := FDataSet.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
                 (FField.IsNull or (FDataSet.GetBlobData(FField, FBuffer) <> {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF}));
      OpenMode := dbiReadOnly;
     end
    else
     begin //bmWrite
      FDataSet.SetBlobData(FField, FBuffer, {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF});
      if FField.ReadOnly then DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
      if not (FDataSet.State in [dsEdit, dsInsert]) then DatabaseError(SNotEditing, FDataSet);
      OpenMode := dbiReadWrite;
    end;

    if not FCached then
    begin
      if Mode = bmRead then
       begin
        if FDataSet.State = dsBrowse then
         begin
          FDataSet.GetCurrentRecord(FDataSet.ActiveBuffer);
         end
        else if (FDataSet.State = dsEdit) or (FDataSet.State = dsInsert) then
         begin
          TNativeDataSet(FDataSet.FHandle).PreventRememberBuffer := true; //we just need to read the record without storing in recordbuffer
          FDataSet.GetCurrentRecord(FDataSet.ActiveBuffer);
          TNativeDataSet(FDataSet.FHandle).PreventRememberBuffer := false;
         end;
       end;

      Check(Engine, Engine.OpenBlob(FDataSet.Handle, FBuffer, FFieldNo, OpenMode));
    end;

  end;

  FOpened := TRUE;

  if Mode = bmWrite then Truncate;
end;

destructor TPSQLBlobStream.Destroy;
begin
  if FOpened then
  begin
    if FModified then FField.Modified := TRUE;
    if not FField.Modified and not FCached then Engine.FreeBlob(FDataSet.Handle, FBuffer, FFieldNo);
    Engine.CloseBlob(FDataset.Handle, FFieldNo); //17.08.2009
  end;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    {$IFNDEF FPC}
    Self.FDataSet.InternalHandleException();
    {$ENDIF}
  end;
end;

function TPSQLBlobStream.Engine : TPSQLEngine;
begin
  Result := FDataSet.Engine;
end;

function TPSQLBlobStream.PositionDataset: Boolean;
begin
   Result := True;
end;

{$IFDEF DELPHI_17}
function TPSQLBlobStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
var
  Status: DBIResult;
begin
  Result := 0;
  if FOpened then
  begin
    if FCached then
    begin
      if Count > Size - FPosition then
        Result := Size - FPosition else
        Result := Count;
      if Result > 0 then
      begin
        Move(TRecordBuffer(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer[0], Result);
        Inc(FPosition, Result);
      end;
    end else
    begin
      Status := Engine.GetBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, Buffer, Result);
      case Status of
        DBIERR_NONE, DBIERR_ENDOFBLOB:
          begin
            if FDataset.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
              (FMode = bmRead) and not FField.Modified and (FPosition = FCacheSize) then
            begin
              FCacheSize := FPosition + Result;
              SetLength(FBlobData, FCacheSize);
              Move(Buffer[0], FBlobData[FPosition], Result);
              if FCacheSize = Size then
              begin
                FDataSet.SetBlobData(FField, FBuffer, FBlobData);
                SetLength(FBlobData, 0);
                FCached := True;
                Engine.FreeBlob(FDataSet.Handle, FBuffer, FFieldNo);;
              end;
            end;
            Inc(FPosition, Result);
          end;
        DBIERR_INVALIDBLOBOFFSET:
          {Nothing};
      else
        TDbiError(Engine, Status);
      end;
    end;
  end;
end;
{$ENDIF DELPHI_17}

function TPSQLBlobStream.Read(var Buffer; Count: Longint): Longint;
var
  Status: DBIResult;
  P: Pointer;
begin
  Result := 0;
  P := @Buffer;
  if FOpened then
  begin
    if FCached then
    begin
      if Count > Size - FPosition then
        Result := Size - FPosition else
        Result := Count;
      if Result > 0 then
      begin
        Move(PAnsiChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end;
    end else
    begin
      Status := Engine.GetBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, P, Result);
      case Status of
        DBIERR_NONE, DBIERR_ENDOFBLOB:
          begin
            {if FField.Transliterate then
              TNativeToAnsiBuf(Engine, @Buffer, @Buffer, Result);}
            if FDataset.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
              (FMode = bmRead) and not FField.Modified and (FPosition = FCacheSize) then
            begin
              FCacheSize := FPosition + Result;
              SetLength(FBlobData, FCacheSize);
              Move(Buffer, PAnsiChar(FBlobData)[FPosition], Result);
              if FCacheSize = Size then
              begin
                FDataSet.SetBlobData(FField, FBuffer, FBlobData);
                FBlobData := {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF};
                FCached := TRUE;
                Engine.FreeBlob(FDataSet.Handle, FBuffer, FFieldNo);
              end;
            end;
            Inc(FPosition, Result);
          end;
        DBIERR_INVALIDBLOBOFFSET:
          {Nothing};
      else
        TDbiError(Engine, Status);
      end;
    end;
  end;
end;

function TPSQLBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp, P: Pointer;
begin
  Result := 0;
  P := @Buffer;
  FField.Transliterate := false;
  if FOpened then
  begin
    if FField.Transliterate then
    begin
      GetMem(Temp, Count+1);
      try
        //TAnsiToNativeBuf(Engine, @Buffer, Temp, Count);
        Check(Engine, Engine.PutBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, @Buffer));
      finally
        FreeMem(Temp, Count+1);
      end;
    end else
      Check(Engine, Engine.PutBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, P));
    Inc(FPosition, Count);
    Result := Count;
    FModified := TRUE;
    FDataSet.SetBlobData(FField, FBuffer, {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF});
  end;
end;


function TPSQLBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TPSQLBlobStream.Truncate;
begin
  if FOpened then
  begin
    Check(Engine, Engine.TruncateBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition));
    FModified := TRUE;
    FDataSet.SetBlobData(FField, FBuffer, {$IFDEF DELPHI_12}nil{$ELSE}''{$ENDIF});
  end;
end;

function TPSQLBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer)) else
      Check(Engine, Engine.GetBlobSize(FDataSet.Handle, FBuffer, FFieldNo, Result));
end; 

{ TPSQLQueryDataLink }
constructor TPSQLQueryDataLink.Create(AQuery : TPSQLQuery);
begin
  Inherited Create;
  FQuery := AQuery;
end;

procedure TPSQLQueryDataLink.ActiveChanged;
begin
  if FQuery.Active then FQuery.RefreshParams;
end;

function TPSQLQueryDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FQuery;
end;

procedure TPSQLQueryDataLink.RecordChanged(Field : TField);
begin
  if (Field = nil)and FQuery.Active then FQuery.RefreshParams;
end;

procedure TPSQLQueryDataLink.CheckBrowseMode;
begin
  if FQuery.Active then  FQuery.CheckBrowseMode;
end;

var
  SaveInitProc: Pointer;

procedure InitDBTables;
begin
  if (SaveInitProc <> nil) then
    TProcedure(SaveInitProc);
end;


procedure TPSQLTable.SetShowSystemTables(const Value: boolean);
begin
  if FShowSystemTables <> Value then
   begin
    FShowSystemTables := Value;
    IF not Value and
        ((pos('pg_',FTableName) = 1) or
        (pos('information_schema',FTableName)=1)) then
      TableName := '';
   end;
end;



{ TPSQLStoredProc }
const
  SEmptyprocedureName          = 'procedure name is empty';
  SDatabaseProperty            = '(%s) property Database is not set!';
  SCantCreateWriteBLOB         = 'Can''t create BLOB stream with write permissions on read-only result set!';

procedure TPSQLStoredProc.CloseCursor;
var
  r : DbiResult;
begin
  inherited;

  if FHandle <> nil then
  begin
    r := Engine.CloseCursor(FHandle);
    FHandle := nil;
    if not (csDestroying in ComponentState) then
       Check(Engine, r);
  end;
end;

constructor TPSQLStoredProc.Create(AOwner: TComponent);
begin
  inherited;
	FParams := TPSQLParams.Create(Self);
	FNeedRefreshParams := false;
end;

function TPSQLStoredProc.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
 if Mode = bmRead then
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode)
 else
  raise EPSQLDatabaseError.CreateFmt(SCantCreateWriteBLOB,[]);
end;

function TPSQLStoredProc.CreateCursor(IsExecProc : boolean): HDBICur;
var
  PCursor: phDBICur;
  AffectedRows: integer;
begin
  Result := nil;

  if Database=nil then
    DatabaseError(Format(SDatabaseProperty, [Self.Name]),Self);

  if Length(FProcName) = 0 then
  begin
    DatabaseError(SEmptyprocedureName, Self);
    exit;
	end;

  RefreshParams;

  try
    SetBoolProp(Engine, hDBIStmt(FHandle), stmtLIVENESS, false);//procedure results alway readonly
    while not CheckOpen(Engine.QPrepareProc(GetDBHandle, PChar(FProcName), FParams, hDBIStmt(FHandle))) do
      {Retry};
  except
    if FHandle <> nil then
    begin
      Engine.CloseCursor(FHandle);
      FHandle := nil;
    end;
    raise;
  end;

  if IsExecProc then
    PCursor := nil
  else
    PCursor := @Result;

  Check(Engine, Engine.QSetProcParams(hDBIStmt(FHandle), FParams));

	Check(Engine, Engine.QExec(hDBIStmt(FHandle), PCursor, AffectedRows));
end;

function TPSQLStoredProc.CreateHandle: HDBICur;
begin
	 Result := HDBICur(CreateCursor(false));
end;

procedure TPSQLStoredProc.DataEvent(Event: TDataEvent; Info: {$IFDEF DELPHI_16}NativeInt{$ELSE}LongInt{$ENDIF});
var
  F: TField;
  i: integer;
begin
  inherited DataEvent(Event, Info);
  if (Event = deUpdateState) and (State = dsBrowse) then
    for i := 0 to FParams.Count - 1 do
     if FParams[i].ParamType in [ptOutput, ptInputOutput] then
      begin
        F := FieldByName(FParams[i].Name);
        if Assigned(F) then
          Params[i].Value  := F.Value;
      end;
end;

function TPSQLStoredProc.DescriptionsAvailable: Boolean;
begin
 Result := True;
end;

destructor TPSQLStoredProc.Destroy;
begin
	FParams.Free;
	inherited;
end;

function TPSQLStoredProc.Engine: TPSQLEngine;
begin
  Result := FDataBase.Engine;
end;

procedure TPSQLStoredProc.ExecProc;
begin
  CheckInActive;

  SetDBFlag(dbfExecSQL, TRUE);
  try
		CreateCursor(true);
	finally
		SetDBFlag(dbfExecSQL, FALSE);

    if FHandle <> nil then
    begin
      Check(Engine, Engine.CloseCursor(hDBICur(FHandle)));
      FHandle := nil;
    end;
	end;
end;

function TPSQLStoredProc.GetCanModify: Boolean;
begin
  Result := false; //mi:2006-10-30 we never able modify resultset of stored procedure
end;

function TPSQLStoredProc.GetParamsCount: integer;
begin
	if [csDesigning, csReading] * ComponentState = [] then
		RefreshParams;
  Result := FParams.Count;
end;

function TPSQLStoredProc.GetParamsList: TPSQLParams;
begin
	if [csDesigning, csReading] * ComponentState = [] then
		RefreshParams;
	Result := FParams;
end;

function TPSQLStoredProc.ParamByName(const Value: string): TPSQLParam;
begin
  Result := FParams.ParamByName(Value);
end;

{$IFNDEF FPC}
procedure TPSQLStoredProc.PSExecute;
begin
  inherited;

end;

function TPSQLStoredProc.PSGetParams: TParams;
begin
  Result := FParams;
end;

function TPSQLStoredProc.PSGetTableName: string;
begin
  Result := FProcName;
end;

procedure TPSQLStoredProc.PSSetCommandText(const CommandText: string);
begin
  inherited;

end;

procedure TPSQLStoredProc.PSSetParams(AParams: TParams);
begin
  inherited;

end;
{$ENDIF}

procedure TPSQLStoredProc.RefreshParams;
var
  Desc: ^SPParamDesc;
  ParamName: string;
  ParamDataType: TFieldType;
  List : TList;
  i:integer;
begin
   if not FNeedRefreshParams or not FDatabase.Connected then Exit;
   List := TList.Create;
   try
    FParams.Clear;
    if Engine.OpenStoredProcParams(DBHandle, StoredProcName, FOverload, List) = 0 then
      for i:=0 to List.Count-1 do
       begin
        Desc := List[i];
        with Desc^ do
        begin
          ParamName := szName;
          if (TParamType(eParamType) = ptResult) and (ParamName = '') then
            ParamName := SResultName;
          if uFldType < MAXLOGFLDTYPES then ParamDataType := DataTypeMap[uFldType]
          else ParamDataType := ftUnknown;
          case uFldtype of
            fldFloat:
               if uSubType = fldstMONEY then ParamDataType := ftCurrency;
            fldBlob:
               if (uSubType >= fldstMEMO) and (uSubType <= fldstBFILE) then
                 ParamDataType := BlobTypeMap[uSubType];
          end;
          with TParam(FParams.Add) do
          begin
            ParamType := TParamType(eParamType);
            DataType := ParamDataType;
            Name := string(ParamName);
          end;
        end;
       end;
   finally
    for i:=0 to List.Count-1 do
     begin
      Desc := List[i];
      Dispose(Desc);
     end;
    List.Free;
    FNeedRefreshParams := false;
   end;
end;

procedure TPSQLStoredProc.SetNeedRefreshParams;
begin
  FNeedRefreshParams := true;
end;

procedure TPSQLStoredProc.SetOverload(const Value: cardinal);
begin
	if ([csReading, csLoading] * ComponentState = []) then
    SetNeedRefreshParams();
  FOverload := Value;
  RefreshParams;
	DataEvent(dePropertyChange, 0);
end;

procedure TPSQLStoredProc.SetParamsList(const Value: TPSQLParams);
begin
	FParams.AssignValues(Value);
end;

procedure TPSQLStoredProc.SetProcedureName(const Value: string);
begin
	if ([csReading, csLoading] * ComponentState = []) then
    SetNeedRefreshParams();
	FProcName := Value;
  RefreshParams;
	DataEvent(dePropertyChange, 0);
end;

procedure TPSQLStoredProc.SetProcName(const Value: string);
begin
	if ([csReading, csLoading] * ComponentState = []) then
    SetNeedRefreshParams();
	FProcName := Value;
  RefreshParams;
	DataEvent(dePropertyChange, 0);
end;

function TPSQLTable.GetOffset: Integer;
begin
 Result := FOffset;
end;

procedure TPSQLTable.SetOffset(const Value: Integer);
begin
 FOffset := Value;
end;

procedure TPSQLTable.SetDummyBool(const Value: boolean);
begin
//dummy method for published
end;

procedure TPSQLTable.SetDummyInt(const Value: cardinal);
begin
//dummy method for published
end;

procedure TPSQLTable.SetDummyStr(const Value: string);
begin
//dummy method for published
end;

function TPSQLTable.GetTableSpace: string;
begin
 if FTableSpace = '<DEFAULT>' then
  begin
   if Database.Connected then
     Result := Database.Tablespace
  end
 else
  Result := FTablespace;
end;

procedure TPSQLTable.SetOptions(const Value: TPSQLDatasetOptions);
begin
 if Value = FOptions then Exit;
 inherited;
 if Active then
  begin
   Close;
   Open;
  end;
end;

{ TPSQLParams }
constructor TPSQLParams.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TPSQLParam);
end;

procedure TPSQLParams.AssignValues(Value: TParams);
begin
 inherited;
end;

function TPSQLParams.GetOwner: TPersistent;
begin
 Result := FOwner;
end;

constructor TPSQLParams.Create;
begin
  inherited Create(TPSQLParam);
  FOwner := nil;
end;

function TPSQLParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType; const DataTypeOID: cardinal = 0; Binary: boolean = False): TPSQLParam;
begin
  Result := inherited CreateParam(FldType, ParamName, ParamType) as TPSQLParam;
  Result.DataTypeOID := DataTypeOID;
  Result.Binary := Binary;
end;

function TPSQLParams.GetItem(Index: Integer): TPSQLParam;
begin
 Result := inherited GetItem(index) as TPSQLParam;
end;

function TPSQLParams.ParamByName(const Value: string): TPSQLParam;
begin
  Result := inherited ParamByName(Value) as TPSQLParam;
end;

function TPSQLParams.ParseSQL(SQL: string; DoCreate: Boolean): string;
const
  Literals = ['''', '"', '`'];
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function NameDelimiter: Boolean;
  begin
    Result := CharInSet(CurChar, [' ', ',', ';', ')', #13, #10]);
  end;

  function IsLiteral: Boolean;
  begin
    Result := CharInSet(CurChar, Literals);
  end;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: integer;
    TempBuf: PChar;

    procedure StripChar;
    begin
      if CharInSet(TempBuf^, Literals) then
      begin
        StrMove(TempBuf, TempBuf + 1, Len - 1);
        if CharInSet((TempBuf + (Len-2))^, Literals) then
          (TempBuf + Len-2)^ := #0;
      end;
    end;

  begin
    Len := StrLen(Buffer);
    TempBuf := AllocMem((Len + 1) * SizeOf(Char));
    try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := TempBuf;
    finally
      FreeMem(TempBuf, (Len + 1) * SizeOf(Char));
    end;
  end;

begin
  Result := SQL;
  Value := PChar(Result);
  if DoCreate then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = ':') and not Literal and not CharInSet((CurPos + 1)^, [':', '=', ' ', ',', ';', #9, #13, #10]) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := string(StartPos + 1);
      if DoCreate then
        TParam(Add).Name := Name;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = ':') and not Literal and CharInSet((CurPos + 1)^, [':', '=', ' ', ',', ';', #9, #13, #10]) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

procedure TPSQLParams.SetItem(Index: Integer; const Value: TPSQLParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TPSQLParam }

function TPSQLParam.IsEqual(Value: TParam): Boolean;
begin
  Result := inherited IsEqual(Value);
  if Value is TPSQLParam then
    Result := Result
              and (FDataTypeOid = TPSQLParam(Value).DataTypeOid)
              and (FBinary = TPSQLParam(Value).Binary);
end;

procedure TPSQLParam.SetDataTypeOID(const Value: cardinal);
begin
  FDataTypeOID := Value;
end;

initialization

  if not IsLibrary then
   begin
    SaveInitProc := InitProc;
    InitProc := @InitDBTables;
   end;
  DBList := TList.Create;

finalization

  DBList.Free;

  if not IsLibrary then
    InitProc := SaveInitProc;

end.


