{$I psqldac.inc}
unit PSQLDbTables;


{$R-,T-,H+,X+}
{$C+}
Interface              

Uses  Windows, SysUtils, Graphics, Classes, Controls, Db,
      {$IFDEF DELPHI_9}DbCommon{$ELSE}PSQLCommon{$ENDIF},
      {$IFDEF DELPHI_6}Variants,{$ENDIF}StdVCL, PSQLAccess, PSQLTypes,
      PSQLCP, ExtCtrls;

const
    VERSION : string = '2.4.2-beta';

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
  FldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT,
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN,
    fldUNKNOWN, fldZSTRING{$IFDEF DELPHI_6}, fldDATETIME, fldBCD{$ENDIF}
    {$IFDEF DELPHI_10}, fldZSTRING, fldBLOB, fldDATETIME, fldINT32{$ENDIF});

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY, 0, fldstFIXED, fldstUNICODE,
    0, 0, 0, 0, 0, fldstHBINARY, fldstHMEMO, 0, 0, 0, 0{$IFDEF DELPHI_6} , 0, 0{$ENDIF}
    {$IFDEF DELPHI_10}, fldstFIXED, fldstMEMO, 0, 0 {$ENDIF});

  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftInteger, ftUnknown, ftVarBytes, ftUnknown, ftUnknown,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet
    {$IFDEF DELPHI_6},ftTimeStamp{$ENDIF});

  BlobTypeMap: array[fldstMEMO..fldstBFILE] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftBlob, ftOraClob, ftOraBlob,
    ftBlob, ftBlob);    

type
  TPSQLDACAbout = Class
  end;

  { Forward declarations }
  TPSQLDatabase      = Class;
  TPSQLParams        = TParams;
  TPSQLParam         = TParam;
  TPSQLDatabaseClass = Class of TPSQLDatabase;
  TPSQLDataSet       = Class;
  TPSQLTable         = Class;
  TPSQLTableClass    = Class of TPSQLTable;
  TPSQLQuery         = Class;
  TPSQLQueryClass    = Class of TPSQLQuery;

  { Exception Classes }
  EPSQLDatabaseError =  Class(EDatabaseError)
    Private
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
    Public
      constructor Create(Engine : TPSQLEngine; ErrorCode: Word);
      destructor Destroy; Override;
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

  TDatabaseNoticeEvent = procedure (Sender: TPSQLDatabase; Message: string) of object;
  TBaseDatabaseLoginEvent = Procedure(Database: TPSQLDatabase; LoginParams: TStrings) of object;

  TDBFlags = set of 0..15;

  TTransIsolation = (tiDirtyRead, tiReadCommitted, tiRepeatableRead);

  TPSQLDatabase =  Class(TCustomConnection)
    Private
      FAbout   : TPSQLDACAbout;
      FTransIsolation: TTransIsolation;
      FKeepConnection: Boolean; //AutoStop
      FOEMConvert : Boolean;  //OEM->ANSI
      FDatabaseName: String; //DatabaseName
      FCharSet: string;
      FUserName : String; //Username
      FUserPassword : String; //UserPassword
      FPort : Cardinal; //Port
      FConnectionTimeout: Cardinal;
      FCommandTimeout: cardinal;
      FHost : String;
      FUseSSL : Boolean; //use SSL connection
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
      //addon info for DB
      FOwner: string;
      FIsTemplate: boolean;
      FTablespace: string;
      FDatabaseID: cardinal;
      FComment: string;
      FServerVersion: string;
      //->>addon
      FOnAdd: TNotifyEvent;
      FOnLogin: TBaseDatabaseLoginEvent;
      FOnNotice: TDatabaseNoticeEvent;
      FNotifyList: TList;  //List of notify
      FDirectQueryList : TList;
      FCheckIfActiveOnParamChange: boolean;
      FSSLMode: TSSLMode;
      function GetNotifyItem(Index: Integer): TObject;
      function GetNotifyCount: Integer;
      Procedure CheckActive;
      Procedure CheckInactive;
      Procedure CheckDatabase(var Password: String);
      Procedure ClearStatements;
      Procedure EndTransaction(TransEnd: EXEnd);
      Function GetInTransaction: Boolean;
      function GetTransactionStatus: TTransactionStatusType;
      function GetServerVersionAsInt: integer;
      Procedure Login(LoginParams: TStrings);
      Procedure ParamsChanging(Sender: TObject);
      Procedure SetDatabaseFlags;
      Procedure SetDatabaseName(const Value: String);
      Procedure SetUserName(const Value: String);
      Procedure SetUserPassword(const Value: String);
      Procedure SetServerPort(const Value: Cardinal);
      Procedure SetConnectionTimeout(const Value: cardinal);
      Procedure SetCommandTimeout(const Value: cardinal);
      procedure SetHost(const Value : String);
      procedure SetUseSSL(const Value : Boolean);
      Procedure SetKeepConnection(Value: Boolean);
      Procedure SetExclusive(Value: Boolean);
      Procedure SetHandle(Value: HDBIDB);
      procedure SetParams(Value: TStrings);
      Procedure SetReadOnly(Value: Boolean);
      procedure SetDummyStr(Value: string);
      procedure SetDummyBool(Value: boolean);
      procedure SetDummyInt(Value: cardinal);
      procedure SetSSLMode(const Value: TSSLMode);
    Protected
      Procedure CloseDatabaseHandle;
      procedure CloseDatabase(Database: TPSQLDatabase);
      Procedure DoConnect; override;
      Procedure DoDisconnect; override;
      Function GetConnected: Boolean; override;
      Function GetDataSet(Index: Integer): TPSQLDataSet; reintroduce;
      Procedure Loaded; Override;
      Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
      Procedure InitEngine; //Init SQL Engine
      Procedure AddDatabase(Value : TPSQLDatabase);
      Procedure RemoveDatabase(Value : TPSQLDatabase);
      property CheckIfActiveOnParamChange: boolean read FCheckIfActiveOnParamChange write FCheckIfActiveOnParamChange;
    Public
      constructor Create(AOwner: TComponent); Override;
      destructor Destroy; Override;

      function  Engine : TPSQLEngine;
      function Execute(const SQL: string; Params: TParams = NIL; Cache: Boolean = FALSE; Cursor: phDBICur = NIL): Integer;
      function GetBackendPID: Integer;
      function GetCharSet: string;
      function SelectString(aSQL : string; var IsOk : boolean; aFieldName : string):string; overload;
      function SelectString(aSQL : string; var IsOk : boolean; aFieldNumber : integer = 0):string; overload;
      function SelectStringDef(aSQL : string; aDefaultValue : string; aFieldName : string):string; overload;
      function SelectStringDef(aSQL : string; aDefaultValue : string; aFieldNumber : integer = 0):string; overload;

      procedure AddNotify(AItem: TObject);
      procedure ApplyUpdates(const DataSets: array of TPSQLDataSet);
      procedure CancelBackend(PID : Integer);
      procedure CloseDataSets;
      procedure CloseNotify;
      procedure Commit;
      procedure GetCharsets(List: TStrings);
      procedure GetDatabases(Pattern: String;List : TStrings);
      procedure GetSchemaNames(Pattern: String; SystemSchemas: Boolean; List: TStrings);
      procedure GetStoredProcNames(Pattern: String; List: TStrings);
      procedure GetTableNames(Pattern: String; SystemTables: Boolean; List: TStrings);
      procedure GetTablespaces(Pattern: String; List: TStrings);
      procedure GetUserNames(Pattern: String; List: TStrings);
      procedure RegisterDirectQuery(aDirectQuery : TObject);
      procedure RemoveNotify(AItem: TObject);
      procedure Reset;
      procedure Rollback;
      procedure SetCharSet(CharSet: string);
      procedure StartTransaction;
      procedure UnregisterDirectQuery(aDirectQuery : TObject);

      property DataSets[Index: Integer]: TPSQLDataSet read GetDataSet;
      property Handle: HDBIDB read FHandle write SetHandle;
      property InTransaction: Boolean read GetInTransaction;
      property Notifies[Index: Integer]: TObject read GetNotifyItem;
      property NotifyCount: Integer read GetNotifyCount;
      property ServerVersionAsInt: integer read GetServerVersionAsInt;
      property Temporary: Boolean read FTemporary write FTemporary;
      property TransactionStatus: TTransactionStatusType read GetTransactionStatus;
    Published
      property About : TPSQLDACAbout read FAbout write FAbout;
      property AfterConnect;
      property AfterDisconnect;
      property BeforeConnect;
      property BeforeDisconnect;
      property CharSet: string read FCharSet write SetCharSet;
      property CommandTimeout: cardinal read FCommandTimeout write SetCommandTimeout default 0;
      property Comment: string read FComment write SetDummyStr stored False;
      property Connected;
      property ConnectionTimeout: cardinal read FConnectionTimeout write SetConnectionTimeout default 15;
      property DatabaseID: cardinal read FDatabaseID write SetDummyInt stored False;
      property DatabaseName: String read FDatabaseName write SetDatabaseName;
      property Exclusive: Boolean read FExclusive write SetExclusive default FALSE;
      property HandleShared: Boolean read FHandleShared write FHandleShared default FALSE;
      property Host : String read FHost write SetHost;
      property IsTemplate: boolean read FIsTemplate write SetDummyBool stored False;
      property KeepConnection: Boolean read FKeepConnection write SetKeepConnection default TRUE;
      property LoginPrompt;
      property OEMConvert: Boolean read FOEMConvert write FOEMConvert default False;
      property OnAdd: TNotifyEvent read FOnAdd;
      property OnLogin: TBaseDatabaseLoginEvent read FOnLogin write FOnLogin;
      property OnNotice: TDatabaseNoticeEvent read FOnNotice write FOnNotice;
      property Owner: string read FOwner write SetDummyStr stored False;
      property Params: TStrings read FParams write SetParams;
      property Port : Cardinal read FPort write SetServerPort default PSQL_PORT;
      property ReadOnly: Boolean read FReadOnly write SetReadOnly default FALSE;
      property ServerVersion: string read FServerVersion write SetDummyStr stored False;
      property SSLMode: TSSLMode read FSSLMode write SetSSLMode default sslPrefer;
      property Tablespace: string read FTablespace write SetDummyStr stored False;
      property TransIsolation: TTransIsolation read FTransIsolation write FTransIsolation default tiReadCommitted;
      property UserName : String read FUserName write SetUserName;
      property UserPassword : String read FUserPassword write SetUserPassword;
      property UseSSL : Boolean read FUseSSL write SetUseSSL default True;
  end;

  { TPSQLBDECallBack }
  TPSQLBDECallbackEvent = Function(CBInfo: Pointer): CBRType of Object;

  TPSQLBDECallBack = Class(TObject)
    Private
      FHandle: hDBICur;
      FOwner: TObject;
      FCBType: CBType;
      FOldCBData: Longint;
      FOldCBFunc: pfDBICallBack;
      FCallbackEvent: TPSQLBDECallbackEvent;
      FEngine : TPSQLEngine;
    Protected
      Function Invoke(CallType: CBType; CBInfo: Pointer): CBRType;
    Public
      constructor Create(Engine : TPSQLEngine; AOwner: TObject; Handle: hDBICur; CBType: CBType;
        CBBuf: Pointer; CBBufSize: Integer; CallbackEvent: TPSQLBDECallbackEvent;Chain: Boolean);
      destructor Destroy; Override;
  end;

  TFieldDescList = array of FLDDesc;

    { TLocale }

  TLocale = Pointer;

  
  TRecNoStatus = (rnDbase, rnParadox, rnNotSupported);

  TPSQLSQLUpdateObject = class(TComponent)
  protected
     Function GetDataSet: TPSQLDataSet; Virtual; Abstract;
     Procedure SetDataSet(ADataSet: TPSQLDataSet); Virtual; Abstract;
     Procedure Apply(UpdateKind: TUpdateKind); Virtual; Abstract;
     Function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual; abstract;
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
  Private
    FAbout : TPSQLDACAbout;
    FRecProps: RecProps; //Record properties
   // FStmtHandle: HDBIStmt; //Statement handle pg 21/02/06
    FExprFilter: HDBIFilter; //Filter expression
    FFuncFilter: HDBIFilter; // filter function
    FFilterBuffer: PChar; // filter buffer
    FIndexFieldMap: DBIKey; //Index field map
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
    FCacheBlobs: Boolean;
    FKeySize: Word;
    FUpdateCBBuf: PDELAYUPDCbDesc;
    FUpdateCallback: TPSQLBDECallBack;
    FKeyBuffers: array[TKeyIndex] of PKeyBuffer;
    FKeyBuffer: PKeyBuffer;
    FRecNoStatus: TRecNoStatus;
    FIndexFieldCount: Integer;
    FRecordSize: Word;
    FBookmarkOfs: Word;
    FRecInfoOfs: Word;
    FBlobCacheOfs: Word;
    FRecBufSize: Word;
    FBlockBufOfs: Integer;
    FLastParentPos: Integer;
    FBlockReadBuf: PChar;
    FBlockBufSize: Integer;
    FBlockBufCount: Integer;
    FBlockReadCount: Integer;
    FOldBuffer : PChar;
    FParentDataSet: TPSQLDataSet;
    FUpdateObject: TPSQLSQLUpdateObject;
    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    FAutoRefresh: Boolean;
    FDBFlags: TDBFlags;
    FUpdateMode: TUpdateMode;
    FDatabase: TPSQLDatabase;
    FAllowSequenced : Boolean;  //Add by Nicolas Ring
    FByteaAsEscString: boolean;
    FOIDAsInt: boolean;
    FSortFieldNames: string;
    Procedure ClearBlobCache(Buffer: PChar);
    Function GetActiveRecBuf(var RecBuf: PChar): Boolean;
    Function GetBlobData(Field: TField; Buffer: PChar): TBlobData;
    Function GetOldRecord: PChar;
    Procedure InitBufferPointers(GetProps: Boolean);
    Function RecordFilter(RecBuf: Pointer; RecNo: Integer): Smallint; stdcall;
    Procedure SetBlobData(Field: TField; Buffer: PChar; Value: TBlobData);
    Function GetDBHandle: HDBIDB;
    Procedure SetUpdateMode(const Value: TUpdateMode);
    Procedure SetAutoRefresh(const Value: Boolean);
    procedure SetDatabase(Value : TPSQLDatabase);
    function GetDatabase:TPSQLDatabase;
    {$IFNDEF DELPHI_4}
    Procedure SetupAutoRefresh;
    {$ENDIF}
    procedure SetByteaAsEscString(const Value: boolean); virtual;
    procedure SetOIDAsInt(const Value: boolean); virtual;
    function GetStmtHandle: HDBIStmt;
    procedure SetSortFieldNames(const Value: string);
    function GetSortFieldNames: string;


  protected
    FHandle: HDBICur;  //cursor handle // to make it visible to PSQLUser
      {$IFNDEF DELPHI_4}
      { IProviderSupport }
      Procedure PSEndTransaction(Commit: Boolean); override;
      Function PSExecuteStatement(const ASQL: string; AParams: TParams;
        ResultSet: Pointer = NIL): Integer; override;
      Procedure PSGetAttributes(List: TList); override;
      Function PSGetQuoteChar: string; override;
      Function PSInTransaction: Boolean; override;
      Function PSIsSQLBased: Boolean; override;
      Function PSIsSQLSupported: Boolean; override;
      Procedure PSStartTransaction; override;
      Procedure PSReset; override;
      Function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
  Protected
    {$ENDIF}
    Function  Engine : TPSQLEngine; Virtual; Abstract;
    procedure SetBlockReadSize(Value: Integer); override;
    procedure BlockReadNext; override;
    {$IFDEF DELPHI_4}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; Override;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,  Decimals: Integer): Boolean; Override;
    {$ENDIF}
    Procedure Notification(AComponent: TComponent; Operation: TOperation); Override;
    Procedure ActivateFilters;
    Procedure AddFieldDesc(FieldDescs: TFieldDescList; var DescNo: Integer;
      var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
    Procedure AllocCachedUpdateBuffers(Allocate: Boolean);
    Procedure AllocKeyBuffers;
    Function  AllocRecordBuffer: PChar; Override;
    Function  CachedUpdateCallBack(CBInfo: Pointer): CBRType;
    Procedure CheckCachedUpdateMode;    
    Procedure CheckSetKeyMode;
    Procedure ClearCalcFields(Buffer: PChar); Override;
    Procedure CloseCursor; Override;
    Procedure CloseBlob(Field: TField); Override;
    Function  CreateExprFilter(const Expr: String;
      Options: TFilterOptions; Priority: Integer): HDBIFilter;
    Function  CreateFuncFilter(FilterFunc: Pointer;
      Priority: Integer): HDBIFilter;
    Function  CreateHandle: HDBICur; Virtual;
    Function  CreateLookupFilter(Fields: TList; const Values: Variant;
      Options: TLocateOptions; Priority: Integer): HDBIFilter;
    Procedure DataEvent(Event: TDataEvent; Info: Longint); Override;
    Procedure DeactivateFilters;
    Procedure DestroyHandle; Virtual;
    Procedure DestroyLookupCursor; Virtual;
    Function  FindRecord(Restart, GoForward: Boolean): Boolean; Override;
    Function  ForceUpdateCallback: Boolean;
    Procedure FreeKeyBuffers;
    Procedure FreeRecordBuffer(var Buffer: PChar); Override;
    Procedure GetBookmarkData(Buffer: PChar; Data: Pointer); Override;
    Function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; Override;
    Function  GetCanModify: Boolean; Override;
    Function  GetFieldFullName(Field: TField): string; override;
    Function  GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    Function  GetIndexField(Index: Integer): TField;
    Function  GetIndexFieldCount: Integer;
    Function  GetIsIndexField(Field: TField): Boolean; Override;
    Function  GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
    Function  GetKeyExclusive: Boolean;
    Function  GetKeyFieldCount: Integer;
    Function  GetLookupCursor(const KeyFields: String; CaseInsensitive: Boolean): HDBICur; Virtual;
    Function  GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; Override;
    Function  GetRecordCount: Integer; Override;
    Function  GetRecNo: Integer; Override;
    Function  GetRecordSize: Word; Override;
    Function  GetStateFieldValue(State: TDataSetState; Field: TField): Variant; Override;
    Procedure GetObjectTypeNames(Fields: TFields);
    Function  GetUpdatesPending: Boolean;
    Function  GetUpdateRecordSet: TUpdateRecordTypes;
    Function  InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
    Procedure InitRecord(Buffer: PChar); Override;
    Procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); Override;
    Procedure InternalCancel; Override;
    Procedure InternalClose; Override;
    Procedure InternalDelete; Override;
    Procedure InternalEdit; Override;
    Procedure InternalFirst; Override;
    Procedure InternalGotoBookmark(Bookmark: TBookmark); Override;
    Procedure InternalHandleException; Override;
    Procedure InternalInitFieldDefs; Override;
    Procedure InternalInitRecord(Buffer: PChar); Override;
    Procedure InternalInsert; override;
    Procedure InternalLast; Override;
    Procedure InternalOpen; Override;
    Procedure InternalPost; Override;
    Procedure InternalRefresh; Override;
    Procedure InternalSetToRecord(Buffer: PChar); Override;
    Function  IsCursorOpen: Boolean; Override;
    Function  LocateRecord(const KeyFields: String; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    Function  LocateNearestRecord(const KeyFields: String; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Word;
    Function  MapsToIndex(Fields: TList; CaseInsensitive: Boolean): Boolean;
    Procedure PostKeyBuffer(Commit: Boolean);
    Procedure PrepareCursor; Virtual;
    Function  ProcessUpdates(UpdCmd: DBIDelayedUpdCmd): Word;
    Function  ResetCursorRange: Boolean;
    Procedure SetBookmarkData(Buffer: PChar; Data: Pointer); Override;
    Procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); Override;
    Procedure SetCachedUpdates(Value: Boolean);
    Function  SetCursorRange: Boolean;
    Procedure SetFieldData(Field: TField; Buffer: Pointer); Override;
    Procedure SetFilterData(const Text: String; Options: TFilterOptions);
    Procedure SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
    Procedure SetFiltered(Value: Boolean); Override;
    Procedure SetFilterOptions(Value: TFilterOptions); Override;
    Procedure SetFilterText(const Value: String); Override;
    Procedure SetIndexField(Index: Integer; Value: TField);
    Procedure SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
    Procedure SetKeyExclusive(Value: Boolean);
    Procedure SetKeyFieldCount(Value: Integer);
    Procedure SetKeyFields(KeyIndex: TKeyIndex; const Values: array of const);
    Procedure SetLinkRanges(MasterFields: TList);
    Procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); Override;
    Procedure SetOnFilterRecord(const Value: TFilterRecordEvent); Override;
    procedure SetOnUpdateError(UpdateEvent: TUpdateErrorEvent);    
    Procedure SetRecNo(Value: Integer); Override;
    Procedure SetupCallBack(Value: Boolean);
    Procedure SetUpdateRecordSet(RecordTypes: TUpdateRecordTypes);
    Procedure SetUpdateObject(Value: TPSQLSQLUpdateObject);
    Procedure SwitchToIndex(const IndexName, TagName: String);
    Function  UpdateCallbackRequired: Boolean;
    Procedure Disconnect; Virtual;
    Procedure OpenCursor(InfoQuery: Boolean); Override;
    Function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; virtual;
    function GetHandle: HDBICur;
    property DBFlags: TDBFlags read FDBFlags;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode default upWhereAll;
    property StmtHandle: HDBIStmt read GetStmtHandle;
    property ByteaAsEscString: boolean read FByteaAsEscString
        write SetByteaAsEscString;
    property OIDAsInt: boolean read FOIDAsInt write SetOIDAsInt;
  Public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; Override;
    function GetLastInsertID(const FieldNum: integer): integer;
    Procedure ApplyUpdates;
    Function  BookmarkValid(Bookmark: TBookmark): Boolean; Override;
    Procedure Cancel; Override;
    Procedure CancelUpdates;
    property  CacheBlobs: Boolean read FCacheBlobs write FCacheBlobs default TRUE;
    Function  CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; Override;
    Procedure CommitUpdates;
    Procedure FetchAll;
    Procedure FlushBuffers;
    Function GetCurrentRecord(Buffer: PChar): Boolean; Override;
    Function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; override;
    Function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    Function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override;
    Procedure GetIndexInfo;
    Function  Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; Override;
    Function  Lookup(const KeyFields: String; const KeyValues: Variant;
      const ResultFields: String): Variant; Override;
    Function  IsSequenced: Boolean; Override;
    Procedure Post; Override;
    Procedure RevertRecord;
    Function  UpdateStatus: TUpdateStatus; Override;
    Function  Translate(Src, Dest: PChar; ToOem: Boolean) : Integer;  Override;
    Function CheckOpen(Status: Word): Boolean;
    Procedure CloseDatabase(Database: TPSQLDatabase);
    Procedure GetDatabaseNames(List: TStrings);
    property DBHandle: HDBIDB read GetDBHandle;
    property Handle: HDBICur read GetHandle;
    property ExpIndex: Boolean read FExpIndex;
    property KeySize: Word read FKeySize;
    property UpdateObject: TPSQLSQLUpdateObject read FUpdateObject write SetUpdateObject;
    property UpdatesPending: Boolean read GetUpdatesPending;
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordSet write SetUpdateRecordSet;
 	  procedure SortBy(FieldNames : string);
	  property SortFieldNames : string read GetSortFieldNames write SetSortFieldNames;    
  Published
    property About : TPSQLDACAbout read FAbout write FAbout;
    property AutoRefresh: Boolean read FAutoRefresh write SetAutoRefresh default FALSE;
    property Database: TPSQLDatabase read GetDatabase write SetDatabase;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates default False;
    property AllowSequenced : Boolean read FAllowSequenced Write FAllowSequenced default False; //Added by Nicolas Ring
    property Filter;
    property Filtered;
    property FilterOptions;
    property OnFilterRecord;
    property Active;
    property AutoCalcFields;
    property ObjectView default FALSE;
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
    {$IFNDEF DELPHI_4}
    property BeforeRefresh;
    property AfterRefresh;
    {$ENDIF}
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write SetOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;

//////////////////////////////////////////////////////////
//Class       : TPSQLTable
//Description : TPSQLTable class
//////////////////////////////////////////////////////////
  TPSQLLockType = (mltReadLock, mltWriteLock);

  TTableType = (ttDefault, ttParadox, ttDBase, ttFoxPro, ttASCII);
  TIndexName = type string;

  TIndexDescList = array of IDXDesc;

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
    FLookupKeyFields: String;
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
    Procedure CheckMasterRange;
    Procedure DecodeIndexDesc(const IndexDesc: IDXDesc;
      var Source, Name, FieldExpression, DescFields: string;
      var Options: TIndexOptions);
    Function FieldDefsStored: Boolean;
    Function GetExists: Boolean;
    Function GetIndexFieldNames: String;
    Function GetIndexName: String;
    Procedure GetIndexParams(const IndexName: String; FieldsIndex: Boolean;
      var IndexedName, IndexTag: String);
    Function GetMasterFields: String;
    Function GetTableTypeName: PChar;
    Function GetTableLevel: Integer;
    Function IndexDefsStored: Boolean;
    Procedure MasterChanged(Sender: TObject);
    Procedure MasterDisabled(Sender: TObject);
    Procedure SetDataSource(Value: TDataSource);
    Procedure SetExclusive(Value: Boolean);
    Procedure SetIndexDefs(Value: TIndexDefs);
    Procedure SetIndex(const Value: String; FieldsIndex: Boolean);
    Procedure SetIndexFieldNames(const Value: String);
    Procedure SetIndexName(const Value: String);
    Procedure SetMasterFields(const Value: String);
    Procedure SetReadOnly(Value: Boolean);
    Procedure SetTableLock(LockType: TPSQLLockType; Lock: Boolean);
    Procedure SetTableName(const Value: TFileName);
    function GetTableName: TFileName;
    Procedure UpdateRange;
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
    {$IFNDEF DELPHI_4}
    { IProviderSupport }
    Function PSGetDefaultOrder: TIndexDef; override;
    Function PSGetKeyFields: string; override;
    Function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    Procedure PSSetCommandText(const CommandText: string); override;
    Procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}
    Function CreateHandle: HDBICur; Override;
    Procedure DataEvent(Event: TDataEvent; Info: Longint); Override;
    Procedure DefChanged(Sender: TObject); override;
    Procedure DestroyHandle; Override;
    Procedure DestroyLookupCursor; Override;
    Procedure DoOnNewRecord; Override;
    Procedure EncodeFieldDesc(var FieldDesc: FLDDesc;
      const Name: string; DataType: TFieldType; Size, Precision: Integer);
    Procedure EncodeIndexDesc(var IndexDesc: IDXDesc;
      const Name, FieldExpression: string; Options: TIndexOptions;
      const DescFields: string = '');
    Function GetCanModify: Boolean; Override;
    Function GetDataSource: TDataSource; Override;
    Function GetHandle(const IndexName, IndexTag: String): HDBICur;
    Function GetLanguageDriverName: String;
    Function GetLookupCursor(const KeyFields: String;
      CaseInsensitive: Boolean): HDBICur; Override;
    Procedure InitFieldDefs; Override;
    Function GetFileName: string;
    Function GetTableType: TTableType;
    Function NativeTableName: PChar;
    Procedure PrepareCursor; Override;
    Procedure UpdateIndexDefs; Override;
    property MasterLink: TMasterDataLink read FMasterLink;
    procedure SetByteaAsEscString(const Value: boolean); override;
    procedure SetOIDAsInt(const Value: boolean); override;
  Public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; Override;
    Function  Engine : TPSQLEngine; Override;
    Function  CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; Override;
    Function  IsSequenced: Boolean; Override;
    Procedure AddIndex(const Name, Fields: string; Options: TIndexOptions; const DescFields: string = '');
    Procedure ApplyRange;
    Procedure CancelRange;
    procedure CreateTable;
    Procedure DeleteIndex(const Name: String);
    Procedure EditKey;
    Procedure EditRangeEnd;
    Procedure EditRangeStart;
    Procedure EmptyTable;
    Function FindKey(const KeyValues: array of const): Boolean;
    Procedure FindNearest(const KeyValues: array of const);
    Procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    Procedure GetIndexNames(List: TStrings);
    Procedure GotoCurrent(Table: TPSQLTable);
    Function GotoKey: Boolean;
    Procedure GotoNearest;
    Procedure LockTable(LockType: TPSQLLockType);
    Procedure SetKey;
    Procedure SetRange(const StartValues, EndValues: array of const);
    Procedure SetRangeEnd;
    Procedure SetRangeStart;
    Procedure UnlockTable;
    property Exists: Boolean read GetExists;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;
    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;
    property KeyFieldCount: Integer read GetKeyFieldCount write SetKeyFieldCount;
    property TableLevel: Integer read GetTableLevel write FTableLevel;
    property BatchModify : Boolean read GetBatchModify write SetBatchModify default False;
  Published
    property DefaultIndex: Boolean read FDefaultIndex write FDefaultIndex default TRUE;
    property Exclusive: Boolean read FExclusive write SetExclusive default FALSE;
    property FieldDefs stored FieldDefsStored;
    property IndexDefs: TIndexDefs read FIndexDefs write SetIndexDefs stored IndexDefsStored;
    property IndexFieldNames: String read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: String read GetIndexName write SetIndexName;
    property MasterFields: String read GetMasterFields write SetMasterFields;
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
    property OIDAsInt;
    property ByteaAsEscString;
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
      FText: String;
      FDataLink: TDataLink;
      FLocal: Boolean;
      FRowsAffected: Integer;
      FUniDirectional: Boolean;
      FRequestLive: Boolean;
      FSQLBinary: PChar;
      FParamCheck: Boolean;
      FExecSQL: Boolean;
      FCheckRowsAffected: Boolean;
      Function CreateCursor(GenHandle: Boolean): HDBICur;
      Function GetQueryCursor(GenHandle: Boolean): HDBICur;
      Function GetRowsAffected: Integer;
      Procedure PrepareSQL(Value: PChar);
      Procedure QueryChanged(Sender: TObject);
      Procedure ReadBinaryData(Stream: TStream);
      Procedure ReadParamData(Reader: TReader);
      Procedure RefreshParams;
      Procedure SetDataSource(Value: TDataSource);
      Procedure SetQuery(Value: TStrings);
      function GetQuery:TStrings;
      Procedure SetParamsList(Value: TPSQLParams);
      Procedure SetParamsFromCursor;
      Procedure SetPrepared(Value: Boolean);
      Procedure SetPrepare(Value: Boolean);
      Procedure WriteBinaryData(Stream: TStream);
      Procedure WriteParamData(Writer: TWriter);
      procedure SetRequestLive(const Value : Boolean);
      function GetRequestLive : Boolean;
    protected
      {$IFNDEF DELPHI_4}
      { IProviderSupport }
      Procedure PSExecute; override;
      Function PSGetDefaultOrder: TIndexDef; override;
      Function PSGetParams: TParams; override;
      Function PSGetTableName: string; override;
      Procedure PSSetCommandText(const CommandText: string); override;
      Procedure PSSetParams(AParams: TParams); override;
      {$ENDIF}
      Function CreateHandle: HDBICur; Override;
      Procedure DefineProperties(Filer: TFiler); Override;
      Procedure Disconnect; Override;
//      Procedure FreeStatement; virtual;
      Function GetDataSource: TDataSource; Override;
      Function GetParamsCount: Word;
      Function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
      Procedure GetStatementHandle(SQLText: PChar); virtual;
      property DataLink: TDataLink read FDataLink;
      procedure SetByteaAsEscString(const Value: boolean); override;
      procedure SetOIDAsInt(const Value: boolean); override;
    Public
      constructor Create(AOwner: TComponent); Override;
      destructor Destroy; Override;
      Function  Engine : TPSQLEngine; Override;
      Function  CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream; Override;
      Function  IsSequenced: Boolean; Override;
      Procedure ExecSQL;
      Procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
      Function ParamByName(const Value: String): TPSQLParam;
      Procedure Prepare;
      Procedure UnPrepare;
      property Prepared: Boolean read FPrepared write SetPrepare;
      property ParamCount: Word read GetParamsCount;
      property Local: Boolean read FLocal;
      property Text: String read FText;
      property RowsAffected: Integer read GetRowsAffected;
      property SQLBinary: PChar read FSQLBinary write FSQLBinary;
    Published
      property DataSource: TDataSource read GetDataSource write SetDataSource;
      property ParamCheck: Boolean read FParamCheck write FParamCheck default TRUE;
      property RequestLive: Boolean read GetRequestLive write SetRequestLive default FALSE;
      property SQL: TStrings read GetQuery write SetQuery;
      property Params: TPSQLParams read FParams write SetParamsList;
      property UniDirectional: Boolean read FUniDirectional write FUniDirectional default FALSE;
      property UpdateMode;
      property UpdateObject;
      property OIDAsInt;
      property ByteaAsEscString;
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
    Function GetQuery(UpdateKind: TUpdateKind): TPSQLQuery;
    Function GetSQLIndex(Index: Integer): TStrings;
    Procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    Procedure SetSQLIndex(Index: Integer; Value: TStrings);
  Protected
    Function GetSQL(UpdateKind: TUpdateKind): TStrings; Override;
    Function GetQueryClass : TPSQLQueryClass;
    Function GetDataSet: TPSQLDataSet; Override;
    Procedure SetDataSet(ADataSet: TPSQLDataSet); Override;
    Procedure SQLChanged(Sender: TObject);
  Public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; Override;
    Procedure Apply(UpdateKind: TUpdateKind); Override;
    Procedure ExecSQL(UpdateKind: TUpdateKind);
    Procedure SetParams(UpdateKind: TUpdateKind);
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
    Private
      FField: TBlobField;
      FDataSet: TPSQLDataSet;
      FBuffer: PChar;
      FMode: TBlobStreamMode;
      FFieldNo: Integer;
      FOpened: Boolean;
      FModified: Boolean;
      FPosition: Longint;
      FBlobData: TBlobData;
      FCached: Boolean;
      FCacheSize: Longint;
      Function GetBlobSize: Longint;
    Public
      constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
      destructor Destroy; Override;
      Function Engine : TPSQLEngine;
      Function PositionDataset: Boolean;
      Function Read(var Buffer; Count: Longint): Longint; Override;
      Function Write(const Buffer; Count: Longint): Longint; Override;
      Function Seek(Offset: Longint; Origin: Word): Longint; Override;
      Procedure Truncate;
  end;


//****************************************************************************//
//                  TPSQLNotify Object                                        //
//                  Asynchronous notifying                                    //
//****************************************************************************//
  TPSQLNotifyEvent = procedure (Sender: TObject; Event: string; ProcessID : Integer) of object;

  TPSQLNotify = class (TComponent)
  private
    FHandle : hDBIObj;  //Handle
    FActive : Boolean;
    FAutoOpen: Boolean;
    FListenList: TStrings;
    FTimer: TTimer;
    FDatabase: TPSQLDatabase;
    FBackupList: TStringList;
    FFirstConnect: Boolean;
    FNotifyFired: TPSQLNotifyEvent;
  protected
    procedure SetActive(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetListenList(Value: TStrings);
    procedure SetDatabase(Value: TPSQLDatabase);
    procedure ListenProc(Sender: TObject);
    procedure CheckEvents;
    procedure ListenChange(Sender: TObject);
    procedure ListenChanging(Sender: TObject);
    procedure CheckActive;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    Function  Engine : TPSQLEngine;
    procedure OpenNotify;
    procedure CloseNotify;
    function CreateHandle : hDBIObj;
    property Handle: hDBIObj read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ListenTo(Event: string);
    procedure SendNotify(Event: string);
    procedure UnlistenTo(Event: string);
  published
    property Database: TPSQLDatabase read FDatabase write SetDatabase;
    property Active: Boolean read FActive write SetActive;
    property ListenList: TStrings read FListenList write SetListenList;
    property Interval: Cardinal read GetInterval write SetInterval default 250;
    property OnNotify: TPSQLNotifyEvent read FNotifyFired write FNotifyFired;
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
    {$IFNDEF DELPHI_4}
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetTableName: string; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
    {$ENDIF}
		function CreateHandle: HDBICur;override;
		function CreateCursor(IsExecProc : boolean): HDBICur;
    procedure CloseCursor;override;
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
    function ParamByName(const Value: String): TPSQLParam;

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

{$IFDEF DELPHI_4}
procedure FreeAndNil(var Obj);
{$ENDIF}

Procedure Check(Engine : TPSQLEngine; Status: Word);
procedure NoticeProcessor(arg: Pointer; mes: PChar); cdecl;
procedure Dac4PSQLShowAbout(aComponentName : string);

Var
   DBList : TList;

implementation

uses  ActiveX, Forms, DBPWDlg, DBLogDlg, DBConsts, BDEConst, PsqlAboutFrm
{$IFDEF DELPHI_10}, DBClient{$ENDIF}, PSQLDirectQuery;

{$R DB.DCR}

var
  CSNativeToAnsi: TRTLCriticalSection;
  CSAnsiToNative: TRTLCriticalSection;
  TimerID: Word = 0;
  SQLDelay: DWORD = 50;
  StartTime: DWORD = 0;
  BDEInitProcs: TList;

procedure Dac4PSQLShowAbout(aComponentName : string);
begin
  with TPSQLAboutComp.Create(Application) do
  try
    Caption := 'Thank you for trying DAC for MySQL';
    VersionLabel.Caption := 'v.' + PSQLDBTables.VERSION;
    Label1.Caption := aComponentName;

    {$IFDEF MICROOLAP_BUSINESS_LICENSE}
    RegLabel.Caption := 'Business License.';
    {$ELSE}
      {$IFDEF MICROOLAP_COMMERCIAL_LICENSE}
      RegLabel.Caption := 'Commercial License.';
      {$ELSE}
        {$IFDEF MICROOLAP_EDU_CLASSROOM_LICENSE}
        RegLabel.Caption := 'Educational classroom License.';
        {$ELSE}
          {$IFDEF MICROOLAP_EDU_INSTITUTION_LICENSE}
          RegLabel.Caption := 'Educational institution License.';
          {$ELSE}
            {$IFDEF MICROOLAP_PERSONAL_LICENSE}
            RegLabel.Caption := 'Personal License.';
            {$ELSE}
              {$IFDEF TRIAL}
              RegLabel.Caption := 'Trial License.';
              {$ELSE}
              RegLabel.Caption := 'Edited license string => Trial license';
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

    ShowModal();
  finally
    Free();
  end;
end;

//NoticeProcessor callback function
procedure NoticeProcessor(arg: Pointer; mes: PChar);
var s:string;
begin
 if Assigned(TPSQLDatabase(Arg).FOnNotice) then
  begin
   S := Mes;
   TPSQLDatabase(Arg).FOnNotice(TPSQLDatabase(Arg),S);
  end;
end;

{ TPSQLQueryDataLink }
type
  TPSQLQueryDataLink =  Class(TDetailDataLink)
    Private
      FQuery: TPSQLQuery;
    Protected
      Procedure ActiveChanged; Override;
      Procedure RecordChanged(Field: TField); Override;
      Function GetDetailDataSet: TDataSet; Override;
      Procedure CheckBrowseMode; Override;
    Public
      constructor Create(AQuery: TPSQLQuery);
  end;

{ Utility routines }
Procedure TAnsiToNativeBuf(Engine : TPSQLEngine; Source, Dest: PChar; Len: Integer);
var
  DataLoss: LongBool;
begin
  if Len > 0 then
  begin
     EnterCriticalSection(CSAnsiToNative);
     try
       Engine.AnsiToNative(Dest, Source, Len, DataLoss);
     finally
       LeaveCriticalSection(CSAnsiToNative);
     end;
  end;
end;


Procedure TNativeToAnsiBuf(Engine : TPSQLEngine; Source, Dest: PChar; Len: Integer);
var
  DataLoss: LongBool;
begin
  if Len > 0 then
  begin
     EnterCriticalSection(CSNativeToAnsi);
     try
       Engine.NativeToAnsi(Dest, Source, Len, DataLoss);
     finally
       LeaveCriticalSection(CSNativeToAnsi);
     end;
  end;
end;


Function TAnsiToNative(Engine : TPSQLEngine; const AnsiStr: String;
  NativeStr: PChar; MaxLen: Integer): PChar;
var
  Len: Integer;
begin
  Len := Length(AnsiStr);
  if Len > MaxLen then Len := MaxLen;
  NativeStr[Len] := #0;
  if Len > 0 then
    TAnsiToNativeBuf(Engine, Pointer(AnsiStr), NativeStr, Len);
  Result := NativeStr;
end;


Procedure TNativeToAnsi(Engine : TPSQLEngine; NativeStr: PChar; var AnsiStr: String);
var
  Len : Integer;
begin
  Len := StrLen(NativeStr);
  SetString(AnsiStr, NIL, Len);
  if Len > 0 then
    TNativeToAnsiBuf(Engine, NativeStr, Pointer(AnsiStr), Len);
end;

Procedure TDbiError(Engine : TPSQLEngine; ErrorCode: Word);
begin
  Raise EPSQLDatabaseError.Create(Engine, ErrorCode);
end;

Procedure Check(Engine : TPSQLEngine; Status: Word);
begin
  if Status <> 0 then TDbiError(Engine, Status);
end;

{ Parameter binding routines }

Function GetParamDataSize(Param: TParam): Integer;
begin
  with Param do
    if ((DataType in [ftString, ftFixedChar]) and (Length(VarToStr(Value)) > 255)) or
       (DataType in [ftBlob..ftTypedBinary]) then
      Result := SizeOf(BlobParamDesc)
    else
      Result := GetDataSize;
end;

Procedure GetParamData(Param: TParam; Buffer: Pointer; const DrvLocale: TLocale);
{$IFDEF DELPHI_4}
var
   NativeStr : String;
{$ENDIF}

  Function GetNativeStr: PChar;
  {$IFDEF DELPHI_4}
  var
     NatStr : String;
  {$ENDIF}
  begin
    {$IFDEF DELPHI_4}
    NatStr := VarToStr(Param.Value);
    Result := PChar(NatStr);
    {$ELSE}
    Param.NativeStr := VarToStr(Param.Value);
    Result := PChar(Param.NativeStr);
    {$ENDIF}
    if DrvLocale <> NIL then
      TAnsiToNativeBuf(DrvLocale, Result, Result, StrLen(Result));
  end;

begin
  with Param do
    if DataType in [ftString, ftFixedChar, ftMemo]  then
    begin
      NativeStr := VarToStr(Value);
      if (Length(NativeStr) > 255) or (DataType = ftMemo) then
      begin
        with BlobParamDesc(Buffer^) do
        begin
          if DrvLocale <> NIL then
            TAnsiToNativeBuf(DrvLocale, PChar(NativeStr), PChar(NativeStr), Length(NativeStr));
          pBlobBuffer := PChar(NativeStr);
          ulBlobLen := StrLen(pBlobBuffer);
        end;
      end else
      begin
        if (DrvLocale <> NIL) then
          TAnsiToNativeBuf(DrvLocale, PChar(NativeStr), Buffer, Length(NativeStr) + 1) else
          GetData(Buffer);
      end;
    end
    else if (DataType in [ftBlob..ftTypedBinary]) then
    begin
      with BlobParamDesc(Buffer^) do
      begin
        NativeStr := VarToStr(Value);
        ulBlobLen := Length(NativeStr);
        pBlobBuffer := PChar(NativeStr);
      end;
    end else
      GetData(Buffer);
end;

{ Timer callback Function }
Procedure FreeTimer(ForceKill : Boolean = FALSE);
begin
  if (TimerID <> 0) and (ForceKill or (GetTickCount - StartTime > SQLDelay)) then
  begin
    KillTimer(0, TimerID);
    TimerID   := 0;
    StartTime := 0;
    Screen.Cursor := crDefault;
  end;
end;


Function GetIntProp(Engine : TPSQLEngine; const Handle: Pointer; PropName: Integer): Integer;
Var
  Length : Word;
  Value  : Integer;
begin
  Value := 0;
  if (Engine.GetEngProp(HDBIObj(Handle), PropName, @Value, SizeOf(Value), Length) = DBIERR_NONE) then
    Result := Value else
    Result := 0;
end;

Function SetBoolProp(Engine : TPSQLEngine; const Handle: Pointer; PropName: Integer; Value: Bool) : Boolean;
begin
  Result := Engine.SetEngProp(HDBIObj(Handle), PropName, Abs(Integer(Value))) = DBIERR_NONE;
end;

{ EPSQLDatabaseError }
constructor EPSQLDatabaseError.Create(Engine : TPSQLEngine; ErrorCode : Word);

  Function GetErrorString: String;
  var
    Msg1 : String;
    Msg2 : String;
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
  FreeTimer(TRUE);
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
Procedure TPSQLDatabase.InitEngine;
begin
  Try
    if FEngine = nil then FEngine := TPSQLEngine.Create(nil, nil);
  Except
    raise EDatabaseError.Create('Engine not Initialize');
  end;
end;

Procedure TPSQLDatabase.AddDatabase(Value : TPSQLDatabase);
begin
   DBList.Add(Value);
end;

Procedure TPSQLDatabase.RemoveDatabase(Value : TPSQLDatabase);
begin
   while DataSetCount <> 0  do
      TPSQLDataSet(DataSets[DataSetCount - 1]).FDatabase := nil;
   DBList.Remove(Value);

  while FDirectQueryList.Count > 0 do
    TPSQLDirectQuery(FDirectQueryList[FDirectQueryList.Count - 1]).Database := nil;   
end;

constructor TPSQLDatabase.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  CheckLibraryLoaded; //04.10.2007 will raise exception is no libpq.dll present
  FParams := TStringList.Create;
  TStringList(FParams).OnChanging := ParamsChanging;
  FKeepConnection := TRUE;
  FOEMConvert := False;
  SetConnectionTimeout(15);
  SetServerPort(PSQL_PORT);
  SetSSLMode(sslDisable);
  FTransIsolation := tiReadCommitted;
  AddDatabase(Self);
  FNotifyList := TList.Create;
  FDirectQueryList := TList.Create;
  FCheckIfActiveOnParamChange := True; //SSH Tunneling stuff
  FConnectionTimeout := 15;
end;

destructor TPSQLDatabase.Destroy;
begin
  Destroying;
  Close;
  RemoveDatabase(Self);
  if FEngine <> nil then FEngine.Free;
  FNotifyList.Free;
  FDirectQueryList.Free;
  Inherited Destroy;
  FParams.Free;
  FStmtList.Free;
end;


Procedure TPSQLDatabase.ApplyUpdates(const DataSets: array of TPSQLDataSet);
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

Procedure TPSQLDatabase.ClearStatements;
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

Function TPSQLDatabase.Execute(const SQL: string; Params: TParams = NIL;
  Cache: Boolean = FALSE; Cursor: phDBICur = NIL): Integer;

  Function GetStmtInfo(SQL: PChar): PStmtInfo;

    Function GetHashCode(Str: PChar): Integer;
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
    Result := NIL;
    HashCode := GetHashCode(SQL);
    for i := 0 to FStmtList.Count - 1 do
    begin
      Info := PStmtInfo(FStmtList[i]);
      if (Info.HashCode = HashCode) and
         (AnsiStrIComp(PChar(Info.SQLText), SQL) = 0) then
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

  Function GetStatementHandle: HDBIStmt;
  var
    Info: PStmtInfo;
    Status: Word;
  begin
    Info   := NIL;
    Result := NIL;
    if Cache then
    begin
      Info := GetStmtInfo(PChar(SQL));
      Result := Info.StmtHandle;
    end;
    if not Assigned(Result) then
     begin
      Check(Engine, Engine.QAlloc(Handle, qrylangSQL, Result));
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
  Len        : Word;
begin
  StmtHandle := nil;
  Result := 0;
  Open;
  if Assigned(Params) and (Params.Count > 0) then
    begin
      StmtHandle := GetStatementHandle;
      try
        Check(Engine, Engine.QuerySetParams(StmtHandle, Params, SQL)); //SetQueryParams(Engine, Self, StmtHandle, Params);
        Check(Engine, Engine.QExec(StmtHandle, Cursor));
      finally
        if not Cache then  Engine.QFree(StmtHandle);
      end;
    end
  else
    Check(Engine, Engine.QExecDirect(Handle, qrylangSQL, SQL, Cursor, Result));
  if Result = 0 then
     if (Cursor = nil) and (Engine.GetEngProp(hDBIObj(StmtHandle), stmtROWCOUNT,@Result, SizeOf(Result),Len) <> 0) then
        Result := 0;
end;

Procedure TPSQLDatabase.CheckActive;
begin
  if FHandle = nil then DatabaseError(SDatabaseClosed);
end;

Procedure TPSQLDatabase.CheckInactive;
begin
  if FHandle <> nil then
     if csDesigning in ComponentState then
        Close else
        DatabaseError(SDatabaseOpen, Self);
end;

Procedure TPSQLDatabase.CloseDatabaseHandle;
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


Procedure TPSQLDatabase.DoDisconnect;
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
  end;
end;

Procedure TPSQLDatabase.CloseDataSets;
begin
  while DataSetCount <> 0  do
    TPSQLDataSet(DataSets[DataSetCount - 1]).Disconnect;
end;

Procedure TPSQLDatabase.Commit;
begin
  CheckActive;
  EndTransaction(xendCOMMIT);
end;

Procedure TPSQLDatabase.Rollback;
begin
  CheckActive;
  EndTransaction(xendABORT);
end;

Procedure TPSQLDatabase.StartTransaction;
var
  TransHandle:  HDBIXAct;
begin
  CheckActive;
  Check(Engine, Engine.BeginTran(FHandle, EXILType(FTransIsolation),TransHandle));
end;

Procedure TPSQLDatabase.EndTransaction(TransEnd : EXEnd);
begin
  Check(Engine, Engine.EndTran(FHandle, nil, TransEnd));
end;

Function TPSQLDatabase.GetConnected: Boolean;
begin
  Result := FHandle <> nil;
end;

Function TPSQLDatabase.GetDataSet(Index : Integer) : TPSQLDataSet;
begin
  Result := inherited GetDataSet(Index) as TPSQLDataSet;
end;

Procedure TPSQLDatabase.SetDatabaseFlags;
var
  Length: Word;
  Buffer: DBINAME;
begin
  Check(Engine, Engine.GetEngProp(HDBIOBJ(FHandle), dbDATABASETYPE, @Buffer, SizeOf(Buffer), Length));
  FPseudoIndexes := FALSE;
end;

//Function TPSQLDatabase.GetTraceFlags: TTraceFlags;
//begin
//  if Connected then
//    Result := TTraceFlags(Word(GetIntProp(Engine,FHandle,dbTraceMode))) else
//    Result := [];
//end;

Function TPSQLDatabase.GetInTransaction: Boolean;
var
  TranInfo : XInfo;
begin
  Result := (Handle <> nil) and
            (Engine.GetTranInfo(Handle, nil, @TranInfo) = DBIERR_NONE) and
            ( (TranInfo.exState = xsActive));
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
 If Handle <> nil then
   Engine.GetTranStatus(Handle,Result)
 else
   Result := trstUnknown;
end;

Procedure TPSQLDatabase.Loaded;
begin
  Inherited Loaded;
  if not StreamedConnected then InitEngine;
end;

Procedure TPSQLDatabase.Notification(AComponent : TComponent; Operation : TOperation);
begin
  Inherited Notification(AComponent, Operation);
end;

Procedure TPSQLDatabase.Login(LoginParams: TStrings);
var
  UserName, Password: String;
begin
  if Assigned(FOnLogin) then FOnLogin(Self, LoginParams) else
  begin
    UserName := LoginParams.Values['UID'];
    if not LoginDialogEx(DatabaseName, UserName, Password, FALSE) then DatabaseErrorFmt(SLoginError, [DatabaseName]);
    LoginParams.Values['UID'] := UserName;
    LoginParams.Values['PWD'] := Password;
  end;
end;

Procedure TPSQLDatabase.CheckDatabase(var Password: String);
var
  DBName: String;
  LoginParams: TStringList;
begin
  Password := '';
  DBName := FDatabaseName;
  if LoginPrompt then
  begin
     LoginParams := TStringList.Create;
     try
       Login(LoginParams);
       Password := LoginParams.Values['PWD'];
       FParams.Values['UID'] := LoginParams.Values['UID'];
       FParams.Values['PWD'] := LoginParams.Values['PWD'];
     finally
       LoginParams.Free;
     end;
  end else
      Password := FParams.Values['PWD'];
end;

Procedure TPSQLDatabase.DoConnect;
const
  OpenModes: array[Boolean] of DbiOpenMode = (dbiReadWrite, dbiReadOnly);
  ShareModes: array[Boolean] of DbiShareMode = (dbiOpenShared, dbiOpenExcl);
var
  DBPassword: String;
  ParamList : TStrings;

procedure CheckDB;
begin
  ParamList.Assign(FParams);
end;

procedure FillAddonInfo;
begin
  Engine.GetCommandTimeout(FHandle, FCommandTimeout);
  FCharSet := GetCharSet;
  Engine.GetDBProps(FHandle,FDatabaseName, FOwner, FTablespace,
        FIsTemplate,FDatabaseId, FComment);
end;

begin
  if FHandle = nil then
  begin
    InitEngine;
    CheckDatabase(DBPassword);
    ParamList := TStringList.Create;
    Try
      CheckDB;
      OEMConv := FOEMConvert;
      if (SQLLibraryHandle = HINSTANCE_ERROR) then
        EPSQLDatabaseError.CreateFmt('Error Loading DLL %s', [DLL]);
      {$IFDEF TRIAL}
        with TPSQLAboutComp.Create(Application) do
        try
          Caption := 'Thank you for trying PSQLDAC';
          VersionLabel.Caption := 'v. '+PSQLDBTables.VERSION;
          Label1.Caption := Self.ClassName;
          RegLabel.Caption := 'Trial version.';
          ShowModal;
        finally
          Free;
        end;
      {$ENDIF}
      Check(Engine, Engine.OpenDatabase(ParamList, FHandle));
      Check(Engine, Engine.GetServerVersion(FHandle, FServerVersion));
      If FCharSet <> '<default>' then //to deal with server encoding
        Check(Engine, Engine.SetCharacterSet(FHandle, FCharSet));
      Check(Engine, Engine.SetCommandTimeout(FHandle, FCommandTimeout));
      IF Assigned(FHandle) then
       begin
        PQSetNoticeProcessor(TNativeConnect(FHandle).Handle,NoticeProcessor,Self);
        FillAddonInfo;
       end;
    Finally
       ParamList.Free
    end;
    SetBoolProp(Engine, FHandle, dbUSESCHEMAFILE,        TRUE);
    SetBoolProp(Engine, FHandle, dbPARAMFMTQMARK,        TRUE);
    SetBoolProp(Engine, FHandle, dbCOMPRESSARRAYFLDDESC, TRUE);
    SetDatabaseFlags;
  end;
end;

Procedure TPSQLDatabase.ParamsChanging(Sender: TObject);
begin
 if FCheckIfActiveOnParamChange then CheckInactive; //SSH tunneling
end;

Procedure TPSQLDatabase.SetDatabaseName(const Value : String);
begin
    if csReading in ComponentState then
    begin
       FDatabaseName := Value;
       FParams.Values['DatabaseName'] := FDatabaseName;
    end
    else
    if FDatabaseName <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FDatabaseName := Value;
      FParams.Values['DatabaseName'] := FDatabaseName;
    end;
end;

Procedure TPSQLDatabase.SetServerPort(const Value : Cardinal);
begin
   if csReading in ComponentState then
    begin
       FPort := Value;
       FParams.Values['Port'] := IntToStr(FPort);
    end
    else
    if FPort <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FPort := Value;
      FParams.Values['Port'] := IntToStr(FPort);
    end;
end;

Procedure TPSQLDatabase.SetConnectionTimeout(const Value : Cardinal);
begin
   if csReading in ComponentState then
    begin
       FConnectionTimeout := Value;
       FParams.Values['ConnectionTimeout'] := IntToStr(FConnectionTimeout);
    end
    else
    if FConnectionTimeout <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FConnectionTimeout := Value;
      FParams.Values['ConnectionTimeout'] := IntToStr(FConnectionTimeout);
    end;
end;

Procedure TPSQLDatabase.SetCommandTimeout(const Value : Cardinal);
begin
  if FCommandTimeout <> Value then
    begin
      FCommandTimeout := Value;
      If Connected then
       Check(Engine, Engine.SetCommandTimeout(FHandle,FCommandTimeout));
    end;
end;

Procedure TPSQLDatabase.SetHost(const Value : String);
begin
    if FHost <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FHost := Value;
      FParams.Values['Host'] := FHost;
    end;
end;

Procedure TPSQLDatabase.SetUseSSL(const Value : Boolean);
begin
    if FUseSSL <> Value then
      If Value then
       SSLMode := sslPrefer
      else
       SSLMode := sslDisable;
end;

Procedure TPSQLDatabase.SetUserName(const Value : String);
begin
    if FUserName <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FUserName := Value;
      FParams.Values['UID'] := FUserName;
    end;
end;

Procedure TPSQLDatabase.SetUserPassword(const Value : String);
begin
    if FUserPassword <> Value then
    begin
      if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
      FUserPassword := Value;
      FParams.Values['PWD'] := FUserPassword;
    end;
end;

Procedure TPSQLDatabase.SetHandle(Value: HDBIDB);
begin
  if Connected then Close;
  if Value <> nil then
  begin
    FHandle := Value;
    SetDatabaseFlags;
    FAcquiredHandle := TRUE;
  end;
end;

Procedure TPSQLDatabase.SetKeepConnection(Value: Boolean);
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

Procedure TPSQLDatabase.SetExclusive(Value: Boolean);
begin
  if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
  FExclusive := Value;
end;

Procedure TPSQLDatabase.SetReadOnly(Value: Boolean);
begin
 if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
  FReadOnly := Value;
end;

Function TPSQLDatabase.Engine : TPSQLEngine;
begin
  Result := FEngine;
end;

Procedure TPSQLDatabase.GetStoredProcNames(Pattern: String; List: TStrings);
var
   WildCard: PChar;
   SPattern: DBITBLNAME;
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    WildCard := NIL;
    if Pattern <> '' then
      WildCard := TAnsiToNative(Engine, Pattern, SPattern, SizeOf(SPattern)- 1);
    Check(Engine, Engine.OpenStoredProcList(Handle, WildCard, List));
  finally
    List.EndUpdate;
  end;
end;

Procedure TPSQLDatabase.GetTableNames(Pattern: String; SystemTables: Boolean; List: TStrings);
var
   WildCard: PChar;
   SPattern: DBITBLNAME;
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    WildCard := NIL;
    if Pattern <> '' then
      WildCard := TAnsiToNative(Engine, Pattern, SPattern, SizeOf(SPattern)- 1);
    Check(Engine, Engine.OpenTableList(Handle,WildCard, SystemTables, List));
  finally
    List.EndUpdate;
  end;
end;

Procedure TPSQLDatabase.GetSchemaNames(Pattern: String; SystemSchemas: Boolean; List: TStrings);
var
   WildCard: PChar;
   SPattern: DBITBLNAME;
begin
  If not Assigned(List) then Exit;
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    WildCard := NIL;
    if Pattern <> '' then
      WildCard := TAnsiToNative(Engine, Pattern, SPattern, SizeOf(SPattern)- 1);
    Check(Engine, Engine.OpenSchemaList(Handle, WildCard, SystemSchemas, List));
  finally
    List.EndUpdate;
  end;
end;

procedure TPSQLDatabase.GetUserNames(Pattern: String; List: TStrings);
var
   WildCard: PChar;
   SPattern: DBITBLNAME;
begin
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    WildCard := NIL;
    if Pattern <> '' then
      WildCard := TAnsiToNative(Engine, Pattern, SPattern, SizeOf(SPattern)- 1);
    Check(Engine, Engine.OpenUserList(Handle,WildCard, List));
  finally
    List.EndUpdate;
  end;
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

function TPSQLDatabase.GetCharSet: string;
begin
  if Connected then
    Engine.GetCharacterSet(Handle,Result) else
    result := '';
end;

procedure TPSQLDatabase.SetCharSet(CharSet: string);
begin
  If FCharSet <> CharSet then
  begin
   FCharSet := CharSet;
   If Connected then
     Engine.SetCharacterSet(Handle,CharSet)
  end;
end;

procedure TPSQLDatabase.GetDatabases(Pattern: String;List : TStrings);
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
      Check(Engine, Engine.GetDatabases(Handle,PChar(Pattern),List)) else
      Check(Engine, Engine.GetDatabases(Handle,nil,List));
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
   if Connected then
    Engine.GetBackendPID(Handle,Result) else
    Result := 0;
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
//Function    : TPSQLBDECallBack.Invoke
//Description : Invoke method TPSQLBDECallback
//////////////////////////////////////////////////////////
//Input       : CallType: CBType
//              CBInfo: Pointer
//Output      : Result: CBRType
//////////////////////////////////////////////////////////
Function TPSQLBDECallBack.Invoke(CallType: CBType; CBInfo: Pointer): CBRType;
begin
  if (CallType = FCBType) then
    Result := FCallbackEvent(CBInfo)
  else
    Result := cbrUSEDEF;
  if Assigned(FOldCBFunc) then
    Result := FOldCBFunc(CallType, FOldCBData, CBInfo);
end;


procedure TPSQLDatabase.GetTablespaces(Pattern: String; List: TStrings);
var
   WildCard: PChar;
   SPattern: DBITBLNAME;
begin
  If not Assigned(List) then Exit;
  List.BeginUpdate;
  try
    if Handle = nil then Connected := True;
    List.Clear;
    WildCard := NIL;
    if Pattern <> '' then
      WildCard := TAnsiToNative(Engine, Pattern, SPattern, SizeOf(SPattern)- 1);
    Check(Engine, Engine.OpenTablespaceList(Handle, WildCard, List));
  finally
    List.EndUpdate;
  end;
end;

procedure TPSQLDatabase.SetSSLMode(const Value: TSSLMode);
begin
  If FSSLMode <> Value then
   begin
     if FCheckIfActiveOnParamChange then
        CheckInactive; //SSH tunneling
     FSSLMode := Value;
     FParams.Values['SSLMode'] := SSLConsts[FSSLMode];
     FUseSSL := Value in [sslPrefer,sslAllow,sslRequire];
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

{ TPSQLDataSet }
constructor TPSQLDataSet.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  CheckLibraryLoaded; //04.10.2007 will raise exception is no libpq.dll present
  FCacheBlobs := False;
  FAutoRefresh := FALSE;
  FAllowSequenced := False; //Added by Nicolas Ring
end;

destructor TPSQLDataSet.Destroy;
begin
  Inherited Destroy;
  if FBlockReadBuf <> nil then
  begin
    FreeMem(FBlockReadBuf);
    FBlockReadBuf := NIL;
  end;
  SetUpdateObject(NIL);
end;

//////////////////////////////////////////////////////////
//Procedure   : TPSQLDataSet.OpenCursor
//Description : Open cursor
//////////////////////////////////////////////////////////
//Input       : InfoQuery: Boolean
//////////////////////////////////////////////////////////
Procedure TPSQLDataSet.OpenCursor(InfoQuery: Boolean);
begin
  if Database=nil then raise EDatabaseError.Create('Property Database not set!');
  If FHandle = nil then
     FHandle := CreateHandle;
  if FHandle = nil then
  begin
    FreeTimer(TRUE);
    raise ENoResultSet.Create(SHandleError);
  end;
  SetDBFlag(dbfOpened, TRUE);
  Inherited OpenCursor(InfoQuery);
  SetUpdateMode(FUpdateMode);
  {$IFNDEF DELPHI_4}
  SetupAutoRefresh;
  {$ENDIF}
end;

//////////////////////////////////////////////////////////
//Procedure   : TPSQLDataSet.CloseCursor
//Description : Close cursor
//////////////////////////////////////////////////////////
Procedure TPSQLDataSet.CloseCursor;
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

//////////////////////////////////////////////////////////
//Function    : TPSQLDataSet.CreateHandle
//Description : Virtual method Create Handle will be overwritten
//              in TPSQLQuery and TPSQLTable
//////////////////////////////////////////////////////////
//Output      : Result: HDBICur
//////////////////////////////////////////////////////////
Function TPSQLDataSet.CreateHandle: HDBICur;
begin
  Result := nil;
end;

Procedure TPSQLDataSet.DestroyHandle;
begin
  Engine.CloseCursor(FHandle);
end;

Procedure TPSQLDataSet.InternalInitFieldDefs;
var
  I, FieldID: Integer;
  FieldDescs: TFieldDescList;
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
      Engine.GetVChkDesc(FHandle, I, @ValCheckDesc);
      if ValCheckDesc.bRequired and not ValCheckDesc.bHasDefVal then
        RequiredFields[ValCheckDesc.iFldNum] := True;
    end;
    SetLength(FieldDescs, FldDescCount);
    Engine.GetFieldDescs(FHandle, PFLDDesc(FieldDescs));
    FieldID := FieldNoOfs;
    I := FieldID - 1;
    FieldDefs.Clear;
    while I < FldDescCount do
      AddFieldDesc(FieldDescs, I, FieldID, RequiredFields, FieldDefs);
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

Procedure TPSQLDataSet.GetObjectTypeNames(Fields: TFields);
var
  Len: Word;
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
        ObjectField.ObjectType := TypeDesc.szTypeName;
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

Procedure TPSQLDataSet.InternalOpen;
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
  if DefaultFields then CreateFields;
  BindFields(TRUE);
  if ObjectView then GetObjectTypeNames(Fields);
  InitBufferPointers(FALSE);
  AllocKeyBuffers;
  Engine.SetToBegin(FHandle);
  PrepareCursor;
  if Filter <> '' then FExprFilter := CreateExprFilter(Filter, FilterOptions, 0);
  if Assigned(OnFilterRecord) then FFuncFilter := CreateFuncFilter(@TPSQLDataSet.RecordFilter, 1);
  if Filtered then ActivateFilters;
end;

Procedure TPSQLDataSet.InternalClose;
begin
  FFuncFilter := NIL;
  FExprFilter := NIL;
  FreeKeyBuffers;
  BindFields(FALSE);
  if DefaultFields then DestroyFields;
  FIndexFieldCount := 0;
  FKeySize := 0;
  FExpIndex := FALSE;
  FCaseInsIndex := FALSE;
  FCanModify := FALSE;
end;

Procedure TPSQLDataSet.PrepareCursor;
begin
end;

Function TPSQLDataSet.IsCursorOpen: Boolean;
begin
  Result := Handle <> nil;
end;

{$IFDEF DELPHI_4}
function TPSQLDataSet.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
   Result := FMTBCDToCurr(FMTBCD(BCD^), Curr);
end;

function TPSQLDataSet.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
   Result := CurrToFMTBCD(Curr, FMTBCD(BCD^), 32, Decimals);
end;
{$ENDIF}

Procedure TPSQLDataSet.InternalHandleException;
begin
  Application.HandleException(Self)
end;

////////////////////////////////////////////////////////////
//                Record Functions                        //
////////////////////////////////////////////////////////////
Procedure TPSQLDataSet.InitBufferPointers(GetProps: Boolean);
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

Function TPSQLDataSet.AllocRecordBuffer: PChar;
begin
   Result := AllocMem(FRecBufSize);
end;

Procedure TPSQLDataSet.FreeRecordBuffer(var Buffer : PChar);
begin
  Engine.CheckBuffer(FHandle, Buffer); //pasha_golub 10.08.06
  ClearBlobCache(Buffer);
  FreeMem(Buffer);
  Buffer := nil;
end;

Procedure TPSQLDataSet.InternalInitRecord(Buffer : PChar);
begin
  Engine.InitRecord(FHandle, Buffer);
end;

Procedure TPSQLDataSet.ClearBlobCache(Buffer : PChar);
var
  I: Integer;
begin
  if FCacheBlobs then
    for I := 0 to Pred(BlobFieldCount) do
      TBlobDataArray(Buffer + FBlobCacheOfs)[ I ] := '';
end;

Procedure TPSQLDataSet.ClearCalcFields(Buffer : PChar);
begin
  FillChar(Buffer[ RecordSize ], CalcFieldsSize, 0);
end;

Procedure TPSQLDataSet.InitRecord(Buffer : PChar);
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

Function TPSQLDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
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

Function TPSQLDataSet.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    Result := (Engine.GetRecord(FHandle, dbiNoLock, Buffer, NIL) = DBIERR_NONE);
  end else
    Result := FALSE;
end;

Function TPSQLDataSet.GetOldRecord: PChar;
begin
  UpdateCursorPos;
  if SetBoolProp(Engine, Handle, curDELAYUPDGETOLDRECORD, TRUE) then
  try
    AllocCachedUpdateBuffers(True);
    Check(Engine, Engine.GetRecord(FHandle, dbiNoLock, FUpdateCBBuf.pOldRecBuf, NIL));
    Result := PChar(FUpdateCBBuf.pOldRecBuf);
    AllocCachedUpdateBuffers(False);
  finally
    SetBoolProp(Engine, Handle, curDELAYUPDGETOLDRECORD, FALSE);
  end else
    Result := NIL;
end;

Procedure TPSQLDataSet.FetchAll;
begin
  if not EOF then
  begin
    CheckBrowseMode;
    Check(Engine, Engine.SetToEnd(Handle));
    Check(Engine, Engine.GetPriorRecord(FHandle, dbiNoLock, nil, nil));
    CursorPosChanged;
  end;
end;

Procedure TPSQLDataSet.FlushBuffers;
begin
  CheckBrowseMode;
end;

Function TPSQLDataSet.GetRecordCount: Integer;
var p: pointer;
begin
  CheckActive;
  Result := 0;
  if Filtered then
    begin
     p := GetBookmark;
     try
       DisableControls;
       try
         First;
         while not Eof do
           begin
            inc(Result);
            Next;
           end;
         GotoBookmark(p);
       finally
        EnableControls;
       end;
     finally
       FreeBookmark(p);
     end
    end
  else
    if Engine.GetRecordCount(FHandle, Result) <> DBIERR_NONE then
      Result := -1;
end;

Function TPSQLDataSet.GetRecNo: Integer;
var
  BufPtr: PChar;
begin
  CheckActive;
  if (State = dsCalcFields) then
    BufPtr := CalcBuffer
  else
    BufPtr := ActiveBuffer;
  Result := PRecInfo(BufPtr + FRecInfoOfs).RecordNumber;
end;

Procedure TPSQLDataSet.SetRecNo(Value : Integer);
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

Function TPSQLDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

Function TPSQLDataSet.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBlockRead: RecBuf := FBlockReadBuf + (FBlockBufOfs * FRecordSize);
    dsBrowse: if IsEmpty then
                 RecBuf := nil else
                 RecBuf := ActiveBuffer;
    dsEdit, dsInsert: RecBuf := ActiveBuffer;
    dsSetKey: RecBuf := PChar(FKeyBuffer) + SizeOf(TKeyBuffer);
    dsCalcFields: RecBuf := CalcBuffer;
    dsFilter: RecBuf := FFilterBuffer;
    dsNewValue: if FInUpdateCallback then
                   RecBuf := FUpdateCBBuf.pNewRecBuf else
                   RecBuf := ActiveBuffer;
    dsOldValue: if FInUpdateCallback then
                   RecBuf := FUpdateCBBuf.pOldRecBuf else
                   RecBuf := GetOldRecord;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

Procedure TPSQLDataSet.AddFieldDesc(FieldDescs: TFieldDescList; var DescNo: Integer;
  var FieldID: Integer; RequiredFields: TBits; FieldDefs: TFieldDefs);
var
  FType: TFieldType;
  FSize: Word;
  FRequired: Boolean;
  FPrecision, I: Integer;
  FieldName, FName: string;
  FieldDesc: FLDDesc;
begin
  FieldDesc := FieldDescs[DescNo];
  Inc(DescNo);
  with FieldDesc do
  begin
    TNativeToAnsi(Engine, szName, FieldName);
    I := 0;
    FName := FieldName;
    while FieldDefs.IndexOf(FName) >= 0 do
    begin
      Inc(I);
      FName := Format('%s_%d', [FieldName, I]);
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
    end;
    with FieldDefs.AddFieldDef do
    begin
      FieldNo := FieldID;
      Inc(FieldID);
      Name := FName;
      DataType := FType;
      Size := FSize;
      Precision := FPrecision;
      if FRequired then
        Attributes := [faRequired];
      if efldrRights = fldrREADONLY then
        Attributes := Attributes + [faReadonly];
      {$IFNDEF DELPHI_4}
      if iSubType = fldstFIXED then
        Attributes := Attributes + [faFixed];
      {$ENDIF}
      InternalCalcField := bCalcField;
      case FType of
        ftADT:
          begin
            if iSubType = fldstADTNestedTable then
              Attributes := Attributes + [faUnNamed];
            for I := 0 to iUnits1 - 1 do
              AddFieldDesc(FieldDescs, DescNo, FieldID, RequiredFields, ChildDefs);
          end;
        ftArray:
          begin
            I := FieldID;
            StrCat(StrCopy(FieldDescs[DescNo].szName, FieldDesc.szName),'[0]');
            AddFieldDesc(FieldDescs, DescNo, I, RequiredFields, ChildDefs);
            Inc(FieldID, iUnits2);
          end;
      end;
    end;
  end;
end;

Function TPSQLDataSet.GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
var
  RecBuf: PChar;
  Status: DBIResult;
  DoCheck: Boolean;
begin
  Result := 0;
  DoCheck := (BlockReadSize = 0);
  if (BlockReadSize > 0) then
    RecBuf := FBlockReadBuf + (FBlockBufOfs * FRecordSize)
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

Function TPSQLDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PChar;
  Status: DBIResult;
begin
  if (BlockReadSize > 0) then
  begin
    Status := Engine.GetField(FHandle, FieldNo, FBlockReadBuf +
      (FBlockBufOfs * FRecordSize), Buffer, IsBlank);
    Result := (Status = DBIERR_NONE) and not IsBlank;
  end
  else
  begin
    Result := GetActiveRecBuf(RecBuf);
    if Result then
    begin
      Check(Engine, Engine.GetField(FHandle, FieldNo, RecBuf, Buffer, IsBlank));
      Result := not IsBlank;
    end
  end;
end;

Function TPSQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf: PChar;
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
      if Result and (Buffer <> NIL) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

Procedure TPSQLDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf: PChar;
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
      Validate(Buffer);
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

Function TPSQLDataSet.GetBlobData(Field : TField; Buffer : PChar) : TBlobData;
begin
  Result := TBlobDataArray(Buffer + FBlobCacheOfs)[ Field.Offset ];
end;

Procedure TPSQLDataSet.SetBlobData(Field : TField; Buffer : PChar; Value : TBlobData);
begin
  if (Buffer = ActiveBuffer) then
    TBlobDataArray(Buffer + FBlobCacheOfs)[ Field.Offset ] := Value;
end;

Procedure TPSQLDataSet.CloseBlob(Field: TField);
begin
  Engine.FreeBlob(Handle, ActiveBuffer, Field.FieldNo);
end;

Function TPSQLDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
begin
  CheckCachedUpdateMode;
  Result := Inherited GetStateFieldValue(State, Field);
end;

Procedure TPSQLDataSet.SetStateFieldValue(State: TDataSetState; Field: TField; Const Value: Variant);
begin
  CheckCachedUpdateMode;
  Inherited SetStateFieldValue(State, Field, Value);
end;


Function TPSQLDataSet.Translate(Src, Dest: PChar; ToOem: Boolean) : Integer;
begin
  Result := StrLen(Src);
  if ToOem then
  begin
     TAnsiToNativeBuf(Engine, Src, Dest, Result);
  end else
  begin
        TNativeToAnsiBuf(Engine, Src, Dest, Result);
  end;
  if Src <> Dest then Dest[ Result ] := #0;
end;

Function TPSQLDataSet.GetFieldFullName(Field : TField) : string;
var
  Len: Word;
  AttrDesc: ObjAttrDesc;
  Buffer: array[0..1024] of Char;
begin
  if Field.FieldNo > 0  then
  begin
    AttrDesc.iFldNum := Field.FieldNo;
    AttrDesc.pszAttributeName := Buffer;
    Check(Engine, Engine.GetEngProp(HDBIOBJ(Handle), curFIELDFULLNAME, @AttrDesc, SizeOf(Buffer), Len));
    TNativeToAnsi(Engine, Buffer, Result);
  end else
    Result := inherited GetFieldFullName(Field);
end;

Procedure TPSQLDataSet.InternalFirst;
begin
  Check(Engine, Engine.SetToBegin(FHandle));
end;

Procedure TPSQLDataSet.InternalLast;
begin
  Check(Engine, Engine.SetToEnd(FHandle));
end;

Procedure TPSQLDataSet.InternalEdit;
begin
  FOldBuffer := AllocRecordBuffer;
  Move(ActiveBuffer^,FOldBuffer[0],FRecBufSize);
  Check(Engine, Engine.GetRecord(FHandle, {dbiNoLock}dbiWriteLock, ActiveBuffer, NIL)); //locking stuff need attention
  ClearBlobCache(ActiveBuffer);
end;

Procedure TPSQLDataSet.InternalInsert;
begin
  SetBoolProp(Engine, Handle, curMAKECRACK, TRUE);
  CursorPosChanged;
end;

Procedure TPSQLDataSet.InternalPost;
begin
  If Assigned(FUpdateObject) then
   begin
     if State = dsEdit then
       FUpdateObject.Apply(ukModify)
     else
       If State = dsInsert then
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
        If State = dsInsert then
          Check(Engine, Engine.InsertRecord(FHandle, dbiNoLock, ActiveBuffer));
    end; //else
  if assigned(fOldBuffer) then  FreeRecordBuffer(FOldBuffer);
end;

Procedure TPSQLDataSet.InternalDelete;
var
  Result: Word;
begin
  If not Assigned(FUpdateObject) then
   begin
    Result := Engine.DeleteRecord(FHandle, ActiveBuffer);
    if (Result <> DBIERR_NONE) then Check(Engine, Result);
   end
  else
   FUpdateObject.Apply(ukDelete);
end;

Function TPSQLDataSet.IsSequenced: Boolean;
begin
  Result := (FRecNoStatus = rnParadox) and (not Filtered);
end;

Function TPSQLDataSet.GetCanModify: Boolean;
begin
  Result := FCanModify or ForceUpdateCallback;
end;

Procedure TPSQLDataSet.InternalRefresh;
begin
    Check(Engine, Engine.ForceReread(FHandle));
end;

Procedure TPSQLDataSet.Post;
begin
  Inherited Post;
  if (State = dsSetKey) then
    PostKeyBuffer(TRUE);
end;

Procedure TPSQLDataSet.Cancel;
begin
  Inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(FALSE);
end;

Procedure TPSQLDataSet.InternalCancel;
begin
  If State = dsEdit then
    Engine.RelRecordLock(FHandle, FALSE);
  If assigned(fOldBuffer) then
    FreeRecordBuffer(FOldBuffer);
end;

Procedure TPSQLDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then
    Check(Engine,Engine.AppendRecord(FHandle,Buffer))  else
    Check(Engine,Engine.InsertRecord(FHandle,dbiNoLock,Buffer));
end;

Procedure TPSQLDataSet.InternalGotoBookmark(Bookmark : TBookmark);
begin
  Check(Engine, Engine.SetToBookmark(FHandle, Bookmark));
end;

Procedure TPSQLDataSet.InternalSetToRecord(Buffer : PChar);
begin
  InternalGotoBookmark(Buffer + FBookmarkOfs);
end;

Function TPSQLDataSet.GetBookmarkFlag(Buffer : PChar) : TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

Procedure TPSQLDataSet.SetBookmarkFlag(Buffer : PChar; Value : TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

Procedure TPSQLDataSet.GetBookmarkData(Buffer : PChar; Data : Pointer);
begin
  Move(Buffer[ FBookmarkOfs ], Data^, BookmarkSize);
end;

Procedure TPSQLDataSet.SetBookmarkData(Buffer : PChar; Data : Pointer);
begin
  Move(Data^, Buffer[ FBookmarkOfs ], BookmarkSize);
end;

Function TPSQLDataSet.CompareBookmarks(Bookmark1, Bookmark2 : TBookmark) : Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,CMPLess),(CMPGtr,CMPEql));
begin
  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = NIL, Bookmark2 = NIL];
  if (Result = 2) then
  begin
    if (Handle <> NIL) then
      Check(Engine, Engine.CompareBookmarks(Handle, Bookmark1, Bookmark2, Result));
    if (Result = CMPKeyEql) then
      Result := CMPEql;
  end;
end;

Function TPSQLDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Handle <> NIL);
  if Result then
  begin
    CursorPosChanged;
    Result := (Engine.SetToBookmark(FHandle, Bookmark) = DBIERR_NONE) and
      (Engine.GetRecord(FHandle, dbiNOLOCK, nil, nil) = DBIERR_NONE)
  end;
end;

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

Procedure TPSQLDataSet.GetIndexInfo;
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


Procedure TPSQLDataSet.SwitchToIndex(const IndexName, TagName : String);
var
  Status: DBIResult;
begin
  ResetCursorRange;
  UpdateCursorPos;
  Status := Engine.SwitchToIndex(FHandle, PChar(IndexName), PChar(TagName), 0, TRUE);
  if (Status = DBIERR_NOCURRREC) then
    Status := Engine.SwitchToIndex(FHandle, PChar(IndexName), PChar(TagName), 0, FALSE);
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

Function TPSQLDataSet.GetIndexField(Index : Integer): TField;
var
  FieldNo: Integer;
begin
  if (Index < 0) or (Index >= FIndexFieldCount) then DatabaseError(SFieldIndexError, Self);
  FieldNo := FIndexFieldMap[Index];
  Result := FieldByNumber(FieldNo);
  if Result = nil then   DatabaseErrorFmt(SIndexFieldMissing, [ FieldDefs[FieldNo - 1].Name ], Self);
end;

Procedure TPSQLDataSet.SetIndexField(Index : Integer; Value : TField);
begin
  GetIndexField(Index).Assign(Value);
end;

Function TPSQLDataSet.GetIndexFieldCount: Integer;
begin
  Result := FIndexFieldCount;
end;

Procedure TPSQLDataSet.AllocKeyBuffers;
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

Procedure TPSQLDataSet.FreeKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
    DisposeMem(FKeyBuffers[ KeyIndex ], SizeOf(TKeyBuffer) + FRecordSize);
end;

Function TPSQLDataSet.InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
begin
  FillChar(Buffer^, SizeOf(TKeyBuffer) + FRecordSize, 0);
  Engine.InitRecord(FHandle, PChar(Buffer) + SizeOf(TKeyBuffer));
  Result := Buffer;
end;

Procedure TPSQLDataSet.CheckSetKeyMode;
begin
  if (State <> dsSetKey) then DatabaseError(SNotEditing, Self);
end;

Function TPSQLDataSet.SetCursorRange: Boolean;
var
  RangeStart, RangeEnd: PKeyBuffer;
  StartKey, EndKey: PChar;
  IndexBuffer: PChar;
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
        StartKey := PChar(RangeStart) + SizeOf(TKeyBuffer);
        UseStartKey := Engine.ExtractKey(Handle, StartKey, IndexBuffer) = 0;
      end
      else
        StartKey := NIL;
      RangeEnd := FKeyBuffers[kiRangeEnd];
      if RangeEnd.Modified then
      begin
        EndKey := PChar(RangeEnd) + SizeOf(TKeyBuffer);
        UseEndKey := (Engine.ExtractKey(Handle, EndKey, IndexBuffer + KeySize) = 0);
      end
      else
        EndKey := NIL;
      UseKey := UseStartKey and UseEndKey;
      if UseKey then
      begin
        if (StartKey <> NIL) then
          StartKey := IndexBuffer;
        if (EndKey <> NIL) then
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

Function TPSQLDataSet.ResetCursorRange: Boolean;
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

Procedure TPSQLDataSet.SetLinkRanges(MasterFields: TList);
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

Function TPSQLDataSet.GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
begin
  Result := FKeyBuffers[KeyIndex];
end;

Procedure TPSQLDataSet.SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  Move(FKeyBuffer^, FKeyBuffers[kiSave]^, SizeOf(TKeyBuffer) + FRecordSize);
  if Clear then InitKeyBuffer(FKeyBuffer);
  SetState(dsSetKey);
  SetModified(FKeyBuffer.Modified);
  DataEvent(deDataSetChange, 0);
end;

Procedure TPSQLDataSet.PostKeyBuffer(Commit: Boolean);
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

Function TPSQLDataSet.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.Exclusive;
end;

Procedure TPSQLDataSet.SetKeyExclusive(Value: Boolean);
begin
  CheckSetKeyMode;
  FKeyBuffer.Exclusive := Value;
end;

Function TPSQLDataSet.GetKeyFieldCount: Integer;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.FieldCount;
end;

Procedure TPSQLDataSet.SetKeyFieldCount(Value: Integer);
begin
  CheckSetKeyMode;
  FKeyBuffer.FieldCount := Value;
end;

Procedure TPSQLDataSet.SetKeyFields(KeyIndex: TKeyIndex;
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

Function TPSQLDataSet.GetIsIndexField(Field: TField): Boolean;
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

Function TPSQLDataSet.MapsToIndex(Fields: TList; CaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  HasStr : Boolean;
begin
  Result := FALSE;
  HasStr := FALSE;
  For i := 0 to Fields.Count - 1 do
  begin
    HasStr := TField(Fields[I]).DataType in [ftString, ftFixedChar, ftWideString];
    if HasStr then
      break;
  end;
  if (CaseInsensitive <> FCaseInsIndex) and HasStr then
    Exit;
  if (Fields.Count > FIndexFieldCount) then
    Exit;
  for I := 0 to Pred(Fields.Count) do
    if TField(Fields[I]).FieldNo <> Integer(FIndexFieldMap[I]) then
      Exit;
  Result := TRUE;
end;

Procedure TPSQLDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
  begin
    Close;
    FDatabase := nil;
  end;
end;

Procedure TPSQLDataSet.ActivateFilters;
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

Procedure TPSQLDataSet.DeactivateFilters;
begin
  if FFuncFilter <> nil then Check(Engine, Engine.DeactivateFilter(FHandle, FFuncFilter));
  if FExprFilter <> nil then Check(Engine, Engine.DeactivateFilter(FHandle, FExprFilter));
end;

Function TPSQLDataSet.CreateExprFilter(const Expr: String;
  Options: TFilterOptions; Priority: Integer): HDBIFilter;
var
  Parser: TExprParser;
begin
  Parser := TExprParser.Create(Self, Expr, Options, [], '', NIL, FldTypeMap);
  try
    Check(Engine, Engine.AddFilter(FHandle, 0, Priority, FALSE, PCANExpr(Parser.FilterData), NIL, Result));
  finally
    Parser.Free;
  end;
end;

Function TPSQLDataSet.CreateFuncFilter(FilterFunc: Pointer;Priority: Integer): HDBIFilter;
begin
  Check(Engine, Engine.AddFilter(FHandle, Integer(Self), Priority, FALSE, NIL, PFGENFilter(FilterFunc), Result));
end;

{$WARNINGS OFF}
Function TPSQLDataSet.CreateLookupFilter(Fields: TList; const Values: Variant;
  Options: TLocateOptions; Priority: Integer): HDBIFilter;
var
  I: Integer;
  Filter: TFilterExpr;
  Expr, Node: PExprNode;
  FilterOptions: TFilterOptions;
begin
  if loCaseInsensitive in Options then
    FilterOptions := [foNoPartialCompare, foCaseInsensitive] else
    FilterOptions := [foNoPartialCompare];
  {$IFDEF DELPHI_4}
  Filter := TFilterExpr.Create(Self, FilterOptions, [], '', NIL);
  {$ELSE}
  Filter := TFilterExpr.Create(Self, FilterOptions, [], '', NIL, FldTypeMap);
  {$ENDIF}
  try
    if Fields.Count = 1 then
    begin
      Node := Filter.NewCompareNode(TField(Fields[0]), {$IFDEF DELPHI_4}canEQ {$ELSE}coEQ {$ENDIF}, Values);
      Expr := Node;
    end
    else
      for I := 0 to Fields.Count-1 do
      begin
        Node := Filter.NewCompareNode(TField(Fields[I]), {$IFDEF DELPHI_4}canEQ {$ELSE}coEQ {$ENDIF}, Values[I]);
        if I = 0 then
          Expr := Node else
          Expr := Filter.NewNode(enOperator, {$IFDEF DELPHI_4}canAND{$ELSE}coAND {$ENDIF}, Unassigned, Expr, Node);
      end;
    if loPartialKey in Options then Node^.FPartial := TRUE;
    Check(Engine, Engine.AddFilter(FHandle, 0, Priority, FALSE, PCANExpr(Filter.GetFilterData(Expr)), NIL, Result));
  finally
    Filter.Free;
  end;
end;
{$WARNINGS ON}

Procedure TPSQLDataSet.SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
begin
  if Filtered then
  begin
    CursorPosChanged;
    DestroyLookupCursor;
    Engine.SetToBegin(FHandle);
    if Filter <> NIL then Engine.DropFilter(FHandle, Filter);
    Filter := Value;
    if Filter <> NIL then Engine.ActivateFilter(FHandle, Filter);
  end else
  begin
    if Filter <> nil then Engine.DropFilter(FHandle, Filter);
    Filter := Value;
  end;
end;

Procedure TPSQLDataSet.SetFilterData(const Text: String; Options: TFilterOptions);
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
        HFilter := NIL;
      SetFilterHandle(FExprFilter, HFilter);
    end;
  end;
  Inherited SetFilterText(Text);
  Inherited SetFilterOptions(Options);
  if Active and Filtered then First;
end;

Procedure TPSQLDataSet.SetFilterText(const Value: String);
begin
  SetFilterData(Value, FilterOptions);
end;

Procedure TPSQLDataSet.SetFiltered(Value: Boolean);
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
      Inherited SetFiltered(Value);
    end;
    First;
  end else Inherited SetFiltered(Value);
end;

Procedure TPSQLDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

Procedure TPSQLDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
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

Function TPSQLDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
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
      Status := Engine.GetNextRecord(FHandle, dbiNoLock, NIL, NIL);
    end
    else
    begin
      if Restart then Check(Engine, Engine.SetToEnd(FHandle));
      Status := Engine.GetPriorRecord(FHandle, dbiNoLock, NIL, NIL);
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

Function TPSQLDataSet.RecordFilter(RecBuf: Pointer; RecNo: Integer): Smallint;
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
    Application.HandleException(Self);
  end;
  RestoreState(SaveState);
  Result := Ord(Accept);
end;

Function TPSQLDataSet.LocateRecord(const KeyFields: String;const KeyValues: Variant;Options: TLocateOptions;SyncCursor: Boolean): Boolean;
var
  I, FieldCount, PartialLength: Integer;
  Buffer: PChar;
  Fields: TList;
  LookupCursor: HDBICur;
  Filter: HDBIFilter;
  Status: DBIResult;
  CaseInsensitive: Boolean;

   //addded by pasha_golub 13.01.05
  {}procedure SetFieldValue(const Fld : TField; const VarValue : Variant);
  {}begin
  {$IFNDEF VER150} // not Delphi 7
  {}  if (Fld is TLargeIntField) then
  {}    TIntegerField(Fld).Value := VarValue
  {}  else
  {$ENDIF}
  {}    Fld.Value := VarValue;
  {}end;

  {function IsLongintFld:boolean;
  var i:integer;
  begin
    i:=0;
    while (i<Fields.Count-1) and
          not (TField(Fields[i]) is TLargeIntField) do
     inc(i);
    Result := i <> Fields.Count-1
  end;}

begin
  CheckBrowseMode;
  CursorPosChanged;
  Buffer := TempBuffer;
  Fields := TList.Create;
  try
    GetFieldList(Fields, KeyFields);
    CaseInsensitive := loCaseInsensitive in Options;
    if MapsToIndex(Fields, CaseInsensitive){ or IsLongintFld}
       then
        LookupCursor := FHandle else
        LookupCursor := GetLookupCursor(KeyFields, CaseInsensitive);
    if (LookupCursor <> nil) then
    begin
      SetTempState(dsFilter);
      FFilterBuffer := Buffer;
      try
        Engine.InitRecord(LookupCursor, Buffer);
        FieldCount := Fields.Count;
        if FieldCount = 1 then
        begin
            if VarIsArray(KeyValues) then
               SetFieldValue(TField(Fields.First), KeyValues[0]) else
               SetFieldValue(TField(Fields.First), KeyValues);
        end else
            for I := 0 to FieldCount - 1 do
                SetFieldValue(TField(Fields[I]), KeyValues[I]);
        PartialLength := 0;
        if (loPartialKey in Options) and
          (TField(Fields.Last).DataType = ftString) then
        begin
          Dec(FieldCount);
          PartialLength := Length(TField(Fields.Last).AsString);
        end;
        Status := Engine.GetRecordForKey(LookupCursor, False, FieldCount,PartialLength, Buffer, Buffer);
      finally
        RestoreState(dsBrowse);
      end;
      if (Status = DBIERR_NONE) and SyncCursor and(LookupCursor <> FHandle) then
        Status := Engine.SetToCursor(FHandle, LookupCursor);
    end else
    begin
      Check(Engine,Engine.SetToBegin(FHandle));
      Filter := CreateLookupFilter(Fields, KeyValues, Options, 2);
      Engine.ActivateFilter(FHandle, Filter);
      Status := Engine.GetNextRecord(FHandle, dbiNoLock, Buffer, nil);
      Engine.DropFilter(FHandle, Filter);
    end;
  finally
    Fields.Free;
  end;
  Result := Status = DBIERR_NONE;
end;

{$WARNINGS OFF}
Function TPSQLDataSet.LocateNearestRecord(const KeyFields: String;const KeyValues: Variant;Options: TLocateOptions;SyncCursor: Boolean): Word;
var
  Buffer: PChar;
  Fields: TList;
  Filter: HDBIFilter;
  Status: DBIResult;
  I: Integer;
  Filter1: TFilterExpr;
  Expr, Node: PExprNode;
  FilterOptions: TFilterOptions;

begin
  CheckBrowseMode;
  CursorPosChanged;
  Buffer := TempBuffer;
  Fields := TList.Create;
  try
    GetFieldList(Fields, KeyFields);
    Check(Engine, Engine.SetToBegin(FHandle));
    FilterOptions := [foNoPartialCompare];
    {$IFDEF DELPHI_4}
    Filter1 := TFilterExpr.Create(Self, FilterOptions, [], '', NIL);
    {$ELSE}
    Filter1 := TFilterExpr.Create(Self, FilterOptions, [], '', NIL, FldTypeMap);
    {$ENDIF}
    try
      if Fields.Count = 1 then
      begin
         Node := Filter1.NewCompareNode(TField(Fields[0]), {$IFDEF DELPHI_4}canGE {$ELSE}coGE {$ENDIF}, KeyValues);
         Expr := Node;
      end
      else
        for I := 0 to Fields.Count-1 do
        begin
          Node := Filter1.NewCompareNode(TField(Fields[I]), {$IFDEF DELPHI_4}canGE {$ELSE}coGE {$ENDIF}, KeyValues[I]);
          if I = 0 then
            Expr := Node else
            Expr := Filter1.NewNode(enOperator, {$IFDEF DELPHI_4}canAND {$ELSE}coAND {$ENDIF}, Unassigned, Expr, Node);
        end;
      if loPartialKey in Options then Node^.FPartial := TRUE;
      Check(Engine, Engine.AddFilter(FHandle, 0, 2, FALSE, PCANExpr(Filter1.GetFilterData(Expr)), NIL,Filter));
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
{$WARNINGS ON}
Function TPSQLDataSet.Lookup(const KeyFields: String; const KeyValues: Variant;
  const ResultFields: String): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], FALSE) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
      Assert(Result <> Null,Format(#13#10+'KeyFields: %s'+#13#10+'KeyValues: %s',[KeyFields,KeyValues]));
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

Function TPSQLDataSet.Locate(const KeyFields: String;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, TRUE);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

Function TPSQLDataSet.GetLookupCursor(const KeyFields: String; CaseInsensitive: Boolean): HDBICur;
begin
  Result := NIL;
end;

Procedure TPSQLDataSet.DestroyLookupCursor;
begin
end;

Procedure TPSQLDataSet.AllocCachedUpdateBuffers(Allocate: Boolean);
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

Procedure TPSQLDataSet.CheckCachedUpdateMode;
begin
end;

Function TPSQLDataSet.UpdateCallbackRequired: Boolean;
begin
  Result := FCachedUpdates  and (Assigned(FOnUpdateError) or
    Assigned(FOnUpdateRecord) or Assigned(FUpdateObject));
end;

Function TPSQLDataSet.ForceUpdateCallback: Boolean;
begin
  Result := True{FCachedUpdates} and (Assigned(FOnUpdateRecord) or
    Assigned(FUpdateObject));
end;

Procedure TPSQLDataSet.SetCachedUpdates(Value: Boolean);

  Procedure ReAllocBuffers;
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

Procedure TPSQLDataSet.SetupCallBack(Value: Boolean);
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
      FUpdateCallback := NIL;
    end;
  end;
end;

Function TPSQLDataSet.ProcessUpdates(UpdCmd: DBIDelayedUpdCmd): Word;
begin
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Result :=0;
//  Resync([]); //NEW
end;

Procedure TPSQLDataSet.ApplyUpdates;
var
  Status: Word;
begin
  if (State <> dsBrowse) then Post;
  Status := ProcessUpdates(dbiDelayedUpdPrepare);
  if (Status <> DBIERR_NONE) then
    if (Status = DBIERR_UPDATEABORT) then SysUtils.Abort else TDbiError(Engine,Status);
end;

Procedure TPSQLDataSet.CommitUpdates;
begin
  Check(Engine, ProcessUpdates(dbiDelayedUpdCommit));
end;

Procedure TPSQLDataSet.CancelUpdates;
begin
  Cancel;
  ProcessUpdates(dbiDelayedUpdCancel);
end;

Procedure TPSQLDataSet.RevertRecord;
var
  Status: Word;
begin
  if State in dsEditModes then Cancel;
  Status := ProcessUpdates(dbiDelayedUpdCancelCurrent);
  if not ((Status = DBIERR_NONE) or (Status = DBIERR_NOTSUPPORTED)) then
    Check(Engine, Status);
end;


Function TPSQLDataSet.UpdateStatus: TUpdateStatus;
begin
   Result := usUnModified;
end;

Function TPSQLDataSet.CachedUpdateCallBack(CBInfo: Pointer): CBRType;
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
    if Assigned(FOnUpdateRecord) then
      FOnUpdateRecord(Self, UpdateKind, UpdateAction)
    else
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
      if (E is EDatabaseError) and Assigned(FOnUpdateError) then
        FOnUpdateError(Self, EDatabaseError(E), UpdateKind, UpdateAction)
      else
      begin
        Application.HandleException(Self);
        UpdateAction := uaAbort;
      end;
    end;
  end;
  Result := CBRetCode[UpdateAction];
  if UpdateAction = uaAbort then
    FUpdateCBBuf.iErrCode := DBIERR_UPDATEABORT;
  FInUpdateCallBack := FALSE;
end;


Function TPSQLDataSet.GetUpdateRecordSet: TUpdateRecordTypes;
begin
  if Active then
  begin
    Result := TUpdateRecordTypes(Byte(GetIntProp(Engine, FHandle,
      curDELAYUPDDISPLAYOPT)));
  end
  else
    Result := [];
end;

Procedure TPSQLDataSet.SetUpdateRecordSet(RecordTypes: TUpdateRecordTypes);
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Check(Engine, Engine.SetEngProp(hDbiObj(Handle), curDELAYUPDDISPLAYOPT, Longint(Byte(RecordTypes))));
  Resync([]);
end;


Procedure TPSQLDataSet.SetUpdateObject(Value: TPSQLSQLUpdateObject);
begin
  if (Value <> FUpdateObject) then
  begin
    if Assigned(FUpdateObject) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := NIL;
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
    begin
      { If another dataset already references this updateobject, then
        remove the reference }
      if Assigned(FUpdateObject.DataSet) and (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := NIL;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

procedure TPSQLDataSet.SetOnUpdateError(UpdateEvent: TUpdateErrorEvent);
begin
  if Active then SetupCallback(UpdateCallBackRequired);
  FOnUpdateError := UpdateEvent;
end;

Function TPSQLDataSet.GetUpdatesPending: Boolean;
begin
  Result := GetIntProp(Engine, FHandle, curDELAYUPDNUMUPDATES) > 0;
end;

Procedure TPSQLDataSet.DataEvent(Event: TDataEvent; Info: Integer);


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
{$IFNDEF DELPHI_4}
{ TBDEDataSet.IProviderSupport}
Function TPSQLDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
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

Function TPSQLDataSet.PSIsSQLSupported: Boolean;
begin
  Result := TRUE;
end;

Procedure TPSQLDataSet.PSReset;
begin
  inherited PSReset;
  If Handle <> NIL then
    Engine.ForceReread(Handle);
end;
{$ENDIF}

function TPSQLDataSet.GetHandle: HDBICur;
begin
  Result := FHandle;
end;


Function TPSQLDataSet.CheckOpen(Status: Word): Boolean;
begin
  case Status of
    DBIERR_NONE: Result := TRUE;
    DBIERR_NOTSUFFTABLERIGHTS: Result := FALSE;
  else
    TDbiError(Engine, Status);
    Result := FALSE;
  end;
end;

Procedure TPSQLDataSet.Disconnect;
begin
  Close;
end;

Function TPSQLDataSet.GetDBHandle: HDBIDB;
begin
  if FDatabase <> nil then
  begin
    if FDatabase.Handle = nil then
       FDatabase.Connected := True;
    Result := FDatabase.Handle;
  end
  else
    Result := NIL;
end;

Procedure TPSQLDataSet.GetDatabaseNames(List : TStrings);
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

Procedure TPSQLDataSet.CloseDatabase(Database: TPSQLDatabase);
begin
  if Assigned(Database) then
    Database.CloseDatabase(Database);
end;

Function TPSQLDataSet.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
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
        FDatabase.RegisterClient(Self);
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
        FDatabase.UnRegisterClient(Self);
        CloseDatabase(FDatabase);
      end;
    end;
  end;
end;

Procedure TPSQLDataSet.SetUpdateMode(const Value: TUpdateMode);
begin
  if (FHandle <> NIL) and True and CanModify then
    Check(Engine, Engine.SetEngProp(hDbiObj(FHandle), curUPDLOCKMODE, Longint(Value)));
  FUpdateMode := Value;
end;

{ AutoRefresh }
Procedure TPSQLDataSet.SetAutoRefresh(const Value: Boolean);
begin
  CheckInactive;
  FAutoRefresh := Value;
end;

procedure TPSQLDataSet.SetDatabase(Value: TPSQLDatabase);
begin
   if Active then Close;
   try
     if Assigned(FDatabase) then  FDatabase.UnRegisterClient(Self);
     if Assigned(Value) then FDatabase := Value;
   finally
     FDatabase := Value;
   end;
end;

function TPSQLDataSet.GetDatabase: TPSQLDatabase;
begin
   Result := TPSQLDatabase(FDatabase);
end;

{$IFNDEF DELPHI_4}
Procedure TPSQLDataSet.SetupAutoRefresh;
const
  PropFlags : array[TAutoRefreshFlag] of LongInt = (0, curFIELDISAUTOINCR, curFIELDISDEFAULT);
var
  I       : Integer;
  ColDesc : ServerColDesc;
begin
  If AutoRefresh then
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
{$ENDIF}

{$IFNDEF DELPHI_4}
{ TPSQLDataSet.IProviderSupport }
Procedure TPSQLDataSet.PSGetAttributes(List : TList);
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

Function TPSQLDataSet.PSIsSQLBased: Boolean;
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

Function TPSQLDataSet.PSGetQuoteChar: String;
begin
  Result := '"';
end;

Function TPSQLDataSet.PSInTransaction: Boolean;
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

Procedure TPSQLDataSet.PSStartTransaction;
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

Procedure TPSQLDataSet.PSEndTransaction(Commit : Boolean);
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

Function TPSQLDataSet.PSExecuteStatement(const ASQL : string; AParams: TParams; ResultSet: Pointer = NIL): Integer;
var
  InProvider: Boolean;
//  Cursor: hDBICur;
  Q: TPSQLQuery;
begin
  InProvider := SetDBFlag(dbfProvider, TRUE);
  try
    if Assigned(ResultSet) or Assigned(AParams) then
    begin
      // >> PaGo 06.06.2007
      Q := TPSQLQuery.Create(nil);
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
       If Assigned(ResultSet) then
         TPSQLDataset(ResultSet^) := Q
       else
         Q.Free;
      end;
      // << PaGo 06.06.2007
    end
    else
      Result := Database.Execute(ASQL);
  finally
    SetDBFlag(dbfProvider, InProvider);
  end;
end;
{$ENDIF}

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

Function TPSQLQuery.Engine : TPSQLEngine;
begin
  Result := FDataBase.Engine;
end;

Function TPSQLQuery.CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream;
begin
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode);
end;

Function TPSQLQuery.IsSequenced: Boolean;
begin
  Result := FAllowSequenced and inherited IsSequenced;
end;

Procedure TPSQLQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

Procedure TPSQLQuery.SetPrepare(Value: Boolean);
begin
  if Value then
    Prepare else  UnPrepare;
end;

Procedure TPSQLQuery.Prepare;
begin
  If Assigned(FHandle) then
   begin
    SetDBFlag(dbfPrepared, TRUE);
    SetPrepared(TRUE);
   end;
end;

Procedure TPSQLQuery.UnPrepare;
begin
  SetPrepared(FALSE);
  SetDBFlag(dbfPrepared, FALSE);
end;

Procedure TPSQLQuery.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

Function TPSQLQuery.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

Procedure TPSQLQuery.SetQuery(Value: TStrings);
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

Procedure TPSQLQuery.QueryChanged(Sender: TObject);
var
  List: TPSQLParams;
begin
  if not (csReading in ComponentState) then
  begin
    Disconnect;
    StrDispose(SQLBinary);
    SQLBinary := nil;
    if ParamCheck or (csDesigning in ComponentState) then
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

Procedure TPSQLQuery.SetParamsList(Value: TPSQLParams);
begin
  FParams.AssignValues(Value);
end;

Function TPSQLQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

Procedure TPSQLQuery.DefineProperties(Filer: TFiler);

  Function WriteData: Boolean;
  begin
    if (Filer.Ancestor <> NIL) then
      Result := not FParams.IsEqual(TPSQLQuery(Filer.Ancestor).FParams)
    else
      Result := (FParams.Count > 0);
  end;

begin
  Inherited DefineProperties(Filer);
  Filer.DefineBinaryproperty('Data', ReadBinaryData, WriteBinaryData, SQLBinary <> NIL);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

Procedure TPSQLQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

Procedure TPSQLQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

Procedure TPSQLQuery.ReadBinaryData(Stream: TStream);
begin
  SQLBinary := StrAlloc(Stream.Size);
  Stream.ReadBuffer(SQLBinary^, Stream.Size);
end;

Procedure TPSQLQuery.WriteBinaryData(Stream: TStream);
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

Procedure TPSQLQuery.SetPrepared(Value: Boolean);
begin
  if (FHandle <> NIL) and Value then
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

{Procedure TPSQLQuery.FreeStatement;
var
  Result: Word;
begin
  if StmtHandle <> NIL then
  begin
    Result := Engine.QFree(FStmtHandle);
    if not (csDestroying in ComponentState) then
      Check(Engine, Result);
  end;
end;}

Procedure TPSQLQuery.SetParamsFromCursor;
var
  I: Integer;
  DataSet: TDataSet;
begin
  if FDataLink.DataSource <> NIL then
  begin
    DataSet := FDataLink.DataSource.DataSet;
    if DataSet <> NIL then
    begin
      DataSet.FieldDefs.Update;
      for I := 0 to FParams.Count - 1 do
        with FParams[I] do
          if not Bound then
          begin
            AssignField(DataSet.FieldByName(Name));
            Bound := FALSE;
          end;
    end;
  end;
end;


Procedure TPSQLQuery.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> NIL then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> NIL then
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


Function TPSQLQuery.ParamByName(const Value: String): TPSQLParam;
begin
  Result := FParams.ParamByName(Value);
end;

Function TPSQLQuery.CreateCursor(GenHandle: Boolean): HDBICur;
begin
  if SQL.Count > 0 then
  begin
    FExecSQL := not GenHandle;
    Try
       SetPrepared(TRUE);
    Finally
      FExecSQL := FALSE;
    end;
    if FDataLink.DataSource <> NIL then SetParamsFromCursor;
    Result := GetQueryCursor(GenHandle);
  end
  else
  begin
    DatabaseError(SEmptySQLStatement, Self);
    Result := NIL;
  end;
  FCheckRowsAffected := (Result = NIL);
end;


Function TPSQLQuery.CreateHandle: HDBICur;
begin
  Result := CreateCursor(TRUE)
end;


Procedure TPSQLQuery.ExecSQL;
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

Function TPSQLQuery.GetQueryCursor(GenHandle: Boolean): HDBICur;
const
  DataType: array[Boolean] of LongInt = (Ord(wantCanned), Ord(wantLive));
var
  PCursor: phDBICur;
  CanLive : boolean;
begin
  Result := NIL;
  if GenHandle then
    PCursor := @Result else
    PCursor := NIL;
  if FParams.Count > 0 then
      Check(Engine,Engine.QuerySetParams(hDBIStmt(FHandle),Params,SQL.Text));
  Check(Engine, Engine.QExec(hDBIStmt(FHandle), PCursor));
  //pasha_golub 20.12.06
  CanLive := False;
  if FRequestLive and not ForceUpdateCallback and not FExecSQL then
    CanLive := TNativeDataSet(FHandle).CheckCanLive();
  Check(Engine, Engine.SetEngProp(hDbiObj(FHandle), stmtLIVENESS, DataType[CanLive]));
  //pasha_golub 20.12.06
end;

Function TPSQLQuery.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
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

Procedure TPSQLQuery.PrepareSQL(Value: PChar);
begin
  GetStatementHandle(Value);
  if not Local then
    SetBoolProp(Engine, FHandle, stmtUNIDIRECTIONAL, FUniDirectional);
end;

Procedure TPSQLQuery.GetStatementHandle(SQLText: PChar);
const
  DataType: array[Boolean] of LongInt = (Ord(wantCanned), Ord(wantLive));
var
  DBh : HDBIDB;
begin
  DBh := DBHandle;
  Check(Engine,Engine.QAlloc(DBH, qrylangSQL, hDBIStmt(FHandle)));
  try
    TNativeDataset(FHandle).OIDAsInt := FOIDAsInt;
    TNativeDataset(FHandle).ByteaAsEscString := FByteaAsEscString;
    If not FExecSQL then
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
    FHandle := NIL;
    raise;
  end;
end;


Function TPSQLQuery.GetRowsAffected: Integer;
var
  Length: Word;
begin
  if Prepared then
    if Engine.GetEngProp(hDBIObj(FHandle), stmtROWCOUNT, @Result, SizeOf(Result), Length) <> 0 then
      Result := -1  else
  else
    Result := FRowsAffected;
end;


Procedure TPSQLQuery.GetDetailLinkFields(MasterFields, DetailFields: TList);

  Function AddFieldToList(const FieldName: string; DataSet: TDataSet;
    List: TList): Boolean;
  var
    Field: TField;
  begin
    Field := DataSet.FindField(FieldName);
    if (Field <> NIL) then
      List.Add(Field);
    Result := Field <> NIL;
  end;

var
  i: Integer;
begin
  MasterFields.Clear;
  DetailFields.Clear;
  if (DataSource <> NIL) and (DataSource.DataSet <> NIL) then
    for i := 0 to Params.Count - 1 do
      if AddFieldToList(Params[i].Name, DataSource.DataSet, MasterFields) then
        AddFieldToList(Params[i].Name, Self, DetailFields);
end;

{$IFNDEF DELPHI_4}
{ TPSQLQuery.IProviderSupport }
Function TPSQLQuery.PSGetDefaultOrder: TIndexDef;
begin
  Result := inherited PSGetDefaultOrder;
  if not Assigned(Result) then
    Result := GetIndexForOrderBy(SQL.Text, Self);
end;

Function TPSQLQuery.PSGetParams : TParams;
begin
  Result := Params;
end;

Procedure TPSQLQuery.PSSetParams(AParams : TParams);
begin
  if (AParams.Count <> 0) then
    Params.Assign(AParams);
  Close;
end;

Function TPSQLQuery.PSGetTableName: string;
begin
  Result := GetTableNameFromSQL(SQL.Text);
end;

Procedure TPSQLQuery.PSExecute;
begin
  ExecSQL;
end;

Procedure TPSQLQuery.PSSetCommandText(const CommandText : string);
begin
  if (CommandText <> '') then
    SQL.Text := CommandText;
end;
{$ENDIF}

procedure TPSQLDataSet.SetByteaAsEscString(const Value: boolean);
begin
  If FByteaAsEscString = Value then Exit;
  FByteaAsEscString := Value;
  If Assigned(FHandle) then
    TNativeDataset(FHandle).ByteaAsEscString := Value;
end;

procedure TPSQLDataSet.SetOIDAsInt(const Value: boolean);
begin
  If FOIDAsInt = Value then Exit;
  FOIDAsInt := Value;
  If Assigned(FHandle) then
    TNativeDataset(FHandle).OIDAsInt := Value;
end;

function TPSQLDataSet.GetLastInsertID(const FieldNum: integer): integer;
begin
 CheckActive;
 If Assigned(FHandle) then
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
  Result := inherited GetFieldClass(FieldType)
end;

procedure TPSQLDataSet.SortBy(FieldNames: string);
begin
	if Active then
	begin
   try
		TNativeDataSet(FHandle).SortBy(FieldNames);
		First;
   except
    FSortFieldNames := '';
    raise;
   end;
	end;
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
 If not Active or
    not Assigned(FHandle) or
    not TNativeDataSet(FHandle).IsSortedLocally then
  FSortFieldNames := '';
 Result := FSortFieldNames;
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
    FDataSet.UpdateObject := NIL;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  Inherited Destroy;
end;

Procedure TPSQLUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
var RN, RC: integer;
begin
  with Query[UpdateKind] do
  begin
    Prepare;
    ExecSQL;
    if Assigned(FDataSet) then
    begin
       RN := TNativeDataset(FDataset.Handle).RecordNumber;
       TNativeDataset(FDataset.Handle).OpenTable;
//       FDataset.DisableControls;
       TNativeDataset(FDataset.Handle).RecordState := tsPos;
       If UpdateKind <> ukDelete then
         TNativeDataset(FDataset.Handle).SetRowPosition(-1,0,FDataset.ActiveBuffer)
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
//       FDataSet.EnableControls;
    end;
    If Assigned(FRecordChangeCompleteEvent) then
      FRecordChangeCompleteEvent(FDataset,UpdateKind);
  end;
end;

Function TPSQLUpdateSQL.GetQueryClass : TPSQLQueryClass;
begin
  Result := TPSQLQuery;
end;

Function TPSQLUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TPSQLQuery;
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

Function TPSQLUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

Function TPSQLUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

Function TPSQLUpdateSQL.GetDataSet: TPSQLDataSet;
begin
  Result := FDataSet;
end;

Procedure TPSQLUpdateSQL.SetDataSet(ADataSet: TPSQLDataSet);
begin
  FDataSet := ADataSet;
end;

Procedure TPSQLUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

Procedure TPSQLUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

Procedure TPSQLUpdateSQL.SQLChanged(Sender: TObject);
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

Procedure TPSQLUpdateSQL.SetParams(UpdateKind: TUpdateKind);
var
  I: Integer;
  Old: Boolean;
  Param: TPSQLParam;
  PName: String;
  Field: TField;
begin
  if not Assigned(FDataSet) then Exit;
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

Procedure TPSQLUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

///////////////////////////////////////////////////////////////////////////////
//                         TPSQLTable                                       //
///////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////
//Constructor : TPSQLTable.Create
//Description : TPSQLTable conponent
//////////////////////////////////////////////////////////
//Input       : AOwner: TComponent
//////////////////////////////////////////////////////////
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
   If FLimit <> Value then
      FLimit := Value;
end;

Function TPSQLTable.GetHandle(const IndexName, IndexTag: String): HDBICur;
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
   Check(Engine,Engine.GetTableProps(DBHandle,NativeTableName,FOwner,
        FComment,FTablespace,FHasOIDs,FTableID));
  end;

begin
  Result := NIL;
  OpenMode := OpenModes[FReadOnly];
  if DefaultIndex then
    IndexID := 0  else IndexID := NODEFAULTINDEX;
  while TRUE do
  begin
    DBH := DBHandle;
    RetCode := Engine.OpenTable(DBH, NativeTableName, GetTableTypeName,
      PChar(IndexName), PChar(IndexTag), IndexID, OpenMode, ShareModes[FExclusive],
      xltField, FALSE, NIL, Result, FLimit, FOffset);
    if RetCode = DBIERR_TABLEREADONLY then
      OpenMode := dbiReadOnly    else
    FillAddonProps;
    if CheckOpen(RetCode) then  Break;
  end;
end;

Function TPSQLTable.Engine : TPSQLEngine;
begin
  Result := FDataBase.Engine;
end;

Function TPSQLTable.CreateBlobStream(Field : TField; Mode : TBlobStreamMode) : TStream;
begin
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode);
end;

Function TPSQLTable.IsSequenced: Boolean;
begin
  Result := FAllowSequenced and inherited IsSequenced;
end;

Function TPSQLTable.CreateHandle: HDBICur;
var
  IndexName, IndexTag: String;
begin
  if FTableName = '' then  DatabaseError(SNoTableName, Self);
  IndexDefs.Updated := FALSE;
  GetIndexParams(FIndexName, FFieldsIndex, IndexName, IndexTag);
  Result := GetHandle(IndexName, IndexTag);
  TNativeDataset(Result).OIDAsInt := FOIDAsInt;
  TNativeDataset(Result).ByteaAsEscString := FByteaAsEscString;
end;

Function TPSQLTable.GetLanguageDriverName: string;
begin
  Result := '';
end;

Procedure TPSQLTable.PrepareCursor;
begin
  CheckMasterRange;
end;

Procedure TPSQLTable.DefChanged(Sender: TObject);
begin
  StoreDefs := TRUE;
end;

Procedure TPSQLTable.InitFieldDefs;
var
  I, FieldID, FldDescCount: Integer;
  FieldDescs: TFieldDescList;
  FCursor: HDBICur;
  RequiredFields: TBits;
begin
  if FHandle <> NIL then
     InternalInitFieldDefs else
  begin
    SetDBFlag(dbfFieldList, TRUE);
    try
      if (FTableName = '') then  DatabaseError(SNoTableName, Self);
        while not CheckOpen(Engine.OpenFieldList(DBHandle, NativeTableName,
          GetTableTypeName, FALSE, FCursor)) do {Retry};
        try
          Check(Engine, Engine.GetRecordCount(FCursor, FldDescCount));
          SetLength(FieldDescs, FldDescCount);
          { Create an array of field descriptors }
          for I := 0 to FldDescCount - 1 do
            Check(Engine, Engine.GetNextRecord(FCursor, dbiNoLock, @FieldDescs[I], NIL));
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

Procedure TPSQLTable.DestroyHandle;
begin
  DestroyLookupCursor;
  Inherited DestroyHandle;
end;

Procedure TPSQLTable.DecodeIndexDesc(const IndexDesc: IDXDesc;
  var Source, Name, FieldExpression, DescFields: string;
  var Options: TIndexOptions);

  Procedure ConcatField(var FieldList: string; const FieldName: string);
  begin
    if FieldList = '' then
      FieldList := FieldName else
      FieldList := Format('%s;%s', [FieldList, FieldName]);
  end;

var
  IndexOptions: TIndexOptions;
  I: Integer;
  SSource, SName: PChar;
  FieldName: String;
begin
  with IndexDesc do
  begin
    if szTagName[0] = #0 then
    begin
      SName := szName;
      Source := '';
    end
    else
    begin
      SSource := szName;
      SName := szTagName;
      TNativeToAnsi(Engine, SSource, Source);
    end;
    TNativeToAnsi(Engine, SName, Name);
    Name := ExtractFileName(Name);
    Source := ExtractFileName(Source);
    IndexOptions := [];
    if bPrimary then Include(IndexOptions, ixPrimary);
    if bUnique then Include(IndexOptions, ixUnique);
    if bDescending then Include(IndexOptions, ixDescending);
    if bCaseInsensitive then Include(IndexOptions, ixCaseInsensitive);
    if not bMaintained then Include(IndexOptions, ixNonMaintained);
    if bExpIdx then
    begin
      TNativeToAnsi(Engine, szKeyExp, FieldExpression);
      Include(IndexOptions, ixExpression);
    end else
    begin
      FieldExpression := '';
      DescFields := '';
      for I := 0 to iFldsInKey - 1 do
      begin
        FieldName := FieldDefList[aiKeyFld[I] - 1].Name;
        ConcatField(FieldExpression, FieldName);
        if abDescending[I] then
          ConcatField(DescFields, FieldName);
      end;
      if bDescending and (DescFields = FieldExpression) then  DescFields := '';
    end;
    Options := IndexOptions;
  end;
end;

Procedure TPSQLTable.EncodeIndexDesc(var IndexDesc: IDXDesc;
  const Name, FieldExpression: string; Options: TIndexOptions;
  const DescFields: string);

  Function IndexFieldOfs(const FieldName: string): Integer;
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
    TAnsiToNative(Engine, Name, szName, SizeOf(szName) - 1);
    bPrimary    := ixPrimary in Options;
    bUnique     := ixUnique in Options;
    bDescending := (ixDescending in Options) and (DescFields = '');
    bMaintained := not (ixNonMaintained in Options);
    Word(bCaseInsensitive) := Word(ixCaseInsensitive in Options);
    if ixExpression in Options then
    begin
      bExpIdx := TRUE;
      TAnsiToNative(Engine, FieldExpression, szKeyExp, SizeOf(szKeyExp) - 1);
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

Procedure TPSQLTable.AddIndex(const Name, Fields: string; Options: TIndexOptions;
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
    Check(Engine, Engine.AddIndex(DBHandle, Handle, NIL, NIL, IndexDesc, NIL));
  end
  else
  begin
      EncodeIndexDesc(IndexDesc, Name, Fields, Options, DescFields);
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.AddIndex(DBHandle, NIL, NativeTableName, GetTableTypeName, IndexDesc, NIL));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
  IndexDefs.Updated := FALSE;
end;

Procedure TPSQLTable.DeleteIndex(const Name: String);
var
  IndexName, IndexTag: String;
begin
  if Active then
  begin
    GetIndexParams(Name, FALSE, IndexName, IndexTag);
    CheckBrowseMode;
    Check(Engine, Engine.DeleteIndex(DBHandle, Handle, NIL, NIL, PChar(IndexName), PChar(IndexTag), 0));
  end
  else
  begin
    GetIndexParams(Name, FALSE, IndexName, IndexTag);
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.DeleteIndex(DBHandle, NIL, NativeTableName, GetTableTypeName,
        PChar(IndexName), PChar(IndexTag), 0));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
  FIndexDefs.Updated := FALSE;
end;

Function TPSQLTable.GetIndexFieldNames: String;
begin
    if FFieldsIndex then Result := FIndexName else Result := '';
end;

Function TPSQLTable.GetIndexName: String;
begin
  if FFieldsIndex then Result := '' else Result := FIndexName;
end;

Procedure TPSQLTable.GetIndexNames(List: TStrings);
begin
  IndexDefs.Update;
  IndexDefs.GetItemNames(List);
end;

Procedure TPSQLTable.GetIndexParams(const IndexName: String;
  FieldsIndex: Boolean; var IndexedName, IndexTag: String);
var
  IndexStr: TIndexName;
  SIndexName: DBIMSG;
  SIndexTag: DBIPATH;
begin
  SIndexName[0] := #0;
  SIndexTag[0] := #0;
  if IndexName <> '' then
  begin
    IndexDefs.Update;
    IndexStr := IndexName;
    if FieldsIndex then
       IndexStr := IndexDefs.FindIndexForFields(IndexName).Name;
     TAnsiToNative(Engine, IndexStr, SIndexName, SizeOf(SIndexName) - 1);
  end;
  IndexedName := SIndexName;
  IndexTag := SIndexTag;
end;

Procedure TPSQLTable.SetIndexDefs(Value: TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;

Procedure TPSQLTable.SetIndex(const Value: String; FieldsIndex: Boolean);
var
  IndexName, IndexTag: String;
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

Procedure TPSQLTable.SetIndexFieldNames(const Value: String);
begin
    SetIndex(Value, Value <> '');
end;

Procedure TPSQLTable.SetIndexName(const Value: String);
begin
  SetIndex(Value, FALSE);
end;

Procedure TPSQLTable.UpdateIndexDefs;
var
  Opts: TIndexOptions;
  IdxName, Src, Flds, DescFlds: string;

  Procedure UpdateFromCursor;
  var
    I: Integer;
    Cursor: HDBICur;
    CursorProps: CurProps;
    IndexDescs: TIndexDescList;
  begin
    if Handle = nil then
       Cursor := GetHandle('', '') else
       Cursor := Handle;
    try
      Engine.GetCursorProps(Cursor, CursorProps);
      if CursorProps.iIndexes > 0 then
      begin
        SetLength(IndexDescs, CursorProps.iIndexes);
        Engine.GetIndexDescs(Cursor, PIDXDesc(IndexDescs));
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

  Procedure UpdateFromIndexList;
  var
    FCursor: HDBICur;
    IndexDesc: IDXDesc;
  begin
    while not CheckOpen(Engine.OpenIndexList(DBHandle, NativeTableName, GetTableTypeName, FCursor)) do {Retry};
    try
        while Engine.GetNextRecord(FCursor, dbiNoLock, @IndexDesc, NIL) = 0 do
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

Function TPSQLTable.GetExists: Boolean;
var
  E: Word;
begin
  Result := Active;
  if Result or (TableName = '') then  Exit;
  SetDBFlag(dbfTable, TRUE);
  try
    E := Engine.TableExists(DBHandle, NativeTableName);
    Result := (E = DBIERR_NONE);
  finally
    SetDBFlag(dbfTable, FALSE);
  end;
end;

Function TPSQLTable.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  Result := GotoKey;
end;

Procedure TPSQLTable.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  GotoNearest;
end;

{$HINTS OFF}
Function TPSQLTable.GotoKey: Boolean;
var
  KeyBuffer: PKeyBuffer;
  IndexBuffer, RecBuffer: PChar;
  UseKey: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  KeyBuffer := GetKeyBuffer(kiLookup);
  IndexBuffer := AllocMem(KeySize);
  try
    RecBuffer := PChar(KeyBuffer) + SizeOf(TKeyBuffer);
    UseKey := Engine.ExtractKey(Handle, RecBuffer, IndexBuffer) = 0;
    if UseKey then RecBuffer := IndexBuffer;
    Result := Engine.GetRecordForKey(Handle, UseKey, KeyBuffer^.FieldCount, 0, RecBuffer, nil,True) = 0;
    if Result then Resync([rmExact, rmCenter]);
    if Result then DoAfterScroll;
  finally
    FreeMem(IndexBuffer, KeySize);
  end;
end;

Procedure TPSQLTable.GotoNearest;
var
  SearchCond: DBISearchCond;
  KeyBuffer: PKeyBuffer;
  IndexBuffer, RecBuffer: PChar;
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
    RecBuffer := PChar(KeyBuffer) + SizeOf(TKeyBuffer);
    UseKey := Engine.ExtractKey(Handle,RecBuffer,IndexBuffer) = 0;
    if UseKey then RecBuffer := IndexBuffer;

    If Engine.GetRecordForKey(Handle, {SearchCond,} UseKey, KeyBuffer^.FieldCount, 0,RecBuffer, nil, False) = 0
       then  Resync([rmCenter]);
  finally
    FreeMem(IndexBuffer, KeySize);
  end;
end;
{$HINTS ON}

Procedure TPSQLTable.SetKey;
begin
  SetKeyBuffer(kiLookup, TRUE);
end;

Procedure TPSQLTable.EditKey;
begin
  SetKeyBuffer(kiLookup, FALSE);
end;

Procedure TPSQLTable.ApplyRange;
begin
  CheckBrowseMode;
  if SetCursorRange then  First;
end;

Procedure TPSQLTable.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  if ResetCursorRange then   Resync([]);
end;

Procedure TPSQLTable.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiRangeStart, StartValues);
  SetKeyFields(kiRangeEnd, EndValues);
  ApplyRange;
end;

Procedure TPSQLTable.SetRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, TRUE);
end;

Procedure TPSQLTable.SetRangeStart;
begin
  SetKeyBuffer(kiRangeStart, TRUE);
end;

Procedure TPSQLTable.EditRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, FALSE);
end;

Procedure TPSQLTable.EditRangeStart;
begin
  SetKeyBuffer(kiRangeStart, FALSE);
end;

Procedure TPSQLTable.UpdateRange;
begin
  SetLinkRanges(FMasterLink.Fields);
end;

function TPSQLTable.GetBatchModify: Boolean;
var
  Len : Word;
begin
   if FHandle <> nil then
      Engine.GetEngProp(hDBIObj(FHandle), curAUTOREFETCH,@Result, SizeOf(Result),Len);
end;

procedure TPSQLTable.SetBatchModify(const Value : Boolean);
begin
   if FHandle = nil then Exit;
   If Value then
      Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),curAUTOREFETCH,LongInt(TRUE))) else
      begin
         Check(Engine, Engine.SetEngProp(hDbiObj(FHandle),curAUTOREFETCH,LongInt(FALSE)));
         Refresh;
      end;
end;


Function TPSQLTable.GetLookupCursor(const KeyFields: String;
  CaseInsensitive: Boolean): HDBICur;
var
  IndexFound, FieldsIndex: Boolean;
  KeyIndexName, IndexName, IndexTag: String;
  KeyIndex: TIndexDef;
begin
  if (KeyFields <> FLookupKeyFields) or
     (CaseInsensitive <> FLookupCaseIns) then
  begin
    DestroyLookupCursor;
    IndexFound := FALSE;
    FieldsIndex := FALSE;
    { If a range is active, don't use a lookup cursor }
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
        if (KeyIndex <> NIL) and
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
        Check(Engine, Engine.SwitchToIndex(FLookupHandle, PChar(IndexName), PChar(IndexTag), 0, FALSE));
      end;
      FLookupKeyFields := KeyFields;
      FLookupCaseIns := CaseInsensitive;
    end;
  end;
  Result := FLookupHandle;
end;

Procedure TPSQLTable.DestroyLookupCursor;
begin
  if FLookupHandle <> NIL then
  begin
    Engine.CloseCursor(FLookupHandle);
    FLookupHandle := NIL;
    FLookupKeyFields := '';
  end;
end;

Procedure TPSQLTable.GotoCurrent(Table: TPSQLTable);
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

Procedure TPSQLTable.GetDetailLinkFields(MasterFields, DetailFields: TList);
var
  i: Integer;
  Idx: TIndexDef;
begin
  MasterFields.Clear;     
  DetailFields.Clear;
  if (MasterSource <> NIL) and (MasterSource.DataSet <> NIL) and (Self.MasterFields <> '') then
  begin
    Idx := NIL;
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
    if Idx <> NIL then GetFieldList(DetailFields, Idx.Fields);
  end;
end;

Procedure TPSQLTable.CheckMasterRange;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    SetLinkRanges(FMasterLink.Fields);
    SetCursorRange;
  end;
end;

Procedure TPSQLTable.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  UpdateRange;
  ApplyRange;
end;

Procedure TPSQLTable.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;

Function TPSQLTable.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

Procedure TPSQLTable.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then
    DatabaseError(SCircularDataLink, Self);
  FMasterLink.DataSource := Value;
end;

Function TPSQLTable.GetMasterFields: String;
begin
  Result := FMasterLink.FieldNames;
end;

Procedure TPSQLTable.SetMasterFields(const Value: String);
begin
  FMasterLink.FieldNames := Value;
end;

Procedure TPSQLTable.DoOnNewRecord;
var
  I: Integer;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    for I := 0 to Pred(FMasterLink.Fields.Count) do
      IndexFields[I] := TField(FMasterLink.Fields[I]);
  Inherited DoOnNewRecord;
end;

{New 29.05.2001}
procedure TPSQLTable.CreateTable;
var
  IndexDescs: TIndexDescList;
  TableDesc: CRTblDesc;
  FieldDescs: TFieldDescList;
  ValChecks: TValCheckList;
  LvlFldDesc: FLDDesc;
  Level: DBINAME;

  procedure InitTableSettings;
  begin
    FillChar(TableDesc, SizeOf(TableDesc), 0);
    with TableDesc do
    begin
      TAnsiToNative(Engine,TableName,szTblName, SizeOf(szTblName) - 1);
      if FTableLevel > 0 then
      begin
        iOptParams := 1;
        StrCopy(@Level, PChar(IntToStr(FTableLevel)));
        pOptData := @Level;
        StrCopy(LvlFldDesc.szName, 'LEVEL');
        LvlFldDesc.iLen := StrLen(Level) + 1;
        LvlFldDesc.iOffset := 0;
        pfldOptParams :=  @LvlFldDesc;
      end;
    end;
  end;

  procedure InitFieldDescriptors;
  var
    I: Integer;
    TempFieldDescs: TFieldDescList;
  begin
    with TableDesc do
    begin
      InitFieldDefsFromFields;
      iFldCount := FieldDefs.Count;
      SetLength(TempFieldDescs, iFldCount);
      for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
      begin
        EncodeFieldDesc(TempFieldDescs[I], Name, DataType, Size, Precision);
        if Required then Inc(iValChkCount);
      end;
      SetLength(FieldDescs, iFldCount);
      pFldDesc := PSQLTypes.PFLDDesc(FieldDescs);
      Check(Engine,Engine.TranslateRecordStructure(nil,iFldCount,PSQLTypes.PFLDDesc(TempFieldDescs),nil,nil,pFLDDesc,False));
    end;
  end;

  procedure InitIndexDescriptors;
  var
    I: Integer;
  begin
    TableDesc.iIdxCount := IndexDefs.Count;
    SetLength(IndexDescs, TableDesc.iIdxCount);
    TableDesc.pIdxDesc := PIDXDesc(IndexDescs);
    for I := 0 to IndexDefs.Count - 1 do
    with IndexDefs[I] do
      EncodeIndexDesc(IndexDescs[I], Name, FieldExpression, Options, DescFields);
  end;

  procedure InitValChecks;
  var
    I, ValCheckNo: Integer;
  begin
    with TableDesc do
    if iValChkCount > 0 then
    begin
      SetLength(ValChecks, iValChkCount);
      ValCheckNo := 0;
      for I := 0 to FieldDefs.Count - 1 do
        if FieldDefs[I].Required then
        begin
          ValChecks[ValCheckNo].iFldNum := I + 1;
          ValChecks[ValCheckNo].bRequired := True;
          Inc(ValCheckNo);
        end;
      pvchkDesc := PSQLTypes.pVCHKDesc(ValChecks);
    end;
  end;

begin
  CheckInactive;
  SetDBFlag(dbfTable, True);
  try
    InitTableSettings;
    InitFieldDescriptors;
    InitIndexDescriptors;
    InitValChecks;
    Check(Engine,Engine.CreateTable(DBHandle, True, TableDesc));
  finally
    SetDBFlag(dbfTable, False);
  end;
end;

Procedure TPSQLTable.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    Check(Engine, Engine.EmptyTable(DBHandle, Handle, NIL, NIL));
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end else
  begin
    SetDBFlag(dbfTable, TRUE);
    try
      Check(Engine, Engine.EmptyTable(DBHandle, NIL, NativeTableName, GetTableTypeName));
    finally
      SetDBFlag(dbfTable, FALSE);
    end;
  end;
end;

Procedure TPSQLTable.LockTable(LockType: TPSQLLockType);
begin
  SetTableLock(LockType, TRUE);
end;

Procedure TPSQLTable.SetTableLock(LockType: TPSQLLockType; Lock: Boolean);
var
  L: DBILockType;
begin
  CheckActive;
  if LockType = mltReadLock then L := dbiREADLOCK else L := dbiWRITELOCK;
  if Lock then
    Check(Engine, Engine.AcqTableLock(Handle, L)) else
    Check(Engine, Engine.RelTableLock(Handle, False, L));
end;

Procedure TPSQLTable.UnlockTable;
begin
  SetTableLock(mltReadLock, FALSE);
end;

Procedure TPSQLTable.EncodeFieldDesc(var FieldDesc: FLDDesc;
  const Name: string; DataType: TFieldType; Size, Precision: Integer);
begin
  with FieldDesc do
  begin
    TAnsiToNative(Engine, Name, szName, SizeOf(szName) - 1);
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

Procedure TPSQLTable.DataEvent(Event: TDataEvent; Info: Longint);
begin
  if Event = depropertyChange then
     IndexDefs.Updated := FALSE;
  Inherited DataEvent(Event, Info);
end;

Function TPSQLTable.GetCanModify: Boolean;
begin
  Result := Inherited GetCanModify and not ReadOnly;
end;

Function TPSQLTable.GetTableTypeName: PChar;
begin
  Result := NIL;
end;

Function TPSQLTable.GetTableLevel: Integer;
begin
  if Handle <> nil then
    Result := GetIntProp(Engine, Handle, curTABLELEVEL) else
    Result := FTableLevel;
end;

Function TPSQLTable.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

Function TPSQLTable.IndexDefsStored: Boolean;
begin
  Result := StoreDefs and (IndexDefs.Count > 0);
end;

Function TPSQLTable.GetFileName: string;
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

Function TPSQLTable.GetTableType: TTableType;
begin
  Result := ttDefault;
end;

Function TPSQLTable.NativeTableName: PChar;
begin
 //attention, cast type. Need to getmem in other cases
 //pasha_golub 23.12.04
  Result := PChar(FTableName);
end;

Procedure TPSQLTable.SetExclusive(Value: Boolean);
begin
  CheckInactive;
  FExclusive := Value;
end;

Procedure TPSQLTable.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

Procedure TPSQLTable.SetTableName(const Value: TFileName);
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

{$IFNDEF DELPHI_4}
{ TTable.IProviderSupport }
Function TPSQLTable.PSGetDefaultOrder: TIndexDef;

  Function GetIdx(IdxType : TIndexOption) : TIndexDef;
  var
    i: Integer;
  begin
    Result := NIL;
    for i := 0 to IndexDefs.Count - 1 do
      if IdxType in IndexDefs[i].Options then
      try
        Result := IndexDefs[ i ];
        GetFieldList(NIL, Result.Fields);
        break;
      except
        Result := NIL;
      end;
  end;

var
  DefIdx: TIndexDef;
begin
  DefIdx := NIL;
  IndexDefs.Update;
  try
    if (IndexName <> '') then
      DefIdx := IndexDefs.Find(IndexName)
    else
      if (IndexFieldNames <> '') then
        DefIdx := IndexDefs.FindIndexForFields(IndexFieldNames);
    if Assigned(DefIdx) then
      GetFieldList(NIL, DefIdx.Fields);
  except
    DefIdx := NIL;
  end;
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixPrimary);
  if not Assigned(DefIdx) then
    DefIdx := GetIdx(ixUnique);
  if Assigned(DefIdx) then
  begin
    Result := TIndexDef.Create(NIL);
    Result.Assign(DefIdx);
  end
  else
    Result := NIL;
end;

Function TPSQLTable.PSGetIndexDefs(IndexTypes : TIndexOptions): TIndexDefs;
begin
  Result := GetIndexDefs(IndexDefs, IndexTypes);
end;

Function TPSQLTable.PSGetTableName: string;
begin
  Result := TableName;
end;

Procedure TPSQLTable.PSSetParams(AParams: TParams);

  Procedure AssignFields;
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

Procedure TPSQLTable.PSSetCommandText(const CommandText : string);
begin
  if CommandText <> '' then
    TableName := CommandText;
end;

Function TPSQLTable.PSGetKeyFields: string;
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
            IndexFound := (FindField(ExtractFieldName(Result, Pos)) <> NIL);
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
  if FDataSet.State = dsFilter then DatabaseErrorFmt(SNoFieldAccess, [FField.DisplayName], FDataSet);
  if not FField.Modified then
  begin
    if Mode = bmRead then
    begin
      FCached := FDataSet.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
                 (FField.IsNull or (FDataSet.GetBlobData(FField, FBuffer) <> ''));
      OpenMode := dbiReadOnly;
      if PositionDataset then
         if not FDataSet.GetCurrentRecord(FBuffer) then Exit;
    end else
    begin
      FDataSet.SetBlobData(FField, FBuffer, '');
      if FField.ReadOnly then DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);
      if not (FDataSet.State in [dsEdit, dsInsert]) then DatabaseError(SNotEditing, FDataSet);
      OpenMode := dbiReadWrite;
    end;

    if not FCached then
    begin
      if (FDataSet.State in [dsBrowse,dsInsert,dsEdit]) and (Mode = bmRead) then
        FDataSet.GetCurrentRecord(FDataSet.ActiveBuffer); //#363; pg: 18.01.07
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
    if not FField.Modified and not FCached then  Engine.FreeBlob(FDataSet.Handle, FBuffer, FFieldNo);
  end;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    Application.HandleException(Self);
  end;
end;

Function TPSQLBlobStream.Engine : TPSQLEngine;
begin
  Result := FDataSet.Engine;
end;

function TPSQLBlobStream.PositionDataset: Boolean;
begin
   Result := True;
end;

Function TPSQLBlobStream.Read(var Buffer; Count: Longint): Longint;
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
        Move(PChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end;
    end else
    begin
      Status := Engine.GetBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, @Buffer, Result);
      case Status of
        DBIERR_NONE, DBIERR_ENDOFBLOB:
          begin
            if FField.Transliterate then
              TNativeToAnsiBuf(Engine, @Buffer, @Buffer, Result);
            if FDataset.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
              (FMode = bmRead) and not FField.Modified and (FPosition = FCacheSize) then
            begin
              FCacheSize := FPosition + Result;
              SetLength(FBlobData, FCacheSize);
              Move(Buffer, PChar(FBlobData)[FPosition], Result);
              if FCacheSize = Size then
              begin
                FDataSet.SetBlobData(FField, FBuffer, FBlobData);
                FBlobData := '';
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

Function TPSQLBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: Pointer;
begin
  Result := 0;
  if FOpened then
  begin
    if FField.Transliterate then
    begin
      GetMem(Temp, Count+1);
      try
        TAnsiToNativeBuf(Engine, @Buffer, Temp, Count);
        Check(Engine, Engine.PutBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, Temp));
      finally
        FreeMem(Temp, Count+1);
      end;
    end else
      Check(Engine, Engine.PutBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition, Count, @Buffer));
    Inc(FPosition, Count);
    Result := Count;
    FModified := TRUE;
    FDataSet.SetBlobData(FField, FBuffer, '');
  end;
end;


Function TPSQLBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

Procedure TPSQLBlobStream.Truncate;
begin
  if FOpened then
  begin
    Check(Engine, Engine.TruncateBlob(FDataSet.Handle, FBuffer, FFieldNo, FPosition));
    FModified := TRUE;
    FDataSet.SetBlobData(FField, FBuffer, '');
  end;
end;

Function TPSQLBlobStream.GetBlobSize: Longint;
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

Procedure TPSQLQueryDataLink.ActiveChanged;
begin
  if FQuery.Active then FQuery.RefreshParams;
end;

Function TPSQLQueryDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FQuery;
end;

Procedure TPSQLQueryDataLink.RecordChanged(Field : TField);
begin
  if (Field = nil)and FQuery.Active then FQuery.RefreshParams;
end;

Procedure TPSQLQueryDataLink.CheckBrowseMode;
begin
  if FQuery.Active then  FQuery.CheckBrowseMode;
end;


constructor TPSQLNotify.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenList := TStringList.Create;
  with TStringList(FListenList) do
  begin
    Duplicates := dupIgnore;
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
  FBackupList := TStringList.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  SetInterval(250);
//  FTimer.Interval := 250;
  FTimer.OnTimer := ListenProc;
  FActive := False;
  FFirstConnect := True;
end;

destructor TPSQLNotify.Destroy;
begin
  CloseNotify;
  FListenList.Free;
  FBackupList.Free;
  FTimer.Free;
  if FHandle <> nil then
     Engine.ClosePGNotify(FHandle);
  inherited Destroy;
end;

procedure TPSQLNotify.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

function TPSQLNotify.GetInterval;
begin
  Result := FTimer.Interval;
end;

procedure TPSQLNotify.SetListenList(Value: TStrings);
var
  I: Integer;
begin
  FListenList.Assign(Value);
  for I := 0 to FListenList.Count -1 do
    FListenList[I] := Trim(FListenList[I]);
end;

procedure TPSQLNotify.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
       OpenNotify else
       CloseNotify;
  end;
end;

procedure TPSQLNotify.SetDatabase(Value: TPSQLDatabase);
begin
  if FDatabase <> Value then
  begin
    CloseNotify;
    if FDatabase <> nil then FDatabase.RemoveNotify(Self);
    FDatabase := Value;
    if FDatabase <> nil then FDatabase.AddNotify(Self);
  end;
end;

procedure TPSQLNotify.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDatabase) and (Operation = opRemove) then
  begin
    CloseNotify;
    if  FDatabase <> nil then FDatabase.RemoveNotify(Self);
    FDatabase := nil;
  end;
end;

procedure TPSQLNotify.ListenChanging(Sender: TObject);
begin
  if not Active then Exit;
  FBackupList.Text:=FListenList.Text;
end;

procedure TPSQLNotify.ListenChange(Sender: TObject);
var
  I: Integer;
begin
  if not Active then Exit;
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
  end;
  try
    for I := 0 to FBackupList.Count-1 do
    begin
      if FListenList.IndexOf(FBackupList[I]) = -1 then
         Check(Engine,Engine.UnlistenTo(FHandle,PChar(Trim(FBackupList[I]))));
    end;
    for I := 0 to FListenList.Count-1 do
    begin
      if FBackupList.IndexOf(FListenList[I])=-1 then
         Check(Engine,Engine.ListenTo(fHandle,PChar(Trim(FListenList[I]))));
    end;
  finally
    with TStringList(FListenList) do
    begin
      OnChange := ListenChange;
      OnChanging := ListenChanging;
    end;
    FBackupList.Clear;
  end;
end;

procedure TPSQLNotify.ListenProc(Sender: TObject);
begin
  if not Active then
     FTimer.Enabled := False else
     CheckEvents;
end;

procedure TPSQLNotify.CheckActive;
begin
  if not Assigned(FDatabase) then DatabaseError('Property Database not set!');
  if not Active then DatabaseError('TPSQLNotify not in active mode');
end;

procedure TPSQLNotify.Loaded;
begin
  inherited Loaded;
  if FAutoOpen then
  begin
    FAutoOpen := False;
    OpenNotify;
  end;
end;

Function TPSQLNotify.Engine : TPSQLEngine;
begin
   Result := FDataBase.Engine;
end;

function TPSQLNotify.CreateHandle:hDBIObj;
var
  PObj: phDBIObj;
begin
   PObj := @Result;
   Check(Engine, Engine.OpenPGNotify(FDatabase.Handle, PObj^));
end;

procedure TPSQLNotify.OpenNotify;
var
  I: Integer;
begin
  if Active then Exit;
  if not Assigned(FDatabase) and (csLoading in ComponentState) then
  begin
    FAutoOpen := True;
    Exit;
  end;
  if not Assigned(FDatabase) then DatabaseError('Property Database not set!');
  if not FDatabase.Connected then FDatabase.Open;
  If FHandle = nil then FHandle := CreateHandle;
  for I := 0 to FListenList.Count-1 do
      Check(Engine,Engine.ListenTo(FHandle,PChar(FListenList[I])));
  FActive := True;
  FTimer.Enabled := True;
end;

procedure TPSQLNotify.CloseNotify;
var
  I: Integer;
begin
  if not Active then Exit;
  FActive := False;
  FTimer.Enabled := False;
  for I := 0 to FListenList.Count-1 do
      Check(Engine,Engine.UnlistenTo(FHandle,PChar(FListenList[I])));
end;

procedure TPSQLNotify.ListenTo(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.ListenTo(FHandle,PChar(Trim(Event))));
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
    if IndexOf(Event) = -1 then Append(Event);
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
end;

procedure TPSQLNotify.SendNotify(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.DoNotify(FHandle,PChar(Event)));
end;

procedure TPSQLNotify.UnlistenTo(Event: string);
begin
  CheckActive;
  Check(Engine,Engine.UnlistenTo(FHandle,PChar(Trim(Event))));
  with TStringList(FListenList) do
  begin
    OnChange := nil;
    OnChanging := nil;
    Delete(IndexOf(Event));
    OnChange := ListenChange;
    OnChanging := ListenChanging;
  end;
end;

procedure TPSQLNotify.CheckEvents;
var
  Notify : string;
  Pid    : Integer;
begin
  CheckActive;
  while True do
  begin
    Check(Engine,Engine.CheckEvents(FHandle,Pid,Notify));
    if Notify = '' then Break;
//    if FListenList.IndexOf(Notify) >= 0 then
       if Assigned(FNotifyFired) then FNotifyFired(Self, Notify, Pid);
  end;
end;

var
  SaveInitProc: Pointer;
  NeedToUninitialize: Boolean;

Procedure InitDBTables;
begin
  if (SaveInitProc <> NIL) then
    TProcedure(SaveInitProc);
  NeedToUninitialize := Succeeded(CoInitialize(NIL));
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
  SEmptyProcedureName          = 'Procedure name is empty';
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
 If Mode = bmRead then
  Result := TPSQLBlobStream.Create(Field as TBlobField, Mode)
 else
  raise EPSQLDatabaseError.CreateFmt(SCantCreateWriteBLOB,[]);
end;

function TPSQLStoredProc.CreateCursor(IsExecProc : boolean): HDBICur;
var
  PCursor: phDBICur;
begin
  Result := nil;

  if Database=nil then
    DatabaseError(Format(SDatabaseProperty, [Self.Name]),Self);

  if Length(FProcName) = 0 then
  begin
    DatabaseError(SEmptyProcedureName, Self);
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

	Check(Engine, Engine.QExec(hDBIStmt(FHandle), PCursor));

  Check(Engine, Engine.QGetProcParams(hDBIStmt(FHandle), FParams));
end;

function TPSQLStoredProc.CreateHandle: HDBICur;
begin
	 Result := HDBICur(CreateCursor(false));
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

function TPSQLStoredProc.ParamByName(const Value: String): TPSQLParam;
begin
  Result := FParams.ParamByName(Value);
end;

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

procedure TPSQLStoredProc.RefreshParams;
var
  Desc: ^SPParamDesc;
  Buffer: DBISPNAME;
  ParamName: string;
  ParamDataType: TFieldType;
  List : TList;
  i:integer;
begin
   if not FNeedRefreshParams then Exit;
   List := TList.Create;
   try
    TAnsiToNative(Engine, StoredProcName, Buffer, SizeOf(Buffer)-1);
    FParams.Clear;
    if Engine.OpenStoredProcParams(DBHandle, Buffer, FOverload, List) = 0 then
      for i:=0 to List.Count-1 do
       begin
        Desc := List[i];
        with Desc^ do
        begin
          TNativeToAnsi(Engine, szName, ParamName);
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
            Name := ParamName;
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
 If FTableSpace = '<DEFAULT>' then
  begin
   if Database.Connected then
     Result := Database.Tablespace
  end
 else
  Result := FTablespace;
end;

procedure TPSQLQuery.SetByteaAsEscString(const Value: boolean);
begin
  inherited;
  If Active then
   begin
    Close;
    Open;
   end;
end;

procedure TPSQLQuery.SetOIDAsInt(const Value: boolean);
begin
  inherited;
  If Active then
   begin
    Close;
    Open;
   end;
end;

procedure TPSQLTable.SetByteaAsEscString(const Value: boolean);
begin
  inherited;
  If Active then
   begin
    Close;
    Open;
   end;
end;

procedure TPSQLTable.SetOIDAsInt(const Value: boolean);
begin
  inherited;
  If Active then
   begin
    Close;
    Open;
   end;
end;


Initialization

  if not IsLibrary then
   begin
    SaveInitProc := InitProc;
    InitProc := @InitDBTables;
   end;
  DBList := TList.Create;
  InitializeCriticalSection(CSNativeToAnsi);
  InitializeCriticalSection(CSAnsiToNative);

finalization

  DeleteCriticalSection(CSAnsiToNative);
  DeleteCriticalSection(CSNativeToAnsi);
  DBList.Free;
  FreeAndNil(BDEInitProcs);
  FreeTimer(TRUE);

  if NeedToUninitialize then  CoUninitialize;

  if not IsLibrary then
   begin
    InitProc := SaveInitProc;//mi:2006-09-18 thnks to Sebastien Hordeaux (#242)
   end;

end.