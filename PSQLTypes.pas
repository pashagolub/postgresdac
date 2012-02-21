{$I pSQLDAC.inc}
unit PSQLTypes;

{SVN revision: $Id$}

{$Z+,T-} //taken from MySQLDAC 
interface

uses {$IFDEF FPC}LCLIntf, dynlibs,{$ENDIF}
     Classes, SysUtils, Math,
     {$IFDEF MSWINDOWS}Windows{$ENDIF}
     {$IFDEF MACOS}Macapi.CoreServices{$ENDIF};

//============================================================================//
//                            Result Error Field Codes                        //
//============================================================================//
const
  PG_DIAG_SEVERITY            =	ord('S');
  PG_DIAG_SQLSTATE            =	ord('C');
  PG_DIAG_MESSAGE_PRIMARY     = ord('M');
  PG_DIAG_MESSAGE_DETAIL      =	ord('D');
  PG_DIAG_MESSAGE_HINT        =	ord('H');
  PG_DIAG_STATEMENT_POSITION  = ord('P');
  PG_DIAG_INTERNAL_POSITION   = ord('p');
  PG_DIAG_INTERNAL_QUERY      =	ord('q');
  PG_DIAG_CONTEXT		          =	ord('W');
  PG_DIAG_SOURCE_FILE	        =	ord('F');
  PG_DIAG_SOURCE_LINE	        =	ord('L');
  PG_DIAG_SOURCE_FUNCTION     = ord('R');

//============================================================================//
//                            Option flags for PQcopyResult                   //
//============================================================================//
const
  PG_COPYRES_ATTRS            = 01;
  PG_COPYRES_TUPLES           = 02;  // Implies PG_COPYRES_ATTRS
  PG_COPYRES_EVENTS           = 04;
  PG_COPYRES_NOTICEHOOKS      = 08;

//============================================================================//
//                            Error Categories                                //
//============================================================================//
const
  ERRBASE_NONE                  = 0;      { No error }
  ERRBASE_NOTFOUND              = $2200;  { Object of interest Not Found }
  ERRBASE_INVALIDREQ            = $2700;  { Invalid Request }
  ERRBASE_SEC                   = $2900;  { Access Violation - Security related }
  ERRBASE_IC                    = $2A00;  { Invalid context }
  ERRBASE_QUERY                 = $2E00;  { Query related }
  ERRBASE_CAPABILITY            = $3000;  { Capability not supported }
  ERRBASE_OTHER                 = $3300;  { Miscellaneous }
//=============================================================================//
//                           Error Codes By Category                           //
//=============================================================================//
  ERRCODE_NONE                  = 0;
  DBIERR_NONE                   = (ERRBASE_NONE + ERRCODE_NONE);
  ERRCODE_BOF                   = 1;      { Beginning of Virtual table }
  ERRCODE_EOF                   = 2;      { End of Virtual table }
  ERRCODE_NOCURRREC             = 5;      { No current record }
  ERRCODE_RECNOTFOUND           = 6;      { Record was not found }
  ERRCODE_ENDOFBLOB             = 7;      { End of Blob reached }
  DBIERR_BOF                    = (ERRBASE_NOTFOUND + ERRCODE_BOF);
  DBIERR_EOF                    = (ERRBASE_NOTFOUND + ERRCODE_EOF);
  DBIERR_NOCURRREC              = (ERRBASE_NOTFOUND + ERRCODE_NOCURRREC);
  DBIERR_RECNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_RECNOTFOUND);
  DBIERR_ENDOFBLOB              = (ERRBASE_NOTFOUND + ERRCODE_ENDOFBLOB);
  ERRCODE_INVALIDPARAM          = 2;      { Generic invalid parameter }
  ERRCODE_INVALIDHNDL           = 6;      { Invalid handle to the function }
  ERRCODE_NOSUCHINDEX           = 13;     { 0x0d Index does not exist }
  ERRCODE_INVALIDBLOBOFFSET     = 14;     { 0x0e Invalid Offset into the Blob }
  ERRCODE_INVALIDRECSTRUCT      = 19;     { 0x13 Invalid record structure }
  ERRCODE_NOSUCHTABLE           = 40;     { 0x28 No such table }
  ERRCODE_NOSUCHFILTER          = 66;     { 0x42 Filter handle is invalid }
  DBIERR_INVALIDPARAM           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPARAM);
  DBIERR_INVALIDHNDL            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDHNDL);
  DBIERR_NOSUCHINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHINDEX);
  DBIERR_INVALIDBLOBOFFSET      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBOFFSET);
  DBIERR_INVALIDRECSTRUCT       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRECSTRUCT);
  DBIERR_NOSUCHTABLE            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHTABLE);
  DBIERR_NOSUCHFILTER           = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILTER);
{ ERRCAT_SECURITY }
{ =============== }
  ERRCODE_NOTSUFFTABLERIGHTS    = 2;      { Not sufficient table  rights for operation }
  DBIERR_NOTSUFFTABLERIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFTABLERIGHTS);
{ ERRCAT_INVALIDCONTEXT }
{ ===================== }
  ERRCODE_NOTABLOB              = 1;      { Field is not a blob }
  ERRCODE_TABLEREADONLY         = 11;     { 0x0b Table is read only }
  ERRCODE_NOASSOCINDEX          = 12;     { 0x0c No index associated with the cursor }
  DBIERR_NOTABLOB               = (ERRBASE_IC + ERRCODE_NOTABLOB);
  DBIERR_TABLEREADONLY          = (ERRBASE_IC + ERRCODE_TABLEREADONLY);
  DBIERR_NOASSOCINDEX           = (ERRBASE_IC + ERRCODE_NOASSOCINDEX);
{ ERRCAT_NETWORK }
{ ERRCAT_QUERY }
{ ============ }
  DBICODE_QRYEMPTY              = 110;    { 0x6e }
  DBIERR_QRYEMPTY               = (ERRBASE_QUERY+ DBICODE_QRYEMPTY);
{ END_OF_QUERY_MESSAGES }

{ ERRCAT_CAPABILITY }
{ ================= }
  ERRCODE_NOTSUPPORTED          = 1;      { Capability not supported }
  DBIERR_NOTSUPPORTED           = (ERRBASE_CAPABILITY + ERRCODE_NOTSUPPORTED);
{ ERRCAT_OTHER }
{ ============ }
  ERRCODE_UPDATEABORT           = 6;      { Update operation aborted }
  DBIERR_UPDATEABORT            = (ERRBASE_OTHER + ERRCODE_UPDATEABORT);

{$IFDEF DELPHI_5}
  type
   EFileNotFoundException = class(Exception);
{$ENDIF DELPHI_5}

{$IFDEF UNDER_DELPHI_6}
   type
    PBoolean      = ^Boolean;
    PWordBool     = ^WordBool;

    TFormatSettings = record
      CurrencyFormat: Byte;
      NegCurrFormat: Byte;
      ThousandSeparator: Char;
      DecimalSeparator: Char;
      CurrencyDecimals: Byte;
      DateSeparator: Char;
      TimeSeparator: Char;
      ListSeparator: Char;
      CurrencyString: string;
      ShortDateFormat: string;
      LongDateFormat: string;
      TimeAMString: string;
      TimePMString: string;
      ShortTimeFormat: string;
      LongTimeFormat: string;
      ShortMonthNames: array[1..12] of string;
      LongMonthNames: array[1..12] of string;
      ShortDayNames: array[1..7] of string;
      LongDayNames: array[1..7] of string;
      TwoDigitYearCenturyWindow: Word;
    end;
{$ENDIF}

const
  NAMEDATALEN      = 64;
  TIMESTAMPTZLEN   = length('2006-02-28 09:08:08.677444+02');
  TIMETZLEN        = length('13:45:35.4880123457+13:40');
  UUIDLEN          = length('{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}');
  INETLEN          = length('7628:0d18:11a3:09d7:1f34:8a2e:07a0:765d/128');
  MACADDRLEN       = length('08:00:2b:01:02:03');
  OIDNAMELEN       = 36;
  INV_WRITE        = $00020000;
  INV_READ         = $00040000;
  DELIMITERS       : string = ' .:;,+-<>/*%^=()[]|&~@#$\`{}!?'#10#13;
  PSQL_PORT        = 5432;
  MINLONGINT       = -MaxLongInt;
  MAX_BLOB_SIZE    = 8192; //Max Blob size for read and write operation
  MAX_CHAR_LEN     = 8192; //Max character length allowed in TField descendants
  MAX_ENCODING_ID  = 42; //Max encoding id for pg_encoding_to_char
  InvalidOid       : cardinal = 0;


const //date/time convertion
  TIMESTAMP_MODE = 0;
  DATE_MODE = 1;
  TIME_MODE = 2;

{$IFDEF FPC}
  const
    HINSTANCE_ERROR = 32;
{$ELSE}
  {$IFNDEF WINDOWS}
    const
      HINSTANCE_ERROR = 32;
  {$ENDIF}
{$ENDIF}

const
  LIBEAY_DLL           : string = 'libeay32.dll';
  SSLEAY_DLL           : string = 'ssleay32.dll';

var
  PSQL_DLL             : string =
                                {$IFDEF MSWINDOWS}'libpq.dll'{$ENDIF}
                                {$IFDEF MACOS}'libpq.dylib'{$ENDIF};

  SQLLibraryHandle     : THandle = HINSTANCE_ERROR;
  OEMConv              : Boolean; //Global OEM->ANSI Variable
  PSQL_FS              : TFormatSettings;

type
  _NAME = string[NAMEDATALEN+1];

  TPSQLPoint = packed record
    X: Double;
    Y: Double;
  end;

  TPSQLCircle = packed record
   R: Double;
   case Integer of
    0: (X, Y: Double);
    1: (Center: TPSQLPoint);
  end;

  TPSQLBox = packed record
    case Integer of
      0: (Right, Top, Left, Bottom: Double);
      1: (TopRight, BottomLeft: TPSQLPoint);
  end;

  TPSQLLSeg = packed record
    case Integer of
      0: (X1, Y1, X2, Y2: Double);
      1: (P1, P2: TPSQLPoint);
  end;

  TPSQLDACAbout = class
  end;

const

//--generate_series analogue for < 8.0 versions
  sqlGenerateSeries :string =
        '(select i*10+j as n'+
        ' from (select 0 union all select 1 union all select 2 union all'+
        '       select 3 union all select 4 union all select 5 union all'+
        '       select 6 union all select 7 union all select 8 union all'+
        '       select 9) s1(i),'+
        '      (select 0 union all select 1 union all select 2 union all'+
        '       select 3 union all select 4 union all select 5 union all'+
        '       select 6 union all select 7 union all select 8 union all'+
        '       select 9) s2(j)'+
        ' where (i*10+j >= %d) AND (i*10+j <= %d))';
//--generate_series analogue for < 8.0 versions
  
//////////////////////////////////////////////////////////////////
//            FIELD TYPES                                       //
//////////////////////////////////////////////////////////////////
const
    FIELD_TYPE_BOOL               = 16;
    FIELD_TYPE_BYTEA              = 17;
    FIELD_TYPE_CHAR               = 18;
    FIELD_TYPE_NAME               = 19;
    FIELD_TYPE_INT8               = 20;
    FIELD_TYPE_INT2               = 21;
    FIELD_TYPE_INT2VECTOR         = 22;
    FIELD_TYPE_INT4               = 23;
    FIELD_TYPE_REGPROC            = 24;
    FIELD_TYPE_TEXT               = 25;
    FIELD_TYPE_OID                = 26;
    FIELD_TYPE_TID                = 27;
    FIELD_TYPE_XID                = 28;
    FIELD_TYPE_CID                = 29;
    FIELD_TYPE_OIDVECTOR          = 30;
    FIELD_TYPE_SET                = 32;
    FIELD_TYPE_SMGR               = 210;
    FIELD_TYPE_POINT              = 600;
    FIELD_TYPE_LSEG               = 601;
    FIELD_TYPE_PATH               = 602;
    FIELD_TYPE_BOX                = 603;
    FIELD_TYPE_POLYGON            = 604;
    FIELD_TYPE_LINE               = 628;
    FIELD_TYPE_A_LINE             = 629;
    FIELD_TYPE_CIDR               = 650;
    FIELD_TYPE_A_CIDR             = 651;
    FIELD_TYPE_FLOAT4             = 700;
    FIELD_TYPE_FLOAT8             = 701;
    FIELD_TYPE_ABSTIME            = 702;
    FIELD_TYPE_RELTIME            = 703;
    FIELD_TYPE_TINTERVAL          = 704;
    FIELD_TYPE_UNKNOWN            = 705;
    FIELD_TYPE_CIRCLE             = 718;
    FIELD_TYPE_A_CIRCLE           = 719;
    FIELD_TYPE_MONEY              = 790;
    FIELD_TYPE_A_MONEY            = 791;
    FIELD_TYPE_MACADDR            = 829;
    FIELD_TYPE_INET               = 869;
    FIELD_TYPE_A_BOOL             = 1000;
    FIELD_TYPE_A_BYTEA            = 1001;
    FIELD_TYPE_A_CHAR             = 1002;
    FIELD_TYPE_A_NAME             = 1003;
    FIELD_TYPE_A_INT2             = 1005;
    FIELD_TYPE_A_INT28            = 1006;
    FIELD_TYPE_A_INT4             = 1007;
    FIELD_TYPE_A_REGPROC          = 1008;
    FIELD_TYPE_A_TEXT             = 1009;
    FIELD_TYPE_A_TID              = 1010;
    FIELD_TYPE_A_XID              = 1011;
    FIELD_TYPE_A_CID              = 1012;
    FIELD_TYPE_A_OID8             = 1013;
    FIELD_TYPE_A_BPCHAR           = 1014;
    FIELD_TYPE_A_VARCHAR          = 1015;
    FIELD_TYPE_A_POINT            = 1017;
    FIELD_TYPE_A_LSEG             = 1018;
    FIELD_TYPE_A_PATH             = 1019;
    FIELD_TYPE_A_BOX              = 1020;
    FIELD_TYPE_A_FLOAT4           = 1021;
    FIELD_TYPE_A_FLOAT8           = 1022;
    FIELD_TYPE_A_ABSTIME          = 1023;
    FIELD_TYPE_A_RELTIME          = 1024;
    FIELD_TYPE_A_TINTERVAL        = 1025;
    FIELD_TYPE_A_FILENAME         = 1026;
    FIELD_TYPE_A_POLYGON          = 1027;
    FIELD_TYPE_A_OID              = 1028;
    FIELD_TYPE_ACLITEM            = 1033;
    FIELD_TYPE_A_ACLITEM          = 1034;
    FIELD_TYPE_A_MACADDR          = 1040;
    FIELD_TYPE_A_INET             = 1041;
    FIELD_TYPE_BPCHAR             = 1042;
    FIELD_TYPE_VARCHAR            = 1043;
    FIELD_TYPE_DATE               = 1082;
    FIELD_TYPE_TIME               = 1083;
    FIELD_TYPE_A_TIMESTAMP        = 1115;
    FIELD_TYPE_TIMESTAMP          = 1114;
    FIELD_TYPE_A_DATE             = 1182;
    FIELD_TYPE_A_TIME             = 1183;
    FIELD_TYPE_A_TIMESTAMPTZ      = 1185;
    FIELD_TYPE_TIMESTAMPTZ        = 1184;
    FIELD_TYPE_A_DATETIME         = 1185;
    FIELD_TYPE_INTERVAL           = 1186;
    FIELD_TYPE_A_INTERVAL         = 1187;
    FIELD_TYPE_A_TIMETZ           = 1270;
    FIELD_TYPE_TIMETZ             = 1266;
    FIELD_TYPE_A_BIT              = 1561;
    FIELD_TYPE_BIT                = 1560;
    FIELD_TYPE_A_VARBIT           = 1563;
    FIELD_TYPE_VARBIT             = 1562;
    FIELD_TYPE_A_NUMERIC          = 1231;
    FIELD_TYPE_NUMERIC            = 1700;
    FIELD_TYPE_UUID               = 2950; //09.10.2007
    FIELD_TYPE_XML                = 142;  //09.10.2007
    FIELD_TYPE_TSVECTOR		  = 3614; //15.11.2009
    FIELD_TYPE_GTSVECTOR          = 3642; //15.11.2009
    FIELD_TYPE_TSQUERY            = 3615; //15.11.2009
    FIELD_TYPE_REGCONFIG          = 3734; //15.11.2009
    FIELD_TYPE_REGDICTIONARY      = 3769; //15.11.2009


    PSEUDO_TYPE_VOID              = 2278;
    PSEUDO_TYPE_TRIGGER           = 2279;
    PSEUDO_TYPE_LANGHANDLER       = 2280;
    PSEUDO_TYPE_RECORD            = 2249;
    PSEUDO_TYPE_CSTRING           = 2275;
    PSEUDO_TYPE_A_CSTRING         = 1263; //10.01.2008
    PSEUDO_TYPE_INTERNAL          = 2281;
    PSEUDO_TYPE_ANYENUM           = 3500; //09.10.2007
    PSEUDO_ANY_ARRAY              = 2277;
    PSEUDO_ANY_NONARRAY           = 2276;
    PSEUDO_ANY_ELEMENT            = 2283;
    PSEUDO_OPAQUE                 = 2282;
    PSEUDO_ANY_ENUM               = 3500;
    
    MAX_BUILTIN_TYPE_OID = FIELD_TYPE_REGDICTIONARY; //15.11.2009 need to be changed if new built-in type appears


    MAXARRFLDTYPES = 38;

    FldArrayType: array[0..MAXARRFLDTYPES-1] of Integer = (
    FIELD_TYPE_A_LINE,    FIELD_TYPE_A_CIDR,   FIELD_TYPE_A_CIRCLE, FIELD_TYPE_A_MONEY,    FIELD_TYPE_A_BOOL,    FIELD_TYPE_A_BYTEA,
    FIELD_TYPE_A_CHAR,    FIELD_TYPE_A_NAME,   FIELD_TYPE_A_INT2,   FIELD_TYPE_A_INT28,    FIELD_TYPE_A_INT4,    FIELD_TYPE_A_REGPROC,
    FIELD_TYPE_A_TEXT,    FIELD_TYPE_A_TID,    FIELD_TYPE_A_XID,    FIELD_TYPE_A_CID,      FIELD_TYPE_A_OID8,    FIELD_TYPE_A_BPCHAR,
    FIELD_TYPE_A_VARCHAR, FIELD_TYPE_A_POINT,  FIELD_TYPE_A_LSEG,   FIELD_TYPE_A_PATH,     FIELD_TYPE_A_BOX,     FIELD_TYPE_A_FLOAT4,
    FIELD_TYPE_A_FLOAT8,  FIELD_TYPE_A_ABSTIME,FIELD_TYPE_A_RELTIME,FIELD_TYPE_A_TINTERVAL,FIELD_TYPE_A_FILENAME,FIELD_TYPE_A_POLYGON,
    FIELD_TYPE_A_OID,     FIELD_TYPE_A_ACLITEM,FIELD_TYPE_A_MACADDR,FIELD_TYPE_A_INET,     FIELD_TYPE_A_DATE,    FIELD_TYPE_A_TIME,
    FIELD_TYPE_A_DATETIME,FIELD_TYPE_A_INTERVAL);

//////////////////////////////////////////////////////////////////
//                   Plain API Types definition                 //
//////////////////////////////////////////////////////////////////
type

  //used to determine what native type used to store BLOBs
  TNativeBLOBType = (nbtNotBLOB, nbtBytea, nbtOID);

  //used to determine what native presentation used for Bytea
  TNativeByteaFormat = (nbfEscape, nbfHex);


  MemPtr       = ^MemArray;
  MemArray     = Array[0..$FFFE] of Byte;

  Oid = Cardinal;
  POid = ^Oid;
  TDynOidArray = array of Oid;

  ConnStatusType = (
  CONNECTION_OK,
  CONNECTION_BAD,
  //Non-blocking mode only below here
	CONNECTION_STARTED,			// Waiting for connection to be made
	CONNECTION_MADE,			// Connection OK; waiting to send
	CONNECTION_AWAITING_RESPONSE,		// Waiting for a response from the postmaster
	CONNECTION_AUTH_OK,			// Received authentication; waiting for backend startup
	CONNECTION_SETENV,			// Negotiating environment
	CONNECTION_SSL_STARTUP,		// Negotiating SSL
	CONNECTION_NEEDED			// Internal state: connect() needed
  );

  PollingStatusType = (
	PGRES_POLLING_FAILED,
	PGRES_POLLING_READING,		// These two indicate that one may
	PGRES_POLLING_WRITING,		// use select before polling again
	PGRES_POLLING_OK,
	PGRES_POLLING_ACTIVE		// unused; keep for awhile for backwards compatibility
	);

  ExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,		// a query command that doesn't return anything was executed properly by the backend
    PGRES_TUPLES_OK,		// a query command that returns tuples was executed properly by the backend, PGresult contains the result tuples
    PGRES_COPY_OUT,		// Copy Out data transfer in progress
    PGRES_COPY_IN,		// Copy In data transfer in progress
    PGRES_BAD_RESPONSE,		// an unexpected response was recv'd from  the backend
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR);

// String descriptions of the ExecStatusTypes
  pgresStatus = array[$00..$ff] of PAnsiChar;

  TErrorVerbosity = (evTERSE, evDEFAULT, evVERBOSE);

  TTransactionStatusType = (
	trstIDLE,			    // connection idle
	trstACTIVE,				// command in progress
	trstINTRANS,			// idle, within transaction block
	trstINERROR,			// idle, within failed transaction
	trstUNKNOWN);     // cannot determine status

  TPingStatus = (
   pstOK,         //The server is running and appears to be accepting connections
   pstReject,     //The server is running but is in a state that disallows connections (startup, shutdown, or crash recovery)
   pstNoResponse, //The server could not be contacted
   pstNoAttempt); //No attempt was made to contact the server due to incorrect parameters

////////////////////////////////////////////////////////////////////
//   PGconn encapsulates a connection to the backend.             //
//   The contents of this struct are not supposed to be known to  //
//   applications.                                                //
////////////////////////////////////////////////////////////////////
  PGconn = Pointer;
  PPGconn = Pointer;

// PGresult encapsulates the result of a query (or more precisely, of a single
//  SQL command --- a query string given to PQsendQuery can contain multiple
//  commands and thus return multiple PGresult objects).
//  The contents of this struct are not supposed to be known to applications.
  PGresult = Pointer;
  PPGresult = ^PGresult;

// PGnotify represents the occurrence of a NOTIFY message.
//  Ideally this would be an opaque typedef, but it's so simple that it's
//  unlikely to change.
//  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
//  whereas in earlier versions it was always your own backend's PID.
 { PGnotify = packed record
    relname: array [0..NAMEDATALEN-1] of Char; // name of relation containing data
    be_pid:  Integer;			       // process id of backend
  end;}
  PPGnotify = ^PGnotify;
  PGnotify = packed record
    relname: PAnsiChar; // name of relation containing data
    be_pid:  Integer;	   // process id of backend
    extra:   PAnsiChar;        // extra notification
    next:    PPGnotify;        // application should never use this
  end;


// PQnoticeProcessor is the function type for the notice-message callback.
  PQnoticeProcessor = procedure(arg: Pointer; message: PAnsiChar);cdecl;

// Print options for PQprint()
//  We can't use the conventional "bool", because we are designed to be
//  included in a user's program, and user may already have that type
//  defined.  Pqbool, on the other hand, is unlikely to be used.

  PPAnsiChar = ^PAnsiChar;

  PQprintOpt = packed record
    header:    Byte;	   { print output field headings and row count }
    align:     Byte;	   { fill align the fields }
    standard:  Byte;	   { old brain dead format }
    html3:     Byte;	   { output html tables }
    expanded:  Byte;	   { expand tables }
    pager:     Byte;	   { use pager for output if needed }
    fieldSep:  PAnsiChar;	   { field separator }
    tableOpt:  PAnsiChar;      { insert to HTML <table ...> }
    caption:   PAnsiChar;	   { HTML <caption> }
    fieldName: PPAnsiChar; 	   { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

//////////////////////////////////////////////////////////////////////////////////
//  Structure for the conninfo parameter definitions returned by PQconndefaults //
//////////////////////////////////////////////////////////////////////////////////
  PQconninfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    dispchar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

//////////////////////////////////////////////////////////////////
//              Plain API Function types definition             //
//////////////////////////////////////////////////////////////////
  TPQconnectdb     = function(ConnInfo: PAnsiChar): PPGconn; cdecl; //blocking manner

  TPQconnectStart  = function(ConnInfo: PAnsiChar): PPGconn; cdecl; //non-blocking manner

  TPQconnectdbParams = function(Keywords: PPAnsiChar; Values: PPAnsichar; ExpandDBName: integer): PPGconn; cdecl; //blocking manner

  TPQping          = function(ConnInfo: PAnsiChar): TPingStatus; cdecl;

  TPQpingParams    = function(Keywords: PPAnsiChar; Values: PPAnsichar; ExpandDBName: integer): TPingStatus;

  TPQconnectPoll   = function (Handle : PPGconn): PollingStatusType; cdecl;

  TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): PPGconn; cdecl;

  TPQconndefaults  = function: PPQconninfoOption; cdecl;

  TPQfinish        = procedure(Handle: PPGconn); cdecl;

  TPQreset         = procedure(Handle: PPGconn); cdecl;

  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;

  TPQdb            = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQuser          = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQpass          = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQhost          = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQport          = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQtty           = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQoptions       = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQstatus        = function(Handle: PPGconn): ConnStatusType; cdecl;

  TPQerrorMessage  = function(Handle: PPGconn): PAnsiChar; cdecl;

  TPQsocket        = function(Handle: PPGconn): Integer; cdecl;

  TPQbackendPID    = function(Handle: PPGconn): Integer; cdecl;

  TPQparameterStatus = function(Handle: PPGconn;
                                paramName: PAnsiChar): PAnsiChar; cdecl;

  TPQserverVersion = function(Handle: PPGconn): Integer; cdecl;

  TPQtransactionStatus  = function(Handle: PPGconn): TTransactionStatusType; cdecl;

  TPQgetssl = function(Handle: PPGconn): pointer; cdecl; //point to SSL structure, see OpenSSL manual for details

  TPQtrace         = procedure(Handle: PPGconn;
                               DebugPort: Pointer); cdecl;

  TPQuntrace       = procedure(Handle: PPGconn); cdecl;

  TPQsetNoticeProcessor = function(Handle: PPGconn;
                                   Proc: PQnoticeProcessor;
                                   Arg: Pointer): Pointer; cdecl;

  TPQprepare       = function(Handle: PPGconn;
                              StmtName: PAnsiChar;
                              Query: PAnsiChar;
                              nParams: integer;
                              paramTypes: POid): PPGresult; cdecl;

  TPQexecPrepared  = function(Handle: PPGconn;
                              StmtName: PAnsiChar;
                              nParams: integer;
                              paramValues: PPAnsiChar;
                              paramLengths: PInteger;
                              paramFormats: PInteger;
                              resultFormat: integer): PPGresult; cdecl;

  TPQexec          = function(Handle: PPGconn;
                              Query: PAnsiChar): PPGresult; cdecl;

  TPQexecParams    = function(Handle: PPGconn;
                              Query: PAnsiChar;
                              nParams: integer;
                              paramTypes: POid;
                              paramValues: PPAnsiChar;
                              paramLengths: PInteger;
                              paramFormats: PInteger;
                              resultFormat: integer): PPGresult; cdecl;

  TPQresultErrorField = function(Result: PPGresult;
                                 fieldcode:integer): PAnsiChar; cdecl;

  TPQnotifies      = function(Handle: PPGconn): PPGnotify; cdecl;

  TPQsendQuery     = function(Handle: PPGconn;
                              Query: PAnsiChar): Integer; cdecl;

  TPQgetResult     = function(Handle: PPGconn): PPGresult; cdecl;

  TPQisBusy        = function(Handle: PPGconn): Integer; cdecl;

  TPQconsumeInput  = function(Handle: PPGconn): Integer; cdecl;

  TPQgetline       = function(Handle: PPGconn;
                              Str: PAnsiChar;
                              length: Integer): Integer; cdecl;

  TPQputline       = function(Handle: PPGconn;
                              Str: PAnsiChar): Integer; cdecl;

  TPQgetlineAsync  = function(Handle: PPGconn;
                              Buffer: PAnsiChar;
                              BufSize: Integer): Integer; cdecl;

  TPQputnbytes     = function(Handle: PPGconn;
                              Buffer: PAnsiChar;
                              NBytes: Integer): Integer; cdecl;

  TPQendcopy       = function(Handle: PPGconn): Integer; cdecl;

  TPQgetCopyData   = function(Handle: PPGConn;
                              Buffer: PPAnsiChar;
                              Async: integer = 0): Integer; cdecl;

  TPQputCopyData   = function(Handle: PPGConn;
                              Buffer: PAnsiChar;
                              Len: integer): Integer; cdecl;

  TPQputCopyEnd    = function(Handle: PPGConn;
                              Buffer: PAnsiChar = nil): Integer; cdecl;

  TPQresultStatus  = function(Result: PPGresult): ExecStatusType; cdecl;

  TPQresultErrorMessage = function(Result: PPGresult): PAnsiChar; cdecl;

  TPQntuples       = function(Result: PPGresult): Integer; cdecl;

  TPQnfields       = function(Result: PPGresult): Integer; cdecl;

  TPQbinaryTuples  = function(Result: PPGresult): Integer; cdecl;

  TPQfname         = function(Result: PPGresult;
                              field_num: Integer): PAnsiChar; cdecl;

  TPQfnumber       = function(Result: PPGresult;
                              field_name: PAnsiChar): Integer; cdecl;

  TPQftype         = function(Result: PPGresult;
                              field_num: Integer): Oid; cdecl;

  TPQftable        = function(Result: PPGresult;
                              field_num: Integer): Oid; cdecl;

  TPQftablecol     = function(Result: PPGresult;
                              field_num: Integer): Integer; cdecl;

  TPQfsize         = function(Result: PPGresult;
                              field_num: Integer): Integer; cdecl;

  TPQfmod          = function(Result: PPGresult;
                              field_num: Integer): Integer; cdecl;

  TPQcmdStatus     = function(Result: PPGresult): PAnsiChar; cdecl;

  TPQoidValue      = function(Result: PPGresult): Oid; cdecl;

  TPQoidStatus     = function(Result: PPGresult): PAnsiChar; cdecl;

  TPQcmdTuples     = function(Result: PPGresult): PAnsiChar; cdecl;

  TPQgetvalue      = function(Result: PPGresult;
                              tup_num: Integer;
                              field_num: Integer): PAnsiChar; cdecl;

  TPQsetvalue      = function(Result: PPGresult;
                              tup_num: Integer;
                              field_num: Integer;
                              value: PAnsiChar;
                              len: integer): integer; cdecl;

  TPQcopyResult    = function(Result: PPGresult;
                              flags: integer): PPGresult; cdecl;

  TPQgetlength     = function(Result: PPGresult;
                              tup_num: Integer;
                              field_num: Integer): Integer; cdecl;

  TPQgetisnull     = function(Result: PPGresult;
                              tup_num: Integer;
                              field_num: Integer): Integer; cdecl;

  TPQclear         = procedure(Result: PPGresult); cdecl;

  TPQmakeEmptyPGresult  = function(Handle: PPGconn;
                                   status: ExecStatusType): PPGresult; cdecl;

  TPQEscapeByteaConn   = function(Handle: PPGconn;
                                  from: PAnsiChar;
                                  from_length: integer;
                                  var to_length: integer): PAnsiChar; cdecl;

  TPQUnEscapeBytea = function(from: PAnsiChar;
                              var to_length: integer): PAnsiChar; cdecl;

  TPQEscapeStringConn = function(Handle: PPGconn;
                                 to_str: PAnsiChar;
                                 const from_str: PAnsiChar;
                                 from_size: cardinal;
                                 var Error: integer): cardinal; cdecl;

  TPQFreeMem       = procedure(Ptr: Pointer); cdecl;

  TPQsetClientEncoding = function(Handle: PPGconn;
                                  encoding: PAnsiChar): integer; cdecl;

  TPQsetErrorVerbosity = function(Handle: PPGconn;
                                  verbosity: TErrorVerbosity): TErrorVerbosity; cdecl;

  TPQclientEncoding = function(Handle: PPGconn): integer; cdecl;

  Tpg_encoding_to_char = function(encoding_id: integer): PAnsiChar; cdecl;

  Tlo_open         = function(Handle: PPGconn;
                              lobjId: Oid;
                              mode: Integer): Integer; cdecl;

  Tlo_close        = function(Handle: PPGconn;
                              fd: Integer): Integer; cdecl;

  Tlo_read         = function(Handle: PPGconn;
                              fd: Integer;
                              buf: PAnsiChar;
                              len: Integer): Integer; cdecl;

  Tlo_write        = function(Handle: PPGconn;
                              fd: Integer;
                              buf: PAnsiChar;
                              len: Integer): Integer; cdecl;

  Tlo_lseek        = function(Handle: PPGconn;
                              fd: Integer;
                              offset: Integer;
                              whence: Integer): Integer; cdecl;

  Tlo_creat        = function(Handle: PPGconn;
                              mode: Integer): Oid; cdecl;

  Tlo_tell         = function(Handle: PPGconn;
                              fd: Integer): Integer; cdecl;

  Tlo_unlink       = function(Handle: PPGconn;
                              lobjId: Oid): Integer; cdecl;

  Tlo_import       = function(Handle: PPGconn;
                              filename: PAnsiChar): Oid; cdecl;

  Tlo_export       = function(Handle: PPGconn;
                              lobjId: Oid;
                              filename: PAnsiChar): Integer; cdecl;


//////////////////////////////////////////////////////////////////
//            Plain API Function variables definition           //
//////////////////////////////////////////////////////////////////

var
  PQconnectdb:     TPQconnectdb;
  PQconnectdbParams: TPQconnectdbParams;
  PQconnectStart:  TPQconnectStart;
  PQping:          TPQping;
  PQpingParams:    TPQpingParams;
  PQconnectPoll:   TPQconnectPoll;
  PQsetdbLogin:    TPQsetdbLogin;
  PQconndefaults:  TPQconndefaults;
  PQfinish:        TPQfinish;
  PQreset:         TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb:            TPQdb;
  PQuser:          TPQuser;
  PQpass:          TPQpass;
  PQhost:          TPQhost;
  PQport:          TPQport;
  PQtty:           TPQtty;
  PQoptions:       TPQoptions;
  PQstatus:        TPQstatus;
  PQerrorMessage:  TPQerrorMessage;
  PQsocket:        TPQsocket;
  PQparameterStatus: TPQparameterStatus;
  PQserverVersion: TPQserverVersion;
  PQbackendPID:    TPQbackendPID;
  PQtransactionStatus: TPQtransactionStatus;
  PQgetssl:        TPQgetssl;
  PQtrace:         TPQtrace;
  PQuntrace:       TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;
  PQprepare:       TPQprepare;           
  PQexecPrepared:  TPQexecPrepared;
  PQexec:          TPQexec;
  PQexecParams:    TPQexecParams; 
  PQresultErrorField:TPQresultErrorField;
  PQnotifies:      TPQnotifies;
  PQsendQuery:     TPQsendQuery;
  PQgetResult:     TPQgetResult;
  PQisBusy:        TPQisBusy;
  PQconsumeInput:  TPQconsumeInput;
  PQgetline:       TPQgetline;
  PQputline:       TPQputline;
  PQgetlineAsync:  TPQgetlineAsync;
  PQputnbytes:     TPQputnbytes;
  PQendcopy:       TPQendcopy;
  PQgetCopyData:   TPQgetCopyData;
  PQputCopyData:   TPQputCopyData;
  PQputCopyEnd:    TPQputCopyEnd;
  PQresultStatus:  TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQntuples:       TPQntuples;
  PQnfields:       TPQnfields;
  PQbinaryTuples:  TPQbinaryTuples;
  PQfname:         TPQfname;
  PQfnumber:       TPQfnumber;
  PQftype:         TPQftype;
  PQftable:        TPQftable;
  PQftablecol:     TPQftablecol;
  PQfsize:         TPQfsize;
  PQfmod:          TPQfmod;
  PQcmdStatus:     TPQcmdStatus;
  PQoidValue:      TPQoidValue;
  PQoidStatus:     TPQoidStatus;
  PQcmdTuples:     TPQcmdTuples;
  PQgetvalue:      TPQgetvalue;
  PQsetvalue:      TPQsetvalue;
  PQcopyResult:    TPQcopyResult;
  PQgetlength:     TPQgetlength;
  PQgetisnull:     TPQgetisnull;
  PQclear:         TPQclear;
  PQmakeEmptyPGresult:  TPQmakeEmptyPGresult;
  PQEscapeByteaConn:   TPQEscapeByteaConn;
  PQUnEscapeBytea: TPQUnEscapeBytea;
  PQEscapeStringConn: TPQEscapeStringConn;
  PQFreeMem:       TPQFreeMem;
  PQsetClientEncoding: TPQsetClientEncoding;
  PQsetErrorVerbosity: TPQsetErrorVerbosity;
  PQclientEncoding: TPQclientEncoding;
  pg_encoding_to_char: Tpg_encoding_to_char;
  lo_open:         Tlo_open;
  lo_close:        Tlo_close;
  lo_read:         Tlo_read;
  lo_write:        Tlo_write;
  lo_lseek:        Tlo_lseek;
  lo_creat:        Tlo_creat;
  lo_tell:         Tlo_tell;
  lo_unlink:       Tlo_unlink;
  lo_import:       Tlo_import;
  lo_export:       Tlo_export;




/////////////////////////////////////////////////////////////////////////////////
//   BDE TYPE                                                                  //
/////////////////////////////////////////////////////////////////////////////////

resourcestring
  SAutoSessionExclusive = 'Cannot enable AutoSessionName property with more than one session on a form or data-module';
  SAutoSessionExists = 'Cannot add a session to the form or data-module while session ''%s'' has AutoSessionName enabled';
  SAutoSessionActive = 'Cannot modify SessionName while AutoSessionName is enabled';
  SDuplicateDatabaseName = 'Duplicate database name ''%s''';
  SDuplicateSessionName = 'Duplicate session name ''%s''';
  SInvalidSessionName = 'Invalid session name %s';
  SDatabaseNameMissing = 'Database name missing';
  SSessionNameMissing = 'Session name missing';
  SDatabaseOpen = 'Cannot perform this operation on an open database';
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseHandleSet = 'Database handle owned by a different session';
  SSessionActive = 'Cannot perform this operation on an active session';
  SHandleError = 'Error creating cursor handle';
  SInvalidFloatField = 'Cannot convert field ''%s'' to a floating point value';
  SInvalidIntegerField = 'Cannot convert field ''%s'' to an integer value';
  STableMismatch = 'Source and destination tables are incompatible';
  SFieldAssignError = 'Fields ''%s'' and ''%s'' are not assignment compatible';
  SNoReferenceTableName = 'ReferenceTableName not specified for field ''%s''';
  SCompositeIndexError = 'Cannot use array of Field values with Expression Indices';
  SInvalidBatchMove = 'Invalid batch move parameters';
  SEmptySQLStatement = 'No SQL statement available';
  SNoParameterValue = 'No value for parameter ''%s''';
  SNoParameterType = 'No parameter type for parameter ''%s''';
  SLoginError = 'Cannot connect to database ''%s''';
  SInitError = 'An error occurred while attempting to initialize the Borland Database Engine (error $%.4x)';
  SDatabaseEditor = 'Da&tabase Editor...';
  SExplore = 'E&xplore';
  SLinkDetail = '''%s'' cannot be opened';
  SLinkMasterSource = 'The MasterSource property of ''%s'' must be linked to a DataSource';
  SLinkMaster = 'Unable to open the MasterSource Table';
  SGQEVerb = 'S&QL Builder...';
  SBindVerb = 'Define &Parameters...';
  SIDAPILangID = '0009';
  SDisconnectDatabase = 'Database is currently connected. Disconnect and continue?';
  SBDEError = 'BDE error $%.4x';
  SLookupSourceError = 'Unable to use duplicate DataSource and LookupSource';
  SLookupTableError = 'LookupSource must be connected to TTable component';
  SLookupIndexError = '%s must be the lookup table''s active index';
  SParameterTypes = ';Input;Output;Input/Output;Result';
  SInvalidParamFieldType = 'Must have a valid field type selected';
  STruncationError = 'Parameter ''%s'' truncated on output';
  SDataTypes = ';String;SmallInt;Integer;Word;Boolean;Float;Currency;BCD;Date;Time;DateTime;;;;Blob;Memo;Graphic;;;;;Cursor;';
  SResultName = 'Result';
  SDBCaption = '%s%s%s Database';
  SParamEditor = '%s%s%s Parameters';
  SIndexFilesEditor = '%s%s%s Index Files';
  SNoIndexFiles = '(None)';
  SIndexDoesNotExist = 'Index does not exist. Index: %s';
  SNoTableName = 'Missing TableName property';
  SNoDataSetField = 'Missing DataSetField property';
  SBatchExecute = 'E&xecute';
  SNoCachedUpdates = 'Not in cached update mode';
  SInvalidAliasName = 'Invalid alias name %s';
  SNoFieldAccess = 'Cannot access field ''%s'' in a filter';
  SUpdateSQLEditor = '&UpdateSQL Editor...';
  SNoDataSet = 'No dataset association';
  SUntitled = 'Untitled Application';
  SUpdateWrongDB = 'Cannot update, %s is not owned by %s';
  SUpdateFailed = 'Update failed';
  SSQLGenSelect = 'Must select at least one key field and one update field';
  SSQLNotGenerated = 'Update SQL statements not generated, exit anyway?';
  SSQLDataSetOpen = 'Unable to determine field names for %s';
  SLocalTransDirty = 'The transaction isolation level must be dirty read for local databases';
  SMissingDataSet = 'Missing DataSet property';
  SNoProvider = 'No provider available';
  SNotAQuery = 'Dataset is not a query';

//-----------------------------------------------------------------------//
//     DBI types                                                         //
//-----------------------------------------------------------------------//

const
  DBIMAXNAMELEN      = 63;{31;}         { Name limit (table, field etc) }
  DBIMAXSPNAMELEN    = 64;              { Max stored procedure name length }
  DBIMAXFLDSINKEY    = 16;              { Max fields in a key }
  DBIMAXKEYEXPLEN    = 220;             { Max Key expression length }
  DBIMAXEXTLEN       = 3;               { Max file extension len, not incl. dot (excluding zero termination) }
  DBIMAXTBLNAMELEN   = 260;             { Max table name length }
  DBIMAXPATHLEN      = 260;             { Max path+file name len (excluding zero termination) }
  DBIMAXMSGLEN       = 127;             { Max message len }
  DBIMAXVCHKLEN      = 255;             { Max val check len }
  DBIMAXPICTLEN      = 175;             { Max picture len }
  DBIMAXFLDSINSEC    = 256;             { Max fields in security spec }

Type
//============================================================================//
//                             G e n e r a l                                  //
//============================================================================//
  DBIDATE            = Longint;
  TIME               = Longint;
  DBIResult          = Word;         { Function result }
  TypedEnum          = Integer;

  _hDBIObj           = record end;      { Dummy structure to create "typed" handles }
  hDBIObj            = ^_hDBIObj;       { Generic object handle }
  hDBIDb             = ^_hDBIObj;       { Database handle }
  hDBIStmt           = ^_hDBIObj;       { Statement handle ("new query") }
  hDBICur            = ^_hDBIObj;       { Cursor handle }
  hDBIXact           = ^_hDBIObj;       { Transaction handle }
  hDBIFilter         = ^_hDBIObj;       { Filter handle }


{ Handle Pointers }
  phDBIObj           = ^hDBIObj;        { Pointer to Generic object handle }
  phDBIDb            = ^hDBIDb;         { Pointer to Database handle }
  phDBICur           = ^hDBICur;        { Pointer to Cursor handle }


{ typedefs for buffers of various common sizes: }
  DBIPATH            = packed array [0..DBIMAXPATHLEN] of AnsiChar; { holds a DOS path }
  DBINAME            = packed array [0..DBIMAXNAMELEN] of Char; { holds a name }
  DBIEXT             = packed array [0..DBIMAXEXTLEN] of AnsiChar; { holds an extension EXT }
  DBITBLNAME         = packed array [0..DBIMAXTBLNAMELEN] of AnsiChar; { holds a table name }
  DBISPNAME          = packed array [0..DBIMAXSPNAMELEN] of AnsiChar; { holds a stored procedure name }
  DBIKEY             = packed array [0..DBIMAXFLDSINKEY-1] of Word; { holds list of fields in a key }
  DBIKEYEXP          = packed array [0..DBIMAXKEYEXPLEN] of AnsiChar; { holds a key expression }
  DBIVCHK            = packed array [0..DBIMAXVCHKLEN] of Byte; { holds a validity check }
  DBIPICT            = packed array [0..DBIMAXPICTLEN] of AnsiChar; { holds a picture (Pdox) }
  DBIMSG             = packed array [0..DBIMAXMSGLEN] of AnsiChar; { holds an error message }


//============================================================================//
//                         Basic Query Types                                  //
//============================================================================//
  DBIQryLang = (
    qrylangUNKNOWN,                     { UNKNOWN (Error) }
    qrylangQBE,                         { QBE }
    qrylangSQL                          { SQL }
  );

{============================================================================}
{                         Statement parameter information                    }
{============================================================================}

type
  eINDValues = TypedEnum;
const
    indTRUNC     = -2;                   { Value has been truncated }
    indNULL      = -1;                   { Value is NULL }
    indVALUE     = 1;

const
  INDLEN             = sizeof(Word);

type
  STMTParamType = (
    paramUNKNOWN,                       { UNKNOWN (Error) }
    paramIN,                            { Input parameter }
    paramOUT,                           { Output parameter }
    paramINOUT,                         { Input/Output parameter }
    paramRET                            { Procedure (or function) return }
  );

  STMTParamFormat = (
    paramfmtUNKNOWN,                    { UNKNOWN (Error) }
    paramfmtNUMBER,                     { Numbered parameter markers of the form ? or :n }
    paramfmtNAME                        { Named parameters markers of the form :name }
  );



//============================================================================//
//                   General properties  DbiGetProp/DbiSetProp                //
//============================================================================//
{ Cursor properties }
{ General           }

const
  curMAXPROPS        = $00050000;       { ro UINT16   , Number of defined properties }
  curTABLELEVEL      = $00050003;       { ro UINT16   , Table level 1..n }
  curXLTMODE         = $00050005;       { rw XLTMode  , Translate mode }
  curMAXFIELDID      = $0005000F;       { ro UINT16, Max # of field desc }
  curFIELDFULLNAME   = $00050010;       { ro pObjAttrDesc, Object attribute name }
  curFIELDTYPENAME   = $00050011;       { ro pObjTypeDesc, Object Type name }
  curMAKECRACK       = $00050014;       { Create a crack at the current cursor position }
  curFIELDISAUTOINCR = $00050015;       { wo BOOL, Auto increment field }
  curFIELDISDEFAULT  = $00050016;       { wo BOOL, Default field }
  curAUTOREFETCH     = $00050017;       { rw BOOL, Refetch inserted record }

  maxcurPROPS        = 23;              { keep in sync when adding cursor properties }

{ SQL Driver specific }
  curUPDLOCKMODE     = $04050000;       { rw UPDLockMode, Update lock mode }
  curGETHIDDENCOLUMNS= $04050004;       { rw BOOL , Get all selected columns from server. }
{ Delayed Updates Specific. }
  curDELAYUPDDISPLAYOPT   = $05050003;  { rw UINT16, view records }
  curDELAYUPDGETOLDRECORD = $05050004;  { rw BOOL, get un-modified }
  curDELAYUPDNUMUPDATES   = $05050005;  { ro INT32, num of updates }
{ Database properties }
{ General             }
  dbDATABASETYPE     = $00040002;       { ro pDBINAME , Database type }
  dbPARAMFMTQMARK    = $00040004;       { rw BOOL     , Stmt param marker fmt = ? }
  dbUSESCHEMAFILE    = $00040005;       { rw BOOL , for text driver only. }

{ SQL Driver specific }
  dbCOMPRESSARRAYFLDDESC  = $04040011;  { rw BOOL, VARRAY in compressed format, ORACLE 8 specific. }

{ Statement properties }
{ General              }
  stmtUNIDIRECTIONAL = $00060010;       { rw BOOL        Cursor Unidirectional }
  stmtROWCOUNT       = $00060014;       { ro UINT32      Rows effected by a stmt }

{ specific to QBE or local SQL }
  stmtLIVENESS       = $00060021;       { rw LIVENESS    Preference for canned/live answers }
  stmtAUXTBLS        = $00060026;       { rw BOOL        True if QBE to create CHANGED, etc. }
  stmtCANNEDREADONLY = $00060042;       { rw BOOL canned answers are readonly }




//============================================================================//
//                    Transactions                                            //
//============================================================================//
type
  eXILType = (                          { Transaction isolation levels }
    xilDIRTYREAD,                       { Uncommitted changes read }
    xilREADCOMMITTED,                   { Committed changes, no phantoms }
    xilREPEATABLEREAD                   { Full read repeatability }
  );

  eXEnd = (                             { Transaction end control }
    xendCOMMIT,                         { Commit transaction }
    xendCOMMITKEEP,                     { Commit transaction, keep cursors }
    xendABORT                           { Rollback transaction }
  );

  eXState = (                           { Transaction end control }
    xsINACTIVE,                         { Transaction inactive }
    xsACTIVE                            { Transaction active }
  );

  pXInfo = ^XInfo;
  XInfo = packed record
    exState         : eXState;          { xsActive, xsInactive }
    eXIL            : eXILType;         { Xact isolation level }
    uNests          : Word;             { Xact children }
  end;

//============================================================================//
//                    Object types                                            //
//============================================================================//

type
  DBIOBJType = (
    objFILLER,                          { Filler to make next start at 1 }
    objSYSTEM,                          { System object }
    objSESSION,                         { Session object }
    objDRIVER,                          { Driver object }
    objDATABASE,                        { Database object }
    objCURSOR,                          { Cursor object }
    objSTATEMENT,                       { Statement object }
    objCLIENT,                          { Client object }
    objDBSEC,                           { DbSystem object (dBASE only) }
    objREPOSITORY                       { Data Repository object }
  );

  pObjAttrDesc = ^ObjAttrDesc;
  ObjAttrDesc = packed record
    iFldNum    : Word;                  { Field id }
    pszAttributeName : PAnsiChar;           { Object attribute name }
  end;

  pObjTypeDesc = ^ObjTypeDesc;
  ObjTypeDesc = packed record
    iFldNum    : Word;                  { Field id }
    szTypeName : DBINAME;               { Object type name }
  end;



//============================================================================//
//                    Cursor properties                                       //
//============================================================================//

type
  DBIShareMode = (                      { Database/Table Share type }
    dbiOPENSHARED,                      { Open shared  (Default) }
    dbiOPENEXCL                         { Open exclusive }
  );

  DBIOpenMode = (                       { Database/Table Access type }
    dbiREADWRITE,                       { Read + Write   (Default) }
    dbiREADONLY                         { Read only }
  );

  DBILockType = (                       { Lock types (Table level) }
    dbiNOLOCK,                          { No lock   (Default) }
    dbiWRITELOCK,                       { Write lock }
    dbiREADLOCK                         { Read lock }
  );

  XLTMode = (                           { Field translate mode }
    xltNONE,                            { No translation  (Physical Types) }
    xltRECORD,                          { Record level translation (not supported) }
    xltFIELD                            { Field level translation (Logical types) }
  );

  pServerColDesc = ^ServerColDesc;
  ServerColDesc = packed record         { Auto increment and Defaults property }
   iFldNum     : Word;                  { Field id }
   bServerCol  : WordBool;              { Auto Increment and Default }
  end;


type
  pCURProps = ^CURProps;
  CURProps = packed record              { Virtual Table properties }
    szName          : DBITBLNAME;       { table name (no extension, if it can be derived) }
    iFNameSize      : Word;             { Full file name size }
    szTableType     : DBINAME;          { Driver type }
    iFields         : Word;             { No of fields in Table }
    iRecSize        : Word;             { Record size (logical record) }
    iRecBufSize     : Word;             { Record size (physical record) }
    iKeySize        : Word;             { Key size }
    iIndexes        : Word;             { Number of indexes }
    iValChecks      : Word;             { Number of val checks }
    iRefIntChecks   : Word;             { Number of Ref Integrity constraints }
    iBookMarkSize   : Word;             { Bookmark size }
    bBookMarkStable : WordBool;         { Stable book marks }
    eOpenMode       : DBIOpenMode;      { ReadOnly / RW }
    eShareMode      : DBIShareMode;     { Excl / Share }
    bIndexed        : WordBool;         { Index is in use }
    iSeqNums        : SmallInt;         { 1: Has Seqnums; 0: Has Record# }
    bSoftDeletes    : WordBool;         { Supports soft deletes }
    bDeletedOn      : WordBool;         { if above, deleted recs seen }
    iRefRange       : Word;             { Not used }
    exltMode        : XLTMode;          { Translate Mode }
    iRestrVersion   : Word;             { Restructure version number }
    bUniDirectional : WordBool;         { Cursor is uni-directional }
    eprvRights      : Word;             { Table  rights }
    Dummy4          : Word;
    iFmlRights      : Word;             { Family rights }
    iPasswords      : Word;             { Number of Aux passwords }
    iCodePage       : Word;             { Codepage (0 if unknown) }
    bProtected      : WordBool;         { Table is protected by password }
    iTblLevel       : Word;             { Driver dependent table level }
    szLangDriver    : DBINAME;          { Language driver name }
    bFieldMap       : WordBool;         { Field map active }
    iBlockSize      : Word;             { Physical file blocksize in K }
    bStrictRefInt   : WordBool;         { Strict referential integrity }
    iFilters        : Word;             { Number of filters }
    bTempTable      : WordBool;         { Table is a temporary table }
    iUnUsed         : packed array [0..15] of Word;
  end;

//Delayed Update Types and Constants }

type
  DBIDelayedUpdCmd = (                  { Op types for Delayed Update cursor }
    dbiDelayedUpdCommit,                { Commit the updates }
    dbiDelayedUpdCancel,                { Rollback the updates }
    dbiDelayedUpdCancelCurrent,         { Cancel the Current Rec Change }
    dbiDelayedUpdPrepare                { Phase1 of 2 phase commit }
  );

//============================================================================//
//                   Record Properties                                        //
//============================================================================//

type
  pRECProps = ^RECProps;
  RECProps = packed record              { Record properties }
    iSeqNum         : Longint;          { When Seq# supported only }
    iPhyRecNum      : Longint;          { When Phy Rec#s supported only }
    iRecStatus      : Word;             { Delayed Updates Record Status }
    bSeqNumChanged  : WordBool;         { Not used }
    bDeleteFlag     : WordBool;         { When soft delete supported only }
  end;

//============================================================================//
//                    Blob parameter descriptor                               //
//============================================================================//

type
  pBLOBParamDesc = ^BLOBParamDesc;
  BLOBParamDesc = packed record
    pBlobBuffer     : Pointer;          { Blob buffer (client) }
    ulBlobLen       : Integer;          { Length of the blob }
    iUnUsed         : packed array[0..3] of Word;
  end;


//============================================================================//
//                    Index descriptor                                        //
//============================================================================//

type
  pIDXDesc = ^IDXDesc;
  IDXDesc = record               { Index description }
    szName          : DBINAME;       { Index name }
    iIndexId        : Word;             { Index number }
    szTagName       : DBINAME;          { Tag name (for dBASE) }
    szFormat        : string;          { Optional format (BTREE, HASH etc) }
    bPrimary        : WordBool;         { True, if primary index }
    bUnique         : WordBool;         { True, if unique keys (TRI-STATE for dBASE) }
    bDescending     : WordBool;         { True, for descending index }
    bMaintained     : WordBool;         { True, if maintained index }
    bSubset         : WordBool;         { True, if subset index }
    bExpIdx         : WordBool;         { True, if expression index }
    iCost           : Word;             { Not used }
    iFldsInKey      : Word;             { Fields in the key (1 for Exp) }
    iKeyLen         : Word;             { Phy Key length in bytes (Key only) }
    bOutofDate      : WordBool;         { True, if index out of date }
    iKeyExpType     : Word;             { Key type of Expression }
    aiKeyFld        : DBIKEY;           { Array of field numbers in key }
    szKeyExp        : string;        { Key expression }
    szKeyCond       : string;        { Subset condition }
    bCaseInsensitive : WordBool;        { True, if case insensitive index }
    iBlockSize      : Word;             { Block size in bytes }
    iRestrNum       : Word;             { Restructure number }
    abDescending    : packed array [0..DBIMAXFLDSINKEY-1] of WordBool; { TRUE }
    iUnUsed         : packed array [0..15] of Word;
  end;

//============================================================================//
//                             Table / Field Types                            //
//============================================================================//
const
{ Field Types (Logical) }
  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit integer }
  fldUINT32          = 13;              { Unsigned 32 bit integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }
  {$IFDEF FPC}
  fldDATETIME        = 24;              { DateTime structure field }
  {$ENDIF}
  {$IFDEF DELPHI_6}
  fldDATETIME        = 24;              { DateTime structure field }
     {$IFDEF DELPHI_12}
      fldFMTBCD          = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}
      fldWIDESTRING      = 26;              { UCS2 null terminated string }
      MAXLOGFLDTYPES     = 27;              { Number of logical fieldtypes }
     {$ELSE}
     MAXLOGFLDTYPES     = 25;              { Number of logical fieldtypes }
     {$ENDIF}
  {$ELSE}
  MAXLOGFLDTYPES     = 24;              { Number of logical fieldtypes }
  {$ENDIF}

  {$IFDEF DELPHI_12}
  { Additional (non-BDE fieldtypes }
  fldUNICODE          = $1007;          { Unicode }
  {$ENDIF}

  //POSTGRES SPECIFIC
  fldTIMESTAMPTZ     = MAXLOGFLDTYPES + 1;
  fldUUID            = MAXLOGFLDTYPES + 2;
  fldINET            = MAXLOGFLDTYPES + 3;
  fldMACADDR         = MAXLOGFLDTYPES + 4;
  fldPOINT           = MAXLOGFLDTYPES + 5;
  fldCIRCLE          = MAXLOGFLDTYPES + 6;
  fldBOX             = MAXLOGFLDTYPES + 7;
  fldLSEG            = MAXLOGFLDTYPES + 8;

{ Sub Types (Logical) }

{ fldFLOAT subtype }

  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING subtype }

  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { CHAR type }
  fldstUNICODE       = 32;              { Unicode }

{ fldINT32 subtype }
  fldstAUTOINC       = 29;

{ fldADT subtype }

  fldstADTNestedTable = 35;             { ADT for nested table (has no name) }

{ fldDATE subtype }
  fldstADTDATE       = 37;              { DATE (OCIDate ) with in an ADT }

//============================================================================//
//                    Field descriptor                                        //
//============================================================================//
type
  FLDVchk = (                           { Field Val Check type }
    fldvNOCHECKS,                       { Does not have explicit val checks }
    fldvHASCHECKS,                      { One or more val checks on the field }
    fldvUNKNOWN                         { Dont know at this time }
  );

  FLDRights = (                         { Field Rights }
    fldrREADWRITE,                      { Field can be Read/Written }
    fldrREADONLY,                       { Field is Read only }
    fldrNONE,                           { No Rights on this field }
    fldrUNKNOWN                         { Dont know at this time }
  );

  pFLDDesc = ^FLDDesc;
  FLDDesc = packed record               { Field Descriptor }
    iFldNum         : Word;             { Field number (1..n) }
    szName          : string;          { Field name }
    iFldType        : Word;             { Field type }
    iSubType        : Word;             { Field subtype (if applicable) }
    iUnits1         : integer;         { Number of Chars, digits etc }
    iUnits2         : integer;         { Decimal places etc. }
    iOffset         : Word;             { Offset in the record (computed) }
    iLen            : Word;             { Length in bytes (computed) }
    iNullOffset     : Word;             { For Null bits (computed) }
    efldvVchk       : FLDVchk;          { Field Has vcheck (computed) }
    efldrRights     : FLDRights;        { Field Rights (computed) }
    bCalcField      : WordBool;         { Is Calculated field (computed) }
    iUnUsed         : packed array [0..1] of Word;
  end;

  TFLDDescList = array of FLDDesc;

  TIDXDescList = array of IDXDesc;

//============================================================================//
//             Validity check, Referential integrity descriptors              //
//============================================================================//
// Subtypes for Lookup
  LKUPType = (                          { Paradox Lookup type }
    lkupNONE,                           { Has no lookup }
    lkupPRIVATE,                        { Just Current Field + Private }
    lkupALLCORRESP,                     { All Corresponding + No Help }
    lkupHELP,                           { Just Current Fld + Help and Fill }
    lkupALLCORRESPHELP                  { All Corresponging + Help }
  );

  pVCHKDesc = ^VCHKDesc;
  VCHKDesc = packed record              { Val Check structure }
    iFldNum         : Word;             { Field number }
    bRequired       : WordBool;         { if True, value is required }
    bHasMinVal      : WordBool;         { if True, has min value }
    bHasMaxVal      : WordBool;         { if True, has max value }
    bHasDefVal      : WordBool;         { if True, has default value }
    aMinVal         : DBIVCHK;          { Min Value }
    aMaxVal         : DBIVCHK;          { Max Value }
    aDefVal         : string;           { Default value }
    szPict          : DBIPICT;          { Picture string }
    elkupType       : LKUPType;         { Lookup/Fill type }
    szLkupTblName   : string;          { Lookup Table name }
  end;

  RINTType = (                          { Ref integrity type }
    rintMASTER,                         { This table is Master }
    rintDEPENDENT                       { This table is Dependent }
  );

  RINTQual = (                          { Ref integrity action/qualifier }
    rintRESTRICT,                       { Prohibit operation }
    rintCASCADE                         { Cascade operation }
  );

  pRINTDesc = ^RINTDesc;
  RINTDesc = packed record              { Ref Integrity Desc }
    iRintNum        : Word;             { Ref integrity number }
    szRintName      : DBINAME;          { A name to tag this integegrity constraint }
    eType           : RINTType;         { Whether master/dependent }
    szTblName       : DBIPATH;          { Other table name }
    eModOp          : RINTQual;         { Modify qualifier }
    eDelOp          : RINTQual;         { Delete qualifier }
    iFldCount       : Word;             { Fields in foreign key }
    aiThisTabFld    : DBIKEY;           { Fields in this table }
    aiOthTabFld     : DBIKEY;           { Fields in other table }
  end;


//============================================================================//
//                    Security descriptor                                     //
//============================================================================//
type
  PRVType = TypedEnum;
const
    prvUNKNOWN   = $FF;                 { Unknown }

type
  pSECDesc = ^SECDesc;
  SECDesc = packed record               { Security description }
    iSecNum         : Word;             { Nbr to identify desc }
    eprvTable       : PrvType;          { Table privileges }
    iFamRights      : Word;             { Family rights }
    szPassword      : DBINAME;          { Null terminated string }
    aprvFld         : packed array [0..DBIMAXFLDSINSEC-1] of PrvType;
                     { Field level privileges (prvNONE/prvREADONLY/prvFULL) }
  end;

//============================================================================//
//                            Miscellaneous                                   //
//============================================================================//

{ Index Id used to open table without a default index (i.e. no order) }
const
  NODEFAULTINDEX     = $FFFF;


//============================================================================//
//                         BookMark compares                                  //
//============================================================================//

type
  PCMPBkMkRslt = ^CMPBkMkRslt;
  CMPBkMkRslt = TypedEnum;
const
    CMPLess           = -1;             { Bkm1 < Bkm2 }
    CMPEql            = 0;              { BookMarks are exactly the same }
    CMPGtr            = 1;              { Bkm1 > Bkm2 }
    CMPKeyEql         = 2;              { Only Bkm1.key_val = Bkm2.key_val }


{============================================================================}
{                             Key searches                                   }
{============================================================================}

type
  DBISearchCond = (                     { Search condition for keys }
    keySEARCHEQ,                        { = }
    keySEARCHGT,                        { > }
    keySEARCHGEQ                        { >= }
  );


//============================================================================//
//                      Create/Restructure descriptor                         //
//============================================================================//
type
  pCROpType          = ^CROpType;
  CROpType = (                          { Create/Restruct Operation type }
    crNOOP,
    crADD,                              { Add a new element. }
    crCOPY,                             { Copy an existing element. }
    crMODIFY,                           { Modify an element. }
    crDROP,                             { Removes an element. }
    crREDO,                             { Reconstruct an element. }
    crTABLE,                            { Not used }
    crGROUP,                            { Not used }
    crFAMILY,                           { Not used }
    crDONE,                             { Used internally }
    crDROPADD                           { Used internally }
  );

  pCRTblDesc         = ^CRTblDesc;
  CRTblDesc = packed record             { Create/Restruct Table descr }
    szTblName       : string;       { TableName incl. optional path & ext }
    //szTblType       : DBINAME;          { Driver type (optional) }
    //szErrTblName    : DBIPATH;          { Error Table name (optional) }
    //szUserName      : DBINAME;          { User name (if applicable) }
    //szPassword      : DBINAME;          { Password (optional) }
    //bProtected      : WordBool;         { Master password supplied in szPassword }
    //bPack           : WordBool;         { Pack table (restructure only) }
    iFldCount       : Word;             { Number of field defs supplied }
    pecrFldOp       : pCROpType;        { Array of field ops }
    pfldDesc        : pFLDDesc;         { Array of field descriptors }
    iIdxCount       : Word;             { Number of index defs supplied }
    pecrIdxOp       : pCROpType;        { Array of index ops }
    pidxDesc        : PIDXDesc;         { Array of index descriptors }
    iSecRecCount    : Word;             { Number of security defs supplied }
    pecrSecOp       : pCROpType;        { Array of security ops }
    psecDesc        : pSECDesc;         { Array of security descriptors }
    iValChkCount    : Word;             { Number of val checks }
    pecrValChkOp    : pCROpType;        { Array of val check ops }
    pvchkDesc       : pVCHKDesc;        { Array of val check descs }
    iRintCount      : Word;             { Number of ref int specs }
    pecrRintOp      : pCROpType;        { Array of ref int ops }
    printDesc       : pRINTDesc;        { Array of ref int specs }
    iOptParams      : Word;             { Number of optional parameters }
    pfldOptParams   : pFLDDesc;         { Array of field descriptors }
    pOptData        : Pointer;          { Optional parameters }
  end;

//============================================================================//
//                    Filter description                                      //
//============================================================================//

type
  pCANOp = ^CANOp;
  CANOp  = (
    canNOTDEFINED,                      {                                  (*) }
    canISBLANK,                         { CANUnary;  is operand blank.     (*) }
    canNOTBLANK,                        { CANUnary;  is operand not blank. (*) }
    canEQ,                              { CANBinary, CANCompare; equal.    (*) }
    canNE,                              { CANBinary; NOT equal.            (*) }
    canGT,                              { CANBinary; greater than.         (*) }
    canLT,                              { CANBinary; less than.            (*) }
    canGE,                              { CANBinary; greater or equal.     (*) }
    canLE,                              { CANBinary; less or equal.        (*) }
    canNOT,                             { CANUnary; NOT                    (*) }
    canAND,                             { CANBinary; AND                   (*) }
    canOR,                              { CANBinary; OR                    (*) }
    canTUPLE2,                          { CANUnary; Entire record is operand. }
    canFIELD2,                          { CANUnary; operand is field       (*) }
    canCONST2,                          { CANUnary; operand is constant    (*) }
    canMINUS,                           { CANUnary;  minus. }
    canADD,                             { CANBinary; addition. }
    canSUB,                             { CANBinary; subtraction. }
    canMUL,                             { CANBinary; multiplication. }
    canDIV,                             { CANBinary; division. }
    canMOD,                             { CANBinary; modulo division. }
    canREM,                             { CANBinary; remainder of division. }
    canSUM,                             { CANBinary, accumulate sum of. }
    canCOUNT,                           { CANBinary, accumulate count of. }
    canMIN,                             { CANBinary, find minimum of. }
    canMAX,                             { CANBinary, find maximum of. }
    canAVG,                             { CANBinary, find average of. }
    canCONT,                            { CANBinary; provides a link between two }
    canUDF2,                            { CANBinary; invokes a User defined fn }
    canCONTINUE2,                       { CANUnary; Stops evaluating records }
    canLIKE,                            { CANCompare, extended binary compare       (*) }
    canIN,                              { CANBinary field in list of values }
    canLIST2,                           { List of constant values of same type }
    canUPPER,                           { CANUnary: upper case }
    canLOWER,                           { CANUnary: lower case }
    canFUNC2,                           { CANFunc: Function }
    canLISTELEM2,                       { CANListElem: List Element }
    canASSIGN                           { CANBinary: Field assignment }
  );

  NODEClass = (                         { Node Class }
    nodeNULL,                           { Null node                  (*) }
    nodeUNARY,                          { Node is a unary            (*) }
    nodeBINARY,                         { Node is a binary           (*) }
    nodeCOMPARE,                        { Node is a compare          (*) }
    nodeFIELD,                          { Node is a field            (*) }
    nodeCONST,                          { Node is a constant         (*) }
    nodeTUPLE,                          { Node is a record }
    nodeCONTINUE,                       { Node is a continue node    (*) }
    nodeUDF,                            { Node is a UDF node }
    nodeLIST,                           { Node is a LIST node }
    nodeFUNC,                           { Node is a Function node }
    nodeLISTELEM                        { Node is a List Element node }
  );

// NODE definitions including misc data structures //
//-------------------------------------------------//

type
  pCANHdr = ^CANHdr;
  CANHdr = packed record                { Header part common to all     (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record              { Unary Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record             { Binary Node                   (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record              { Field }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iFieldNum       : Word;
    iNameOffset     : Word;             { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record              { Constant }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iType           : Word;             { Constant type. }
    iSize           : Word;             { Constant size. (in bytes) }
    iOffset         : Word;             { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record              { Tuple (record) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iSize           : Word;             { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;
  CANContinue = packed record           { Break Node                    (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iContOperand    : Word;             { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record            { Extended compare Node (text fields) (*) }
    nodeClass       : NODEClass;
    canOp           : CANOp;            { canLIKE, canEQ }
    bCaseInsensitive : WordBool;        { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen     : Word;             { Partial fieldlength (0 is full length) }
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record               { Function }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iNameOffset     : Word;             { Name offset in Literal pool }
    iElemOffset     : Word;             { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record           { List Element }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iOffset         : Word;             { Arg offset in Node pool }
    iNextOffset     : Word;             { Offset in Node pool of next ListElem or 0 if end of list }
  end;

  pCANList = ^CANList;
  CANList = packed record           { List of Constants }
    nodeClass       : NODEClass;
    canOp           : CANOp;
    iType           : Word;            { Constant type. }
    iTotalSize      : Word;            { Total list size; }
    iElemSize       : Word;            { Size of each elem for fix-width types }
    iElems          : Word;            { Number of elements in list }
    iOffset         : Word;            { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr      : CANHdr);
      1: (canUnary    : CANUnary);
      2: (canBinary   : CANBinary);
      3: (canField    : CANField);
      4: (canConst    : CANConst);
      5: (canTuple    : CANTuple);
      6: (canContinue : CANContinue);
      7: (canCompare  : CANCompare);
      8: (canList     : CANList);
      9: (canFunc     : CANFunc);
     10: (canListElem : CANListElem);
  end;

type
  ppCANExpr = ^pCANExpr;
  pCANExpr  = ^CANExpr;
  CANExpr   = packed record             { Expression Tree }
    iVer            : Word;             { Version tag of expression. }
    iTotalSize      : Word;             { Size of this structure }
    iNodes          : Word;             { Number of nodes }
    iNodeStart      : Word;             { Starting offet of Nodes in this }
    iLiteralStart   : Word;             { Starting offset of Literals in this }
  end;

  pfGENFilter = function (
      ulClientData  : Longint;
      pRecBuf       : Pointer;
      iPhyRecNum    : Longint
   ): SmallInt; stdcall;

//----------------------------------------------------------------------------//
//   DBI Query related types                                                  //
//----------------------------------------------------------------------------//

  LIVENESS = (
    wantDEFAULT,                        { Default , same as wantCANNED }
    wantLIVE,                           { Want live data even if extra effort (no guarantee) }
    wantCANNED,                         { Want canned data even if extra effort (guaranteed) }
    wantSPEED                           { Let query manager decide, find out afterwards }
  );

//============================================================================//
//                    Table descriptor                                        //
//============================================================================//
type
  pTBLBaseDesc = ^TBLBaseDesc;
  TBLBaseDesc = packed record           { Table description (Base) }
    szName          : DBITBLNAME;       { Table name(No extension or Dir) }
    szFileName      : DBITBLNAME;       { File name }
    szExt           : DBIEXT;           { File extension }
    szType          : DBINAME;          { Driver type }
    dtDate          : DBIDATE;          { Date on the table }
    tmTime          : Time;             { Time on the table }
    iSize           : Longint;          { Size in bytes }
    bView           : WordBool;         { if this a view }
    bSynonym        : WordBool;         { if this is a synonym }
  end;

{======================================================================}
{            Stored Procedure and Stored Procedure Param descriptor    }
{======================================================================}

type
  pSPDesc = ^SPDesc;
  SPDesc = packed record
    szName          : DBISPNAME;
    dtDate          : DBIDATE;
    tmTime          : Time;
  end;

  pSPParamDesc = ^SPParamDesc;
  SPParamDesc = packed record
    uParamNum       : Word;
    szName          : string;
    eParamType      : STMTParamType;
    uFldType        : Word;
    uSubType        : Word;
    iUnits1         : SmallInt;
    iUnits2         : SmallInt;
    uOffset         : Word;
    uLen            : Word;
    uNullOffset     : Word;
  end;


//============================================================================//
//                                Call Backs                                  //
//============================================================================//
type
  pCBType            = ^CBType;
  CBType = (                            { Call back type }
    cbGENERAL,                          { General purpose }
    cbRESERVED1,
    cbRESERVED2,
    cbINPUTREQ,                         { Input requested }
    cbRESERVED4,
    cbRESERVED5,
    cbBATCHRESULT,                      { Batch processing rslts }
    cbRESERVED7,
    cbRESTRUCTURE,                      { Restructure }
    cbRESERVED9,
    cbRESERVED10,
    cbRESERVED11,
    cbRESERVED12,
    cbRESERVED13,
    cbRESERVED14,
    cbRESERVED15,
    cbRESERVED16,
    cbRESERVED17,
    cbTABLECHANGED,                     { Table changed notification }
    cbRESERVED19,
    cbCANCELQRY,                        { Allow user to cancel Query }
    cbSERVERCALL,                       { Server Call }
    cbRESERVED22,
    cbGENPROGRESS,                      { Generic Progress report. }
    cbDBASELOGIN,                       { dBASE Login }
    cbDELAYEDUPD,                       { Delayed Updates }
    cbFIELDRECALC,                      { Field(s) recalculation }
    cbTRACE,                            { Trace }
    cbDBLOGIN,                          { Database login }
    cbDETACHNOTIFY,                     { DLL Detach Notification }
    cbNBROFCBS                          { Number of cbs }
  );

type
  pCBRType           = ^CBRType;
  CBRType = (                           { Call-back return type }
    cbrUSEDEF,                          { Take default action }
    cbrCONTINUE,                        { Continue }
    cbrABORT,                           { Abort the operation }
    cbrCHKINPUT,                        { Input given }
    cbrYES,                             { Take requested action }
    cbrNO,                              { Do not take requested action }
    cbrPARTIALASSIST,                   { Assist in completing the job }
    cbrSKIP,                            { Skip this operation }
    cbrRETRY                            { Retry this operation }
  );

  ppfDBICallBack = ^pfDBICallBack;
  pfDBICallBack  = function (           { Call-back funtion pntr type }
      ecbType       : CBType;           { Callback type }
      iClientData   : Longint;          { Client callback data }
      CbInfo        : Pointer           { Call back info/Client Input }
   ): CBRType; stdcall;

  DelayUpdErrOpType = (                 { type of delayed update object (delayed updates callback) }
    delayupdNONE,
    delayupdMODIFY,
    delayupdINSERT,
    delayupdDELETE
  );

  PDELAYUPDCbDesc = ^DELAYUPDCbDesc;
  DELAYUPDCbDesc = packed record        { delayed updates callback info }
    iErrCode        : DBIResult;
    eDelayUpdOpType : DelayUpdErrOpType;
    iRecBufSize     : Word;             { Record size (physical record) }
    pNewRecBuf      : Pointer;
    pOldRecBuf      : Pointer;
  end;



/////////////////////////////////////////////////////////////////////////////
//          CONSTANTS DEFINITION                                           //
/////////////////////////////////////////////////////////////////////////////
type
    TFieldArray = array[0..255] of Integer;
    TTrueArray = Set of AnsiChar;
    TFalseArray = Set of AnsiChar;
/////////////////////////////////////////////////////////////////////////////
//                        TPgSQLFilter TYPES AND CONST                     //
/////////////////////////////////////////////////////////////////////////////
type
  TFldType=(FT_UNK,FT_INT,FT_DATETIME,FT_DATE,FT_TIME, FT_CURRENCY,FT_FLOAT,FT_STRING,FT_BOOL);

  StrRec = record
     allocSiz : Longint;
     refCnt   : Longint;
     length   : Longint;
  end;

const
   strsz = sizeof(StrRec);

/////////////////////////////////////////////////////////////////////////////
//            INDEX AND PRIMARY KEY DEFINITIONS                            //
/////////////////////////////////////////////////////////////////////////////
type

  TPropRec = Record
    Prop  : Word;
    Group : Word;
  end;

  TBlobItem =  Record
    Blob : TMemoryStream;
  end;

  PPSQLBookMark = ^TPSQLBookMark;
  TPSQLBookMark =  Record
    Position     : Int64;//Longint;
  end;

  PFieldStatus = ^TFieldStatus;
  TFieldStatus =  Record
    isNULL  : SmallInt;
    Changed : LongBool;
  end;

  TRecordState = (tsNoPos, tsPos, tsFirst, tsLast, tsEmpty, tsClosed);
  TDir = (tdUndefined, tdNext, tdPrev);

  TSSLMode = (sslDisable , sslAllow, sslPrefer, sslRequire, sslVerifyCA, sslVerifyFull);

  TPSQLDatasetOption = (dsoByteaAsEscString, dsoOIDAsInt, dsoForceCreateFields,
                        dsoUseGUIDField, dsoTrimCharFields, dsoPopulateFieldsOrigin,
                        dsoManageLOFields, dsoEmptyCharAsNull, dsoUDTAsMaxString,
                        dsoRefreshModifiedRecordOnly);

  TPSQLDatasetOptions = set of TPSQLDatasetOption;

const
  SSLConsts: array[TSSLMode] of string = ('disable' , 'allow', 'prefer',
                                          'require', 'verify-ca', 'verify-full');

  SSLOpts: array[0..3] of string = ('sslcert', 'sslkey', 'sslrootcert', 'sslcrl');                                          

type
  TDBOptions = Record
    User             : String;
    Password         : String;
    DatabaseName     : String;
    Port             : Cardinal;
    Host             : String;
    SSLMode          : string;
    ConnectionTimeout: cardinal;
  end;


  PPGFIELD_INFO = ^TPGFIELD_INFO;
  TPGField_Info = record
     FieldIndex   : Integer;
     FieldName    : String;
     FieldType    : cardinal;
     FieldSize    : Integer;
     FieldMaxSize : Integer;
     FieldDefault : String;
  end;

/////////////////////////////////////////////////////////////////////////////
//            BASE OBJECTS DEFINITIONS                                     //
/////////////////////////////////////////////////////////////////////////////
  {TContainer Object}
  TContainer = Class(TObject)
    Private
      FItems : TList;
    Public
      Constructor Create;
      Destructor Destroy; Override;
      Function At( Index : integer ) : pointer;
      Procedure AtDelete( Index : integer );
      Procedure AtFree( Index : integer );
      Procedure AtInsert( Index: integer; Item : pointer );
      Procedure AtPut( Index : Integer; Item : Pointer );
      Procedure Clear;
      Procedure Delete( Item : Pointer );
      Procedure DeleteAll;
      Procedure Error( Code, Info : Integer );
      Procedure FreeAll;
      Procedure FreeItem( Item : pointer );
      Function Get( AIndex : integer ) : pointer;
      Function GetCount : integer;
      Function IndexOf( Item : pointer ) : integer;
      Procedure Insert( Item : pointer ); Virtual;
      Procedure Pack;
      Procedure Put( AIndex : integer; APointer : pointer );
      Function GetCapacity : Integer;
      Procedure SetCapacity( NewCapacity : Integer );
      Property Count: integer Read  GetCount;
      Property Items[ index : integer ] : pointer Read  Get Write Put;
      Property Capacity : Integer Read  GetCapacity Write SetCapacity;
  end;

  TBaseObject = Class(TObject)
    Protected
      FParent : TObject;
      FContainer: TContainer;
    Public
      Property Container : TContainer  Read  FContainer  Write FContainer;
      Property Parent : TObject  Read  FParent  Write FParent;
      Constructor Create(P : TObject; Container : TContainer);
      Destructor Destroy; Override;
  end;

//////////////////////////////////////////////////////////
//     Constants for Quick search
//////////////////////////////////////////////////////////
const
  ToUpperChars: array[0..255] of Char =
    (#$00,#$01,#$02,#$03,#$04,#$05,#$06,#$07,#$08,#$09,#$0A,#$0B,#$0C,#$0D,#$0E,#$0F,
     #$10,#$11,#$12,#$13,#$14,#$15,#$16,#$17,#$18,#$19,#$1A,#$1B,#$1C,#$1D,#$1E,#$1F,
     #$20,#$21,#$22,#$23,#$24,#$25,#$26,#$27,#$28,#$29,#$2A,#$2B,#$2C,#$2D,#$2E,#$2F,
     #$30,#$31,#$32,#$33,#$34,#$35,#$36,#$37,#$38,#$39,#$3A,#$3B,#$3C,#$3D,#$3E,#$3F,
     #$40,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$5B,#$5C,#$5D,#$5E,#$5F,
     #$60,#$41,#$42,#$43,#$44,#$45,#$46,#$47,#$48,#$49,#$4A,#$4B,#$4C,#$4D,#$4E,#$4F,
     #$50,#$51,#$52,#$53,#$54,#$55,#$56,#$57,#$58,#$59,#$5A,#$7B,#$7C,#$7D,#$7E,#$7F,
     #$80,#$81,#$82,#$81,#$84,#$85,#$86,#$87,#$88,#$89,#$8A,#$8B,#$8C,#$8D,#$8E,#$8F,
     #$80,#$91,#$92,#$93,#$94,#$95,#$96,#$97,#$98,#$99,#$8A,#$9B,#$8C,#$8D,#$8E,#$8F,
     #$A0,#$A1,#$A1,#$A3,#$A4,#$A5,#$A6,#$A7,#$A8,#$A9,#$AA,#$AB,#$AC,#$AD,#$AE,#$AF,
     #$B0,#$B1,#$B2,#$B2,#$A5,#$B5,#$B6,#$B7,#$A8,#$B9,#$AA,#$BB,#$A3,#$BD,#$BD,#$AF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF,
     #$C0,#$C1,#$C2,#$C3,#$C4,#$C5,#$C6,#$C7,#$C8,#$C9,#$CA,#$CB,#$CC,#$CD,#$CE,#$CF,
     #$D0,#$D1,#$D2,#$D3,#$D4,#$D5,#$D6,#$D7,#$D8,#$D9,#$DA,#$DB,#$DC,#$DD,#$DE,#$DF);
  

/////////////////////////////////////////////////////////////////////////////
//                  COMMON FUNCTIONS                                       //
/////////////////////////////////////////////////////////////////////////////
{ SQL Parser }
type
  TSQLToken = (stUnknown, stTableName, stFieldName, stAscending, stDescending, stSelect,
    stFrom, stWhere, stGroupBy, stHaving, stUnion, stPlan, stOrderBy, stForUpdate,
    stEnd, stPredicate, stValue, stIsNull, stIsNotNull, stLike, stAnd, stOr,
    stNumber, stAllFields, stComment, stDistinct,stSubSelect,stFunction,stAliace,stAs);

const
  SQLSections = [stSelect, stFrom, stWhere, stGroupBy, stHaving, stUnion,
    stPlan, stOrderBy, stForUpdate];
{$IFDEF DELPHI_4}
  curAUTOREFETCH     = $00050017;       { rw BOOL, Refetch inserted record }
{$ENDIF}

function NextSQLToken(var p: PChar; out Token: string; CurSection: TSQLToken): TSQLToken;
function GetTable(const SQL: String; var Aliace : String): String;
function CompareBegin(Str1, Str2: ansistring): Boolean;


function SqlDateToDateTime(Value: string; const IsTime: boolean): TDateTime;
function DateTimeToSqlDate(Value: TDateTime; Mode : integer): string;
function SQLTimeStampToDateTime(Value: string): TDateTime;
function StrToSQLFloat(Value: string): Double;
function SQLFloatToStr(Value: Double): string;
function SQLPointToPoint(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLPoint;
function PointToSQLPoint(Value: TPSQLPoint; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLCircleToCircle(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLCircle;
function CircleToSQLCircle(Value: TPSQLCircle; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLBoxToBox(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLBox;
function BoxToSQLBox(Value: TPSQLBox; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
function SQLLSegToLSeg(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLLSeg;
function LSegToSQLLSeg(Value: TPSQLLSeg; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;

procedure GetToken(var Buffer, Token: string);
procedure ConverPSQLtoDelphiFieldInfo(Info : TPGFIELD_INFO; Count, Offset : Word;
                                        var RecBuff : FLDDesc;
                                        var ValChk : VCHKDesc;
                                        var LocArray : Boolean);

procedure LoadPSQLLibrary(LibPQPath: string = '');
procedure UnloadPSQLLibrary;
procedure CheckLibraryLoaded;

function IsValidIP(const S: string): boolean;

function MaskSearch(const Str, Mask: string;
                    CaseSensitive : boolean = true;
                    MaskChar: Char = '?';
                    WildCard: Char = '%'): Boolean;

function Search(Op1,Op2 : Variant; OEM, CaseSen : Boolean; PartLen: Integer):Boolean;
function GetBDEErrorMessage(ErrorCode : Word):String;
procedure FieldMapping(FieldType : cardinal; phSize : Integer; var BdeType : Word;
                        var BdeSubType : Word; var LogSize : Integer;
                        var LocArray : Boolean);

function UIntToStr(C: cardinal): string;
function StrToUInt(S: string): cardinal;
function StrToUIntDef(S: string; DefVal: cardinal = 0): cardinal;

{$IFNDEF DELPHI_12}
type
 TCharSet = set of char;

 function CharInSet(C: Char; const CharSet: TCharSet): Boolean;
{$ENDIF}

{$IFNDEF DELPHI_12}

 {$IFDEF DELPHI_5}
   function Utf8Encode(const WS: WideString): AnsiString;
 {$ENDIF}

 function UTF8ToString(const S: String): string;
{$ENDIF}

{$IFDEF DELPHI_5}
function GetModuleName(Module: HMODULE): string;
{$ENDIF}


//function for compatibility with FreePascal and MacOS
procedure ZeroMemory(Destination: Pointer; Length: integer);
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: integer);
function GetTickCount: LongWord; //thanks to Indy project
function GetTickDiff(const AOldTickCount, ANewTickCount: LongWord): LongWord;

implementation

uses DB, PSQLDbTables, PSQLAccess{$IFDEF DELPHI_6}, StrUtils{$ENDIF};

const
  IPv4BitSize = SizeOf(Byte) * 4 * 8;
  IPv6BitSize = SizeOf(Word) * 8 * 8;

type
  T4 = 0..3;
  T8 = 0..7;
  TIPv4ByteArray = array[T4] of Byte;
  TIPv6WordArray = array[T8] of Word;

  TIPv4 = packed record
    case Integer of
      0: (D, C, B, A: Byte);
      1: (Groups: TIPv4ByteArray);
      2: (Value: Cardinal);
  end;

  TIPv6 = packed record
    case Integer of
      0: (H, G, F, E, D, C, B, A: Word);
      1: (Groups: TIPv6WordArray);
  end;

{$IFDEF UNDER_DELPHI_6}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function TryStrToInt(const S: string; out V: integer): boolean;
var Code: integer;
begin
  Val(S, V, Code);
  Result := Code = 0;
end;
{$ENDIF UNDER_DELPHI_6}

function TryStrToIPv4(const S: String; out Value: TIPv4): boolean;
var
  SIP: String;
  Start: Integer;
  I: T4;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;
begin
  Result := False;
  SIP := S + '.';
  Start := 1;
  for I := High(T4) downto Low(T4) do
  begin
    Index := PosEx('.', SIP, Start);
    if Index = 0 then
      Exit;
    Count := Index - Start + 1;
    SGroup := Copy(SIP, Start, Count - 1);
    if TryStrToInt(SGroup, G) and (G >= Low(Byte)) and (G < High(Byte)) then
        Value.Groups[I] := G
      else
        Exit;
    Inc(Start, Count);
  end;
  Result := True;
end;

function TryStrToIPv6(const S: String; out Value: TIPv6): boolean;
{ Valid examples for S:
  2001:0db8:85a3:0000:0000:8a2e:0370:7334
  2001:db8:85a3:0:0:8a2e:370:7334
  2001:db8:85a3::8a2e:370:7334
  ::8a2e:370:7334
  2001:db8:85a3::
  ::1
  ::
  ::ffff:c000:280
  ::ffff:192.0.2.128 }
var
  ZeroPos: Integer;
  DotPos: Integer;
  SIP: String;
  Start: Integer;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;

  procedure NormalNotation;
  var
    I: T8;
  begin
    SIP := S + ':';
    Start := 1;
    for I := High(T8) downto Low(T8) do
    begin
      Index := PosEx(':', SIP, Start);
      if Index = 0 then
        Exit;
      Count := Index - Start + 1;
      SGroup := '$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
        Exit;
      Value.Groups[I] := G;
      Inc(Start, Count);
    end;
    Result := True;
  end;

  procedure CompressedNotation;
  var
    I: T8;
    A: array of Word;
  begin
    SIP := S + ':';
    Start := 1;
    I := High(T8);
    while Start < ZeroPos do
    begin
      Index := PosEx(':', SIP, Start);
      if Index = 0 then
        Exit;
      Count := Index - Start + 1;
      SGroup := '$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
        Exit;
      Value.Groups[I] := G;
      Inc(Start, Count);
      Dec(I);
    end;
    FillChar(Value.H, (I + 1) * SizeOf(Word), 0);
    if ZeroPos < (Length(S) - 1) then
    begin
      SetLength(A, I + 1);
      Start := ZeroPos + 2;
      repeat
        Index := PosEx(':', SIP, Start);
        if Index > 0 then
        begin
          Count := Index - Start + 1;
          SGroup := '$' + Copy(SIP, Start, Count - 1);
          if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
            Exit;
          A[I] := G;
          Inc(Start, Count);
          Dec(I);
        end;
      until Index = 0;
      Inc(I);
      Count := Length(A) - I;
      Move(A[I], Value.H, Count * SizeOf(Word));
    end;
    Result := True;
  end;

  procedure DottedQuadNotation;
  var
    I: T4;
  begin
    if UpperCase(Copy(S, ZeroPos + 2, 4)) <> 'FFFF' then
        Exit;
    FillChar(Value.E, 5 * SizeOf(Word), 0);
    Value.F := $FFFF;
    SIP := S + '.';
    Start := ZeroPos + 7;
    for I := Low(T4) to High(T4) do
    begin
      Index := PosEx('.', SIP, Start);
      if Index = 0 then
        Exit;
      Count := Index - Start + 1;
      SGroup := Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Byte)) or (G < 0) then
        Exit;
      case I of
        0: Value.G := G shl 8;
        1: Inc(Value.G, G);
        2: Value.H := G shl 8;
        3: Inc(Value.H, G);
      end;
      Inc(Start, Count);
    end;
    Result := True;
  end;

begin
  Result := False;
  ZeroPos := Pos('::', S);
  if ZeroPos = 0 then
    NormalNotation
  else
  begin
    DotPos := Pos('.', S);
    if DotPos = 0 then
      CompressedNotation
    else
      DottedQuadNotation;
  end;
end;

function IsValidIP(const S: string): boolean;
var IP4: TIPv4;
    IP6: TIPv6;
begin
  Result := TryStrToIPv4(S, IP4) or TryStrToIPv6(S, IP6);
end;

procedure ZeroMemory(Destination: Pointer; Length: integer);
begin
  FillChar(Destination^, Length, 0);
end;

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: integer);
begin
  Move(Source^, Destination^, Length);
end;

function GetTickCount: LongWord;
{$IFDEF DELPHI_12}inline;{$ENDIF}
begin
{$IFDEF MACOS}
  Result := AbsoluteToNanoseconds(UpTime) div 1000000;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount;
{$ENDIF}
end;

function GetTickDiff(const AOldTickCount, ANewTickCount: LongWord): LongWord;
{$IFDEF DELPHI_12}inline;{$ENDIF}
begin
  {This is just in case the TickCount rolled back to zero}
  if ANewTickCount >= AOldTickCount then begin
    Result := ANewTickCount - AOldTickCount;
  end else begin
    Result := High(LongWord) - AOldTickCount + ANewTickCount;
  end;
end;

{$IFDEF DELPHI_5}
function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;
{$ENDIF}

{$IFNDEF DELPHI_12}

  {$IFDEF DELPHI_5}
  function Utf8Encode(const WS: WideString): AnsiString;
  begin
    Result := WS;
  end;
  {$ENDIF}

  function UTF8ToString(const S: String): string;
  begin
    {$IFDEF DELPHI_5}
    Result := S;
    {$ELSE}
    Result := Utf8Decode(S);
    {$ENDIF}
  end;

{$ENDIF}

{$IFNDEF DELPHI_12}
function CharInSet(C: Char; const CharSet: TCharSet): Boolean;
begin
 Result := C in CharSet;
end;
{$ENDIF}

Constructor TContainer.Create;
begin
  Inherited Create;
  FItems := TList.Create;
end;

Destructor TContainer.Destroy;
begin
  FreeAll;
  FItems.Free;
  Inherited Destroy;
end;

Function TContainer.At(Index : integer) : Pointer;
begin
  Try
    Result := FItems[Index];
  Except
    On E:EListError do Result := nil;
  end;
end;

Procedure TContainer.AtDelete(Index : integer);
begin
  FItems.Delete(Index);
end;

Procedure TContainer.AtFree( Index : integer );
var
  Item : Pointer;
begin
  Item := At(Index);
  if Item <> nil then
  begin
    AtDelete(Index);
    FreeItem(Item);
  end;
end;

Procedure TContainer.AtInsert( Index : integer; Item : pointer );
begin
  FItems.Insert( Index, Item );
end;

Procedure TContainer.AtPut( Index : integer; Item : pointer );
begin
  FItems[ Index ] := Item;
end;

Procedure TContainer.Clear;
begin
  FItems.Clear;
end;

Procedure TContainer.Delete( Item : pointer );
var
  i : Integer;
begin
  i := IndexOf( Item );
  if i <> -1  then  AtDelete(i);
end;

Procedure TContainer.DeleteAll;
begin
  FItems.Clear;
end;

Procedure TContainer.Error( Code, Info : integer );
begin
  Raise EListError.Create( 'Container index out of range' );
end;

Procedure TContainer.FreeAll;
var
  I : integer;
begin
  Try
    for I := Count -1 downto 0 do
      FreeItem(At(I));
  Except
    On EListError do ;
  End;
  FItems.Clear;
end;

Procedure TContainer.FreeItem( Item : pointer );
begin
  if Item <> nil  then TObject(Item).Free;
end;

Function TContainer.Get(AIndex : integer) : pointer;
begin
  Result := FItems[AIndex];
end;

Function TContainer.GetCount: integer;
begin
  Result := FItems.Count;
end;

Function TContainer.IndexOf( Item : pointer ) : integer;
begin
  Result := FItems.IndexOf( Item );
end;

Procedure TContainer.Insert(Item : pointer);
begin
  FItems.Add(Item);
end;

Procedure TContainer.Pack;
begin
  FItems.Pack;
end;

Procedure TContainer.Put( AIndex : integer; APointer : pointer );
begin
  FItems[AIndex] := APointer;
end;

Function TContainer.GetCapacity : Integer;
begin
  Result := FItems.Capacity;
end;

Procedure TContainer.SetCapacity( NewCapacity : Integer );
begin
  FItems.Capacity := NewCapacity;
end;

/////////////////////////////////////////////////////////////////////////////
//                  IMPLEMENTATION TBASEOBJECT OBJECT                      //
/////////////////////////////////////////////////////////////////////////////
Constructor TBaseObject.Create(P : TObject; Container : TContainer);
begin
  Inherited Create;
  FParent    := P;
  FContainer := Container;
  if FContainer <> nil then FContainer.Insert(Self);
end;

Destructor TBaseObject.Destroy;
begin
  if FContainer <> nil then FContainer.Delete(Self);
  Inherited Destroy;
end;

/////////////////////////////////////////////////////////////////////////////
//                  IMPLEMENTATION COMMON FUNCTIONS                        //
/////////////////////////////////////////////////////////////////////////////
{ SQL Parser }
function NextSQLToken(var p: PChar; out Token: string; CurSection: TSQLToken): TSQLToken;
var
  DotStart: Boolean;

  function NextTokenIs(Value: string; var Str: string): Boolean;
  var
    Tmp: PChar;
    S: string;
  begin
    Tmp := p;
    NextSQLToken(Tmp, S, CurSection);
    Result := AnsiCompareText(Value, S) = 0;
    if Result then
    begin
      Str := Str + ' ' + S;
      p := Tmp;
    end;
  end;

  function GetSQLToken(var Str: string): TSQLToken;
  var
    l: PChar;
    s: string;
  begin
    if Length(Str) = 0 then
      Result := stEnd else
    if (Str = '*') and (CurSection = stSelect) then
      Result := stAllFields else
    if DotStart then
      Result := stFieldName else
    if (AnsiCompareText('DISTINCT', Str) = 0) and (CurSection = stSelect) then
      Result := stDistinct else
    if (AnsiCompareText('ASC', Str) = 0) or (AnsiCompareText('ASCENDING', Str) = 0)then
      Result := stAscending else
    if (AnsiCompareText('DESC', Str) = 0) or (AnsiCompareText('DESCENDING', Str) = 0)then
      Result := stDescending else
    if (AnsiCompareText('SELECT', Str) = 0) and (CurSection = stUnknown) then
      Result := stSelect else
    if (AnsiCompareText('SELECT', Str) = 0) and (CurSection in [stSelect, stFieldName]) then
      Result := stSubSelect else
    if AnsiCompareText('AND', Str) = 0 then
      Result := stAnd else
    if AnsiCompareText('OR', Str) = 0 then
      Result := stOr else
    if AnsiCompareText('LIKE', Str) = 0 then
      Result := stLike else
    if (AnsiCompareText('AS', Str) = 0) then
      Result := stAs else
    if (AnsiCompareText('IS', Str) = 0) then
    begin
      if NextTokenIs('NULL', Str) then
        Result := stIsNull else
      begin
        l := p;
        s := Str;
        if NextTokenIs('NOT', Str) and NextTokenIs('NULL', Str) then
          Result := stIsNotNull else
        begin
          p := l;
          Str := s;
          Result := stValue;
        end;
      end;
    end else
    if AnsiCompareText('FROM', Str) = 0 then
      Result := stFrom else
    if AnsiCompareText('WHERE', Str) = 0 then
      Result := stWhere else
    if (AnsiCompareText('GROUP', Str) = 0) and NextTokenIs('BY', Str) then
      Result := stGroupBy else
    if AnsiCompareText('HAVING', Str) = 0 then
      Result := stHaving else
    if AnsiCompareText('UNION', Str) = 0 then
      Result := stUnion else
    if AnsiCompareText('PLAN', Str) = 0 then
      Result := stPlan else
    if (AnsiCompareText('FOR', Str) = 0) and NextTokenIs('UPDATE', Str) then
      Result := stForUpdate else
    if (AnsiCompareText('ORDER', Str) = 0) and NextTokenIs('BY', Str)  then
      Result := stOrderBy else
    if AnsiCompareText('NULL', Str) = 0 then
      Result := stValue else
    if AnsiCompareText('SUBSTRING', Str) = 0 then
      Result := stFunction else
    if AnsiCompareText('TRIM', Str) = 0 then
      Result := stFunction else
    if CurSection = stFrom then
      Result := stTableName else
    if (CurSection = stTableName) or (CurSection = stAs) then
      Result := stAliace else
      Result := stFieldName;

  end;

var
  TokenStart: PChar;

  procedure StartToken;
  begin
    if not Assigned(TokenStart) then
      TokenStart := p;
  end;

var
  Literal: Char;
  Mark: PChar;
  BracketCount : integer;
  LoopEnd: boolean;
begin
  TokenStart := nil;
  DotStart := False;
  while True do
  begin
    case p^ of
      '"','''','`':
      begin
        StartToken;
        Literal := p^;
        Mark := p;
        //changed by pasha_golub 29.12.04, to deal with schema names
        repeat
         Inc(p);
         LoopEnd := (p^ = Literal) or (p^ = #0);
         if LoopEnd and (p^ <> #0) then
           begin
            inc(p);
            if p^ = '.' then
              begin
               inc(p,2);
               LoopEnd := False;
              end;
           end;
        until LoopEnd;
        if p^ = #0 then
        begin
          p := Mark;
          Inc(p);
        end else
        begin
          Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Token := Trim(Token);
          if DotStart then
            Result := stFieldName else
          if p^ = '.' then
            Result := stTableName else
            Result := stValue;
          Exit;
        end;
      end;
      '/':
      begin
        StartToken;
        Inc(p);
        if (p^ = '/') or (p^ = '*') then
        begin
          if p^ = '*' then
          begin
            repeat Inc(p) until (p = #0) or ((p^ = '*') and (p[1] = '/'));
          end else
            while (p^ <> #0) and (p^ <> #10) and (p^ <> #13) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      ' ', #10, #13, ',', '(' ,')':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          if Result = stSubSelect then
             repeat Inc(p) until (p^ = ')') else
          Exit;
        end else
        begin
           if not CharInSet(p^, ['(',')']) then
              while CharInSet(p^, [' ', #10, #13, ',']) do Inc(p) else
           begin
              BracketCount := 1;
              repeat
                  Inc(p);
                  if p^ = '(' then Inc(BracketCount);
                  if p^ = ')' then Dec(BracketCount);
              until (BracketCount = 0) or (p^ = #0) {safety measure};
              Inc(p);
           end;
        end;

      end;
      '.':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := stTableName;
          Exit;
        end else
        begin
          DotStart := True;
          Inc(p);
        end;
      end;
      '=','<','>':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while CharInSet(p^, ['=','<','>']) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stPredicate;
          Exit;
        end;
        Inc(p);
      end;
      '0'..'9':
      begin
        if not Assigned(TokenStart) then
        begin
          TokenStart := p;
          while CharInSet(p^, ['0'..'9','.']) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stNumber;
          Exit;
        end else
          Inc(p);
      end;
      #0:
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          Exit;
        end else
        begin
          Result := stEnd;
          Token := '';
          Exit;
        end;
      end;
    else
      StartToken;
      Inc(p);
    end;
  end;
end;

function GetTable(const SQL: String; var Aliace : String): string;
var
  Start: PChar;
  Token: string;
  SQLToken, CurSection: TSQLToken;
begin
  Result := '';
  Start := PChar(SQL);
  CurSection := stUnknown;
  repeat
    SQLToken := NextSQLToken(Start, Token, CurSection);
    if SQLToken in SQLSections then CurSection := SQLToken;
  until SQLToken in [stEnd, stFrom];
  if SQLToken = stFrom then
  begin
    repeat
      SQLToken := NextSQLToken(Start, Token, CurSection);
      if SQLToken in SQLSections then
        CurSection := SQLToken else
        if (SQLToken = stTableName) or (SQLToken = stValue) then
        begin
           Result := Token;
           Aliace := '';
           if Start[0] = '.' then
            begin
             CurSection := SQLToken;
             SQLToken := NextSQLToken(Start, Token, CurSection);
             if (SQLToken = stFieldName) or (SQLToken = stValue)
              then Result := Result + '.' + Token;
            end;

           while (Start[0] = ' ') and not (SQLToken in [stEnd]) do
           begin
              CurSection := SQLToken;
              SQLToken := NextSqlToken(Start, Token, CurSection);
              if SQLToken = stAliace then
                 Aliace := Token;
           end;
           Exit;
        end;
    until (CurSection <> stFrom) or (SQLToken in [stEnd, stTableName]);
  end;
end;



function CompareBegin(Str1, Str2: ansistring): Boolean;
begin
  if ((Str1 = '') or (Str2 = '')) and (Str1 <> Str2) then
    Result := False  else
    Result := (StrLIComp(PAnsiChar(Str1), PAnsiChar(Str2), Min(Length(Str1), Length(Str2))) = 0);
end;

function SqlDateToDateTime(Value: string; const IsTime: boolean): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Integer;
  Temp: string;
begin
  Temp   := Value;
  try
   if not IsTime then
    begin
      Year  := StrToIntDef(Copy(Temp,1,4),1);
      Month := StrToIntDef(Copy(Temp,6,2),1);
      Day   := StrToIntDef(Copy(Temp,9,2),1);
      Result := EncodeDate(Year, Month, Day);
    end
   else
    begin
      Hour := StrToIntDef(Copy(Temp,1,2),0);
      Min  := StrToIntDef(Copy(Temp,4,2),0);
      Sec  := StrToIntDef(Copy(Temp,7,2),0);
      MSec := StrToIntDef(Copy(Copy(Temp,10,3) + '000',1,3),0); //19.05.2008: for cases when trailing 0 are missing
      Result := EncodeTime(Hour, Min, Sec, Msec);
    end;
  except
    Result := 0;
  end;
end;

function DateTimeToSqlDate(Value: TDateTime; Mode: Integer): string;
begin
  Result := '';
  case Mode of
     TIMESTAMP_MODE:
        begin
           if Trunc(Value) <> 0 then Result := FormatDateTime('mm-dd-yyyy', Value, PSQL_FS);
           if Frac(Value) <> 0 then
           begin
              if Result <> '' then Result := Result + ' ';
              Result := Result + FormatDateTime('hh:nn:ss.z', Value, PSQL_FS);
           end;
        end;

     DATE_MODE:
        if Trunc(Value) <> 0 then
          Result := FormatDateTime('mm-dd-yyyy', Value, PSQL_FS);

     TIME_MODE:
           if Frac(Value) <> 0 then
              Result := Result + FormatDateTime('hh:nn:ss.z', Value, PSQL_FS);
           end;
        end;

function SQLTimestampToDateTime(Value: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Integer;
begin
  if value = 'infinity' then
      Result := 1.7e+308	//EncodeDate(9999, 12, 31) + EncodeTime(0, 0, 0, 0)
  else
   if value = '-infinity' then
      Result := -5.0e+324  //EncodeDate(0, 1, 1) + EncodeTime(0, 0, 0, 0)
   else
    begin
      Year  := Max(1, StrToIntDef(Copy(Value, 1, 4), 1));
      Month := Max(1, StrToIntDef(Copy(Value, 6, 2), 1));
      Day   := Max(1, StrToIntDef(Copy(Value, 9, 2), 1));
      Hour := StrToIntDef(Copy(Value, 12, 2), 0);
      Min  := StrToIntDef(Copy(Value, 15, 2), 0);
      Sec  := StrToIntDef(Copy(Value, 18, 2), 0);
      Msec := StrToIntDef(Copy(Copy(Value, 21, 3) + '000',1,3), 0); //19.05.2008: for cases when trailing 0 are missing
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
    end;
end;

function StrToSQLFloat(Value: string): Double;
begin
  if Value <> '' then
    try
      Result := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Value, PSQL_FS);
    except
      Result := 0;
    end
  else
    Result := 0;
end;

function SQLFloatToStr(Value: Double): string;
begin
  Result := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}FloatToStr(Value, PSQL_FS);
end;

function SQLPointToPoint(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLPoint;
var S, Xs, Ys: string;
    DelimPos: integer;
begin
 S := Copy(Value, 2, Length(Value) - 2); //eliminate brackets
 DelimPos := Pos(Delimiter, S);
 Xs := Copy(S, 1, DelimPos - 1);
 Ys := Copy(S, DelimPos + 1, MaxInt);
 if not UseSystemSeparator then
   begin
    Result.X := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Xs, PSQL_FS);
    Result.Y := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Ys, PSQL_FS);
   end
 else
   begin
    Result.X := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Xs);
    Result.Y := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Ys);
   end;
end;

function PointToSQLPoint(Value: TPSQLPoint; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  if UseSystemSeparator then
    Result := Format('(%g' + Delimiter +'%g)', [Value.X, Value.Y])
  else
    Result := '(' + SQLFloatToStr(Value.X) + Delimiter + SQLFloatToStr(Value.Y) + ')';
end;

function SQLCircleToCircle(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLCircle;
var S, Rs: string;
    DelimPos: integer;
begin
 S := Copy(Value, 2, Length(Value) - 2); //eliminate <> brackets
 DelimPos := Pos(')', S);
 Result.Center := SQLPointToPoint(Copy(S, 1, DelimPos), Delimiter, UseSystemSeparator);
 Rs := Copy(S, DelimPos + 2, MaxInt); //closing bracket plus delimiter
 if not UseSystemSeparator then
    Result.R := {$IFDEF UNDER_DELPHI_6}PSQLAccess.{$ENDIF}StrToFloat(Rs, PSQL_FS)
 else
    Result.R := {$IFDEF UNDER_DELPHI_6}SysUtils.{$ENDIF}StrToFloat(Rs);
end;

function CircleToSQLCircle(Value: TPSQLCircle; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := PointToSQLPoint(Value.Center, Delimiter, UseSystemSeparator);
  with Value do
    if UseSystemSeparator then
      Result := '<' + Result + Delimiter + Format('%g>', [R])
    else
      Result := '<' + Result + Delimiter + SQLFloatToStr(R) + '>';
end;

function SQLBoxToBox(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLBox;
var DelimPos: integer;
begin
  DelimPos := Pos(')', Value) + 1;
  Result.TopRight := SQLPointToPoint(Copy(Value, 1, DelimPos - 1), Delimiter, UseSystemSeparator);
  Result.BottomLeft := SQLPointToPoint(Copy(Value, DelimPos + 1, MaxInt), Delimiter, UseSystemSeparator);
end;

function BoxToSQLBox(Value: TPSQLBox; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := PointToSQLPoint(Value.TopRight, Delimiter, UseSystemSeparator) + Delimiter + PointToSQLPoint(Value.BottomLeft, Delimiter, UseSystemSeparator);
end;

function SQLLSegToLSeg(Value: string; const Delimiter: char = ','; const UseSystemSeparator: boolean = False): TPSQLLSeg;
var
  DelimPos: integer;
  S: string;
begin
  S := Copy(Value, 2, Length(Value) - 2); //eliminate [] brackets
  DelimPos := Pos(')', S) + 1;
  Result.P1 := SQLPointToPoint(Copy(S, 1, DelimPos - 1), Delimiter, UseSystemSeparator);
  Result.P2 := SQLPointToPoint(Copy(S, DelimPos + 1, MaxInt), Delimiter, UseSystemSeparator);
end;

function LSegToSQLLSeg(Value: TPSQLLSeg; const Delimiter: char = ','; const UseSystemSeparator: boolean = False) : string;
begin
  Result := '[' + PointToSQLPoint(Value.P1, Delimiter, UseSystemSeparator) + Delimiter + PointToSQLPoint(Value.P2, Delimiter, UseSystemSeparator) + ']';
end;

procedure GetToken(var Buffer, Token: string);
label ExitProc;
var
  P: Integer;
  Quote: string;
begin
  P := 1;
  Token  := '';
  if Buffer = '' then Exit;
  while CharInSet(Buffer[P], [' ',#9]) do
  begin
    Inc(P);
    if Length(Buffer) < P then  goto ExitProc;
  end;
  if (Pos(Buffer[P],DELIMITERS) <> 0) then
  begin
    Token  := Buffer[P];
    Inc(P);
    goto ExitProc;
  end;
  if CharInSet(Buffer[P], ['"','''']) then
  begin
    Quote  := Buffer[P];
    Token  := Quote;
    Inc(P);
    while P <= Length(Buffer) do
    begin
      Token := Token + Buffer[P];
      Inc(P);
      if (Buffer[P-1] = Quote) and (Buffer[P-2] <> '\') then  Break;
    end;
  end else
  begin
    while P <= Length(Buffer) do
    begin
      Token := Token + Buffer[P];
      Inc(P);
      if (P > Length(Buffer)) or (Pos(Buffer[P],DELIMITERS) <> 0) or CharInSet(Buffer[P], ['"','''']) then Break;
    end;
  end;
ExitProc:
  Delete(Buffer, 1, P-1);
end;

function GetInArrayField(FieldType : Word): boolean;
var
  I : Integer;
begin
   Result := False;
   for i := 0 to MAXARRFLDTYPES-1 do
   begin
      if FieldType = FldArrayType[I] then
      begin
         Result := True;
         Break;
      end;
   end;
end;

function UIntToStr(C: cardinal): string;
begin
  Result := IntToStr(C);
end;

function StrToUInt(S: string): cardinal;
var E: integer;
begin
  Val(S, Result, E);
  if E <> 0 then raise EConvertError.Create(S + ' is not valid cardinal value');
end;

function StrToUIntDef(S: string; DefVal: cardinal = 0): cardinal;
var E: integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := DefVal;
end;

Procedure FieldMapping(FieldType : cardinal; phSize : Integer; var BdeType : Word;
                var BdeSubType : Word; var LogSize : Integer;
                var LocArray : Boolean);
begin
  BdeType    := fldUNKNOWN;
  BdeSubType := 0;
  LogSize    := 0;
  LocArray := GetInArrayField(FieldType);
  Case FieldType of
    FIELD_TYPE_BOOL: begin
                        BdeType := fldBOOL;
                        LogSize := SizeOf(SmallInt);
                     end;
    FIELD_TYPE_BPCHAR,
    FIELD_TYPE_CHAR,
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_TINTERVAL:
                     begin
                        BdeType := fldZSTRING;
                        LogSize   := phSize+1;
                        if FieldType = FIELD_TYPE_BPCHAR then
                           BdeSubType := fldstFIXED;
                     end;
    FIELD_TYPE_OID,
    FIELD_TYPE_BYTEA:  begin
                        BdeType := fldBLOB;
                        LogSize := SizeOf(TBlobItem);
                     end;
    FIELD_TYPE_TEXT: begin
                        BdeType := fldBLOB;
                        LogSize := SizeOf(TBlobItem);
                        BdeSubType := fldstMemo;
                     end;
    FIELD_TYPE_INT2: begin
                        BDEType := fldINT16;
                        LogSize := Sizeof(SmallInt);
                      end;
    FIELD_TYPE_INT4:  begin
                        BDEType := fldINT32;
                        LogSize := Sizeof(LongInt);
                      end;
    FIELD_TYPE_INT8:  begin
                         BDEType := fldINT64;
                         LogSize := Sizeof(Int64);
                      end;
    FIELD_TYPE_DATE:  begin
                         BdeType := fldDATE;
                         LogSize := Sizeof(TTimeStamp);
                      end;
    FIELD_TYPE_TIME:  begin
                         BdeType := fldTIME;
                         LogSize := Sizeof(TDateTime);
                      end;

    FIELD_TYPE_TIMESTAMP:
                      begin
                         BdeType := fldTIMESTAMP;
                         LogSize := SizeOf(TDateTime);
                      end;
    FIELD_TYPE_FLOAT4,
    FIELD_TYPE_NUMERIC,
    FIELD_TYPE_FLOAT8:
                      begin
                         BdeType := fldFLOAT;
                         LogSize := Sizeof(Double);
                      end;
    FIELD_TYPE_MONEY: begin
                         BdeType := fldZSTRING;
                         //BdeSubType := fldstMONEY;
                         LogSize := 32; //Sizeof(Single);
                      end;
    FIELD_TYPE_NAME: begin
                         BdeType := fldZSTRING;
                         LogSize := NAMEDATALEN + 1;
                      end;
    FIELD_TYPE_TIMETZ: begin
                         BdeType := fldZSTRING;
                         LogSize := TIMETZLEN + 1;
                      end;
    FIELD_TYPE_TIMESTAMPTZ: begin
                         BdeType := fldZSTRING;
                         LogSize := TIMESTAMPTZLEN + 1;
                      end;
    FIELD_TYPE_BIT:   begin
                         BdeType := fldZSTRING;
                         LogSize := phSize + 1;
                      end;
    FIELD_TYPE_UUID:   begin
                         BdeType := fldUUID;
                         LogSize := UUIDLEN + 1;
                      end;
    FIELD_TYPE_INET,
    FIELD_TYPE_CIDR:  begin
                         BdeType := fldZSTRING;
                         LogSize := INETLEN + 1;
                      end;
    FIELD_TYPE_MACADDR: begin
                         BdeType := fldZSTRING;
                         LogSize := MACADDRLEN + 1;
                      end;
    FIELD_TYPE_POINT:
                      begin
                         BdeType := fldPOINT;
                         LogSize := SizeOf(TPSQLPoint);
                      end;
    FIELD_TYPE_CIRCLE:
                      begin
                         BdeType := fldCIRCLE;
                         LogSize := SizeOf(TPSQLCircle);
                      end;
    FIELD_TYPE_BOX:
                      begin
                         BdeType := fldBOX;
                         LogSize := SizeOf(TPSQLBox);
                      end;
    FIELD_TYPE_LSEG:
                      begin
                         BdeType := fldLSEG;
                         LogSize := SizeOf(TPSQLLSeg);
                      end;
  else
    begin
       BdeType := fldZSTRING;
       LogSize := phSize+1;
    end;
  end;
end;

Procedure ConverPSQLtoDelphiFieldInfo(Info : TPGFIELD_INFO;
      Count, Offset : Word;
      var RecBuff : FLDDesc;
      var ValChk : VCHKDesc;
      var LocArray : Boolean);
var
  LogSize : Integer;
  dataLen : Integer;
//  i       : Integer;
begin
  ZeroMemory(@RecBuff, Sizeof(FLDDesc));
  ZeroMemory(@ValChk, SizeOf(VCHKDesc));
  with RecBuff do
  begin
    iFldNum  := Count;
    ValChk.iFldNum := Count;
    DataLen := Info.FieldMaxSize;
    FieldMapping(Info.FieldType, DataLen, iFldType, iSubType, LogSize, LocArray);
    if (Info.Fieldtype = FIELD_TYPE_FLOAT4) or (Info.Fieldtype = FIELD_TYPE_FLOAT8) or
       (Info.Fieldtype = FIELD_TYPE_NUMERIC) then
    begin
      iUnits1  := 32;
      iUnits2  := Hi(LogSize);
      iLen     := Lo(LogSize);
    end
    else
    begin
      if iFldType = fldZSTRING then
         iUnits1  := LogSize-1 else
         iUnits1  := LogSize;
      iUnits2  := 0;
      iLen     := LogSize;
    end;
    if (iFldType = fldINT32) and (Pos('nextval(', Info.FieldDefault) > 0)  then iSubType := fldstAUTOINC;
    iOffset := Offset;
    efldvVchk := fldvUNKNOWN;
    if Info.FieldDefault <> '' then ValChk.bHasDefVal := True;
    ValChk.aDefVal := Info.FieldDefault;
    szName := Info.FieldName;
  end;
end;

Procedure LoadPSQLLibrary(LibPQPath: string = '');

  function GetPSQLProc( ProcName : string ) : pointer;
  begin
    Result := GetProcAddress( SQLLibraryHandle, PChar(ProcName));
    {$IFDEF M_DEBUG}
    if not Assigned(Result) then
     LogDebugMessage('PROC', Format('No entry address for procedure <b>"%s"</b>', [ProcName]));
    {$ENDIF}

  end;

begin
   if LibPQPath = EmptyStr then LibPQPath := PSQL_DLL;
   if ( SQLLibraryHandle <= HINSTANCE_ERROR ) then
   begin
      SQLLibraryHandle := LoadLibrary(PChar(LibPQPath));
      if ( SQLLibraryHandle > HINSTANCE_ERROR ) then
      begin
         @PQconnectdb    := GetPSQLProc('PQconnectdb');
         @PQconnectdbParams := GetPSQLProc('PQconnectdbParams');
         @PQping         := GetPSQLProc('PQping');
         @PQpingParams   := GetPSQLProc('PQpingParams');
         @PQconnectPoll  := GetPSQLProc('PQconnectPoll');
         @PQconnectStart := GetPSQLProc('PQconnectStart');
         @PQsetdbLogin   := GetPSQLProc('PQsetdbLogin');
         @PQconndefaults := GetPSQLProc('PQconndefaults');
         @PQfinish       := GetPSQLProc('PQfinish');
         @PQreset        := GetPSQLProc('PQreset');
         @PQrequestCancel := GetPSQLProc('PQrequestCancel');
         @PQdb           := GetPSQLProc('PQdb');
         @PQuser         := GetPSQLProc('PQuser');
         @PQpass         := GetPSQLProc('PQpass');
         @PQhost         := GetPSQLProc('PQhost');
         @PQport         := GetPSQLProc('PQport');
         @PQtty          := GetPSQLProc('PQtty');
         @PQoptions      := GetPSQLProc('PQoptions');
         @PQstatus       := GetPSQLProc('PQstatus');
         @PQerrorMessage := GetPSQLProc('PQerrorMessage');
         @PQsocket       := GetPSQLProc('PQsocket');
         @PQbackendPID   := GetPSQLProc('PQbackendPID');
         @PQparameterStatus := GetPSQLProc('PQparameterStatus');
         @PQserverVersion:= GetPSQLProc('PQserverVersion');
         @PQtrace        := GetPSQLProc('PQtrace');
         @PQuntrace      := GetPSQLProc('PQuntrace');
         @PQsetNoticeProcessor := GetPSQLProc('PQsetNoticeProcessor');
         @PQexecPrepared := GetPSQLProc('PQexecPrepared');
         @PQprepare      := GetPSQLProc('PQprepare');
         @PQexec         := GetPSQLProc('PQexec');
         @PQexecParams   := GetPSQLProc('PQexecParams');
         @PQnotifies     := GetPSQLProc('PQnotifies');
         @PQsendQuery    := GetPSQLProc('PQsendQuery');
         @PQgetResult    := GetPSQLProc('PQgetResult');
         @PQisBusy       := GetPSQLProc('PQisBusy');
         @PQconsumeInput := GetPSQLProc('PQconsumeInput');
         @PQgetline      := GetPSQLProc('PQgetline');
         @PQputline      := GetPSQLProc('PQputline');
         @PQgetlineAsync := GetPSQLProc('PQgetlineAsync');
         @PQputnbytes    := GetPSQLProc('PQputnbytes');
         @PQendcopy      := GetPSQLProc('PQendcopy');
         @PQgetCopyData  := GetPSQLProc('PQgetCopyData');
         @PQputCopyData  := GetPSQLProc('PQputCopyData');
         @PQputCopyEnd   := GetPSQLProc('PQputCopyEnd');
         @PQresultStatus := GetPSQLProc('PQresultStatus');
         @PQresultErrorMessage := GetPSQLProc('PQresultErrorMessage');
         @PQresultErrorField   := GetPSQLProc('PQresultErrorField');
         @PQntuples      := GetPSQLProc('PQntuples');
         @PQnfields      := GetPSQLProc('PQnfields');
         @PQbinaryTuples := GetPSQLProc('PQbinaryTuples');
         @PQfname        := GetPSQLProc('PQfname');
         @PQfnumber      := GetPSQLProc('PQfnumber');
         @PQftype        := GetPSQLProc('PQftype');
         @PQftable       := GetPSQLProc('PQftable');
         @PQftablecol    := GetPSQLProc('PQftablecol');
         @PQfsize        := GetPSQLProc('PQfsize');
         @PQfmod         := GetPSQLProc('PQfmod');
         @PQcmdStatus    := GetPSQLProc('PQcmdStatus');
         @PQoidValue     := GetPSQLProc('PQoidValue');
         @PQoidStatus    := GetPSQLProc('PQoidStatus');
         @PQcmdTuples    := GetPSQLProc('PQcmdTuples');
         @PQgetvalue     := GetPSQLProc('PQgetvalue');
         @PQsetvalue     := GetPSQLProc('PQsetvalue');
         @PQcopyResult   := GetPSQLProc('PQcopyResult');
         @PQgetlength    := GetPSQLProc('PQgetlength');
         @PQgetisnull    := GetPSQLProc('PQgetisnull');
         @PQclear        := GetPSQLProc('PQclear');
         @PQmakeEmptyPGresult := GetPSQLProc('PQmakeEmptyPGresult');
         @PQtransactionStatus := GetPSQLProc('PQtransactionStatus');
         @PQEscapeByteaConn  := GetPSQLProc('PQescapeByteaConn');
         @PQUnEscapeBytea:= GetPSQLProc('PQunescapeBytea');
         @PQEscapeStringConn:=GetPSQLProc('PQescapeStringConn');
         @PQFreeMem      := GetPSQLProc('PQfreemem');
         @PQsetClientEncoding := GetPSQLProc('PQsetClientEncoding');
         @PQsetErrorVerbosity := GetPSQLProc('PQsetErrorVerbosity');
         @PQclientEncoding := GetPSQLProc('PQclientEncoding');
         @PQgetssl := GetPSQLProc('PQgetssl');
         @pg_encoding_to_char := GetPSQLProc('pg_encoding_to_char');
         @lo_open        := GetPSQLProc('lo_open');
         @lo_close       := GetPSQLProc('lo_close');
         @lo_read        := GetPSQLProc('lo_read');
         @lo_write       := GetPSQLProc('lo_write');
         @lo_lseek       := GetPSQLProc('lo_lseek');
         @lo_creat       := GetPSQLProc('lo_creat');
         @lo_tell        := GetPSQLProc('lo_tell');
         @lo_unlink      := GetPSQLProc('lo_unlink');
         @lo_import      := GetPSQLProc('lo_import');
         @lo_export      := GetPSQLProc('lo_export');
      end
     else
      CheckLibraryLoaded();
      {$IFDEF M_DEBUG}
       LogDebugMessage('LIB', GetModuleName(GetModuleHandle(PChar(LIBEAY_DLL))));
       LogDebugMessage('LIB', GetModuleName(GetModuleHandle(PChar(SSLEAY_DLL))));
       LogDebugMessage('LIB', GetModuleName(SQLLibraryHandle));
      {$ENDIF}
   end;
end;

Procedure UnloadPSQLLibrary;
begin
  if ( SQLLibraryHandle > HINSTANCE_ERROR ) then
     FreeLibrary( SQLLibraryHandle );
  SQLLibraryHandle := HINSTANCE_ERROR;
end;

Procedure CheckLibraryLoaded;
begin
  if SQLLibraryHandle <= HINSTANCE_ERROR then
      raise EFileNotFoundException.CreateFmt('Error loading client library "%s"', [PSQL_DLL]);
end;

function MaskSearch(const Str, Mask: string;
                    CaseSensitive : boolean = true;
                    MaskChar: Char = '?';
                    WildCard: Char = '%'): Boolean;//mi:2006-09-07
var
  S, M : PChar;
  W : PChar; //mi:2007-06-20 last wildcard position in mask
begin
  Result := false;
  if CaseSensitive then
  begin
    S := PChar(Str);
    M := PChar(Mask);
  end
  else
  begin
    S := PChar(AnsiUpperCase(Str));
    M := PChar(AnsiUpperCase(Mask));
  end;

  W := nil;

  while true do
  begin
    if (S^ = #0) or (M^ = #0) then//we have an end of one of strings
    begin
      //mi:2007-10-14 there can be some more wildcard chars in mask
      while (M^ = WildCard) do
        inc(M);

      if S^ = M^ then //both are #0, it seems that we have a match or both strings are empty
        Result := true;
      exit;
    end;

    if (M^ <> MaskChar) and (M^ <> WildCard) then
    begin
      if M^ = S^ then
      begin
        Inc(M); Inc(S); //move to the next character
        continue;
      end
      else//character are not equal
      begin
        if W <> nil then//there was a wildcard before, we need to rollback mask to it to continue search
        begin
          M := W;
          Inc(S);
          continue;
        end
        else //there were no wildcards before, string doesn't match
          exit;
      end;
    end
    else if (M^ = MaskChar) then
    begin
      Inc(M); Inc(S); //move to the next character
      continue;
    end
    else if (M^ = WildCard) then
    begin
      W := M;
      while (S^ <> (M+1)^) and (S^ <> #0) do
        Inc(S);
      Inc(M);
    end;
  end;
end;

function Search(Op1,Op2 : Variant; OEM, CaseSen : Boolean; PartLen: Integer):Boolean;
var
  S1,S2 : String;
begin
   if CaseSen then //case insensitive
   begin
      Op1 := AnsiUpperCase(Op1);
      Op2 := AnsiUpperCase(Op2);
   end;
   S1 := Op1;
   S2 := Op2;

{$IFDEF MSWINDOWS}
  if OEM then
  begin
    {$IFDEF DELPHI_12}
    OemToCharBuff(PAnsiChar(AnsiString(S1)), PWideChar(S1), Length(S1));
    OemToCharBuff(PAnsiChar(AnsiString(S2)), PWideChar(S2), Length(S2));
    {$ELSE}
        {$IFNDEF FPC}
        OemToCharBuff(PAnsiChar(S1), PAnsiChar(S1), Length(S1));
        OemToCharBuff(PAnsiChar(S2), PAnsiChar(S2), Length(S2));
        {$ENDIF}
    {$ENDIF}
  end;
{$ENDIF}

   if CaseSen then //case insensitive
   begin
      if PartLen = 0 then
         Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0 else  // Full len
         Result := AnsiStrLIComp(PChar(S1), PChar(S2), PartLen) = 0; //Part len
   end else
   begin
      if PartLen = 0 then
         Result := AnsiStrComp(PChar(S1), PChar(S2)) = 0 else  // Full len
         Result := AnsiStrLComp(PChar(S1), PChar(S2), PartLen) = 0; //Part len
   end;
end;

function GetBDEErrorMessage(ErrorCode : Word):String;
begin
   case ErrorCode of
      DBIERR_BOF: Result :='At beginning of table.';               //8705
      DBIERR_EOF: Result :='At end of table.';               //8706
      DBIERR_NOCURRREC: Result :='No current record.';         //8709
      DBIERR_RECNOTFOUND: Result :='Could not find record.';       //8710
      DBIERR_ENDOFBLOB: Result :='End of BLOB.';         //8711
      DBIERR_INVALIDPARAM: Result :='Invalid parameter.';      //9986
      DBIERR_INVALIDHNDL: Result :='Invalid handle to the function.';       //9990
      DBIERR_NOSUCHINDEX: Result :='Index does not exist.';       //9997
      DBIERR_INVALIDBLOBOFFSET: Result :='Invalid offset into the BLOB.'; //9998
      DBIERR_INVALIDRECSTRUCT: Result :='Invalid record structure.';  //10003
      DBIERR_NOSUCHTABLE: Result :='Table does not exist.';       //10024
      DBIERR_NOSUCHFILTER: Result :='Filter handle is invalid.';      //10050
      DBIERR_NOTSUFFTABLERIGHTS: Result :='Insufficient table rights for operation. Password required.';//10498
      DBIERR_NOTABLOB: Result :='Field is not a BLOB.';          //10753
      DBIERR_TABLEREADONLY: Result :='Table is read only.';     //10763
      DBIERR_NOASSOCINDEX: Result :='No associated index.';      //10764
      DBIERR_QRYEMPTY: Result :='Query string is empty.';          //11886
      DBIERR_NOTSUPPORTED: Result :='Capability not supported.';      //12289
      DBIERR_UPDATEABORT: Result :='Update aborted.';       //13062
   end;
end;


initialization
  SQLLibraryHandle := HINSTANCE_ERROR;

finalization
  UnloadPSQLLibrary;

end.

