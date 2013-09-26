{$I pSQLDAC.inc}
{ *************************************************************************** }
{                                                                             }
{ Kylix and Delphi Cross-Platform Visual Component Library                    }
{                                                                             }
{ Copyright (c) 1995, 2001 Borland Software Corporation                       }
{                                                                             }
{ *************************************************************************** }


unit PSQLCommon;


{SVN revision: $Id$}

{$T-,H+,X+,R-}

interface

uses Windows, Classes,
     {$IFDEF DELPHI_6}Variants, SqlTimSt,{$ENDIF}
     {$IFDEF FPC}Variants, {$ENDIF}
      DB;

type
  TCANOperator = (
    coNOTDEFINED,                      {                                   }
    coISBLANK,                         { coUnary;  is operand blank.      }
    coNOTBLANK,                        { coUnary;  is operand not blank.  }
    coEQ,                              { coBinary, coCompare; equal.     }
    coNE,                              { coBinary; NOT equal.             }
    coGT,                              { coBinary; greater than.          }
    coLT,                              { coBinary; less than.             }
    coGE,                              { coBinary; greater or equal.      }
    coLE,                              { coBinary; less or equal.         }
    coNOT,                             { coUnary; NOT                     }
    coAND,                             { coBinary; AND                    }
    coOR,                              { coBinary; OR                     }
    coTUPLE2,                          { coUnary; Entire record is operand. }
    coFIELD2,                          { coUnary; operand is field        }
    coCONST2,                          { coUnary; operand is constant     }
    coMINUS,                           { coUnary;  minus. }
    coADD,                             { coBinary; addition. }
    coSUB,                             { coBinary; subtraction. }
    coMUL,                             { coBinary; multiplication. }
    coDIV,                             { coBinary; division. }
    coMOD,                             { coBinary; modulo division. }
    coREM,                             { coBinary; remainder of division. }
    coSUM,                             { coBinary, accumulate sum of. }
    coCOUNT,                           { coBinary, accumulate count of. }
    coMIN,                             { coBinary, find minimum of. }
    coMAX,                             { coBinary, find maximum of. }
    coAVG,                             { coBinary, find average of. }
    coCONT,                            { coBinary; provides a link between two }
    coUDF2,                            { coBinary; invokes a User defined fn }
    coCONTINUE2,                       { coUnary; Stops evaluating records }
    coLIKE,                            { coCompare, extended binary compare        }
    coIN,                              { coBinary field in list of values }
    coLIST2,                           { List of constant values of same type }
    coUPPER,                           { coUnary: upper case }
    coLOWER,                           { coUnary: lower case }
    coFUNC2,                           { coFunc: Function }
    coLISTELEM2,                       { coListElem: List Element }
    coASSIGN                           { coBinary: Field assignment }
  );

  NODEClass = (                         { Node Class }
    nodeNULL,                           { Null node                   }
    nodeUNARY,                          { Node is a unary             }
    nodeBINARY,                         { Node is a binary            }
    nodeCOMPARE,                        { Node is a compare           }
    nodeFIELD,                          { Node is a field             }
    nodeCONST,                          { Node is a constant          }
    nodeTUPLE,                          { Node is a record }
    nodeCONTINUE,                       { Node is a continue node     }
    nodeUDF,                            { Node is a UDF node }
    nodeLIST,                           { Node is a LIST node }
    nodeFUNC,                           { Node is a Function node }
    nodeLISTELEM                        { Node is a List Element node }
  );

const
  CANEXPRSIZE        = 10; { SizeOf(CANExpr) }
  CANHDRSIZE         = 8;  { SizeOf(CANHdr) }
  CANEXPRVERSION     = 2;


type
  TExprData = array of Byte;
  TFieldMap = array[TFieldType] of Byte;

  {$IFDEF FPC}
    TBlobByteData = array of Byte;
  {$ENDIF}

{ TFilterExpr }

type

  TParserOption = (poExtSyntax, poAggregate, poDefaultExpr, poUseOrigNames,
                   poFieldNameGiven, poFieldDepend);
  TParserOptions = set of TParserOption;

  TExprNodeKind = (enField, enConst, enOperator, enFunc);
  TExprScopeKind = (skField, skAgg, skConst);

  PExprNode = ^TExprNode;
  TExprNode = record
    FNext: PExprNode;
    FKind: TExprNodeKind;
    FPartial: Boolean;
    FOperator: TCANOperator;
    FData: Variant;
    FLeft: PExprNode;
    FRight: PExprNode;
    FDataType: TFieldType;
    FDataSize: Integer;
    FArgs: TList;
    FScopeKind: TExprScopeKind;
  end;

  TFilterExpr = class
  private
    FDataSet: TDataSet;
    FFieldMap: TFieldMap;
    FOptions: TFilterOptions;
    FParserOptions: TParserOptions;
    FNodes: PExprNode;
    FExprBuffer: TExprData;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    FFieldName: string;
    FDependentFields: TBits;
    function FieldFromNode(Node: PExprNode): TField;
    function GetExprData(Pos, Size: Integer): PChar;
{$IFNDEF FPC}
    function PutConstBCD(const Value: Variant; Decimals: Integer): Integer;
{$ENDIF}
    function PutConstBool(const Value: Variant): Integer;
    function PutConstDate(const Value: Variant): Integer;
    function PutConstDateTime(const Value: Variant): Integer;
    function PutConstFloat(const Value: Variant): Integer;
    function PutConstInt(DataType: TFieldType; const Value: Variant): Integer;

{$IFDEF DELPHI_6}
    function PutConstSQLTimeStamp(const Value: Variant): Integer;
    function PutConstInt64(DataType: TFieldType; const Value: Variant): Integer;
    function PutConstFMTBCD(const Value: Variant; Decimals: Integer): Integer;
{$ENDIF}

    function PutConstNode(DataType: TFieldType; Data: PAnsiChar;
      Size: Integer): Integer;
    function PutConstStr(const Value: string): Integer;
    function PutConstTime(const Value: Variant): Integer;
    function PutData(Data: PAnsiChar; Size: Integer): Integer;
    function PutExprNode(Node: PExprNode; ParentOp: TCANOperator): Integer;
    function PutFieldNode(Field: TField; Node: PExprNode): Integer;
    function PutNode(NodeType: NodeClass; OpType: TCANOperator;
      OpCount: Integer): Integer;
    procedure SetNodeOp(Node, Index, Data: Integer);
    function PutConstant(Node: PExprNode): Integer;
    function GetFieldByName(Name: string) : TField;
  public
    constructor Create(DataSet: TDataSet; Options: TFilterOptions;
      ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits;
      FieldMap: TFieldMap);
    destructor Destroy; override;
    function NewCompareNode(Field: TField; Operator: TCANOperator;
      const Value: Variant): PExprNode;
    function NewNode(Kind: TExprNodeKind; Operator: TCANOperator;
      const Data: Variant; Left, Right: PExprNode): PExprNode;
    function GetFilterData(Root: PExprNode): TExprData;
    property DataSet: TDataSet write FDataSet;
  end;

{ TExprParser }

  TExprToken = (etEnd, etSymbol, etName, etLiteral,  etLParen, etRParen,
    etEQ, etNE, etGE, etLE, etGT, etLT, etADD, etSUB, etMUL, etDIV,
    etComma, etLIKE, etISNULL, etISNOTNULL, etIN);

  TExprParser = class
  private
    FDecimalSeparator: Char;
    FFilter: TFilterExpr;
    FFieldMap: TFieldMap;
    FText: string;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FStrTrue: string;
    FStrFalse: string;
    FToken: TExprToken;
    FPrevToken: TExprToken;
    FFilterData: TExprData;
    FNumericLit: Boolean;
    FDataSize: Integer;
    FParserOptions: TParserOptions;
    FFieldName: string;
    FDataSet: TDataSet;
    FDependentFields: TBits;
    procedure NextToken;
    function NextTokenIsLParen : Boolean;
    function ParseExpr: PExprNode;
    function ParseExpr2: PExprNode;
    function ParseExpr3: PExprNode;
    function ParseExpr4: PExprNode;
    function ParseExpr5: PExprNode;
    function ParseExpr6: PExprNode;
    function ParseExpr7: PExprNode;
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
    function TokenSymbolIsFunc(const S: string) : Boolean;
    procedure GetFuncResultInfo(Node: PExprNode);
    procedure TypeCheckArithOp(Node: PExprNode);
    procedure GetScopeKind(Root, Left, Right : PExprNode);
  public
    constructor Create(DataSet: TDataSet; const Text: string;
      Options: TFilterOptions; ParserOptions: TParserOptions;
      const FieldName: string; DepFields: TBits; FieldMap: TFieldMap);
    destructor Destroy; override;
    procedure SetExprParams(const Text: string; Options: TFilterOptions;
      ParserOptions: TParserOptions; const FieldName: string);
    property FilterData: TExprData read FFilterData;
    property DataSize: Integer read FDataSize;
  end;

{ Field Origin parser }

type
  TFieldInfo = record
    DatabaseName: string;
    TableName: string;
    OriginalFieldName: string;
  end;

function GetFieldInfo(const Origin: string; var FieldInfo: TFieldInfo): Boolean;

{ SQL Parser }

type
  TSQLToken = (stUnknown, stTableName, stFieldName, stAscending, stDescending, stSelect,
    stFrom, stWhere, stGroupBy, stHaving, stUnion, stPlan, stOrderBy, stForUpdate,
    stEnd, stPredicate, stValue, stIsNull, stIsNotNull, stLike, stAnd, stOr,
    stNumber, stAllFields, stComment, stDistinct);

const
  SQLSections = [stSelect, stFrom, stWhere, stGroupBy, stHaving, stUnion,
    stPlan, stOrderBy, stForUpdate];

function NextSQLToken(var p: PChar; out Token: string; CurSection: TSQLToken): TSQLToken;
function GetIndexForOrderBy(const SQL: string; DataSet: TDataSet): TIndexDef;
function GetTableNameFromSQL(const SQL: string): string;
function GetTableNameFromQuery(const SQL: string): string;
function AddParamSQLForDetail(Params: TParams; SQL: string; Native: Boolean; QuoteChar: string = ''): string;
function IsMultiTableQuery(const SQL: string): Boolean;

resourcestring
  SInvalidFieldSize = 'Invalid field size';
  SInvalidFieldKind = 'Invalid FieldKind';
  SInvalidFieldRegistration = 'Invalid field registration';
  SUnknownFieldType = 'Field ''%s'' is of an unknown type';
  SFieldNameMissing = 'Field name missing';
  SDuplicateFieldName = 'Duplicate field name ''%s''';
  SFieldNotFound = 'Field ''%s'' not found';
  SFieldAccessError = 'Cannot access field ''%s'' as type %s';
  SFieldValueError = 'Invalid value for field ''%s''';
  SFieldRangeError = '%g is not a valid value for field ''%s''. The allowed range is %g to %g';
  SBcdFieldRangeError = '%s is not a valid value for field ''%s''. The allowed range is %s to %s';
  SInvalidIntegerValue = '''%s'' is not a valid integer value for field ''%s''';
  SInvalidBoolValue = '''%s'' is not a valid boolean value for field ''%s''';
  SInvalidFloatValue = '''%s'' is not a valid floating point value for field ''%s''';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  SInvalidVarByteArray = 'Invalid variant type or size for field ''%s''';
  SFieldOutOfRange = 'Value of field ''%s'' is out of range';
//  SBCDOverflow = '(Overflow)';
  SFieldRequired = 'Field ''%s'' must have a value';
  SDataSetMissing = 'Field ''%s'' has no dataset';
  SInvalidCalcType = 'Field ''%s'' cannot be a calculated or lookup field';
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SFieldIndexError = 'Field index out of range';
  SNoFieldIndexes = 'No index currently active';
  SNotIndexField = 'Field ''%s'' is not indexed and cannot be modified';
  SIndexFieldMissing = 'Cannot access index field ''%s''';
  SDuplicateIndexName = 'Duplicate index name ''%s''';
  SNoIndexForFields = 'No index for fields ''%s''';
  SIndexNotFound = 'Index ''%s'' not found';
  SDuplicateName = 'Duplicate name ''%s'' in %s';
  SCircularDataLink = 'Circular datalinks are not allowed';
  SLookupInfoError = 'Lookup information for field ''%s'' is incomplete';
  SNewLookupFieldCaption = 'New Lookup Field';
  SDataSourceChange = 'DataSource cannot be changed';
  SNoNestedMasterSource = 'Nested datasets cannot have a MasterSource';
  SDataSetOpen = 'Cannot perform this operation on an open dataset';
  SNotEditing = 'Dataset not in edit or insert mode';
  SDataSetClosed = 'Cannot perform this operation on a closed dataset';
  SDataSetEmpty = 'Cannot perform this operation on an empty dataset';
  SDataSetReadOnly = 'Cannot modify a read-only dataset';
  SNestedDataSetClass = 'Nested dataset must inherit from %s';
  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoLParen = '''('' expected but %s found';
  SExprNoRParen = ''')'' expected but %s found';
  SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprRangeError = 'Constant out of range';
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprNothing = 'nothing';
  SExprTypeMis = 'Type mismatch in expression';
  SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
  SExprNoArith = 'Arithmetic in filter expressions not supported';
  SExprNotAgg = 'Expression is not an aggregate expression';
  SExprBadConst = 'Constant is not correct type %s';
  SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
  SExprEmptyInList = 'IN predicate list may not be empty';
  SInvalidKeywordUse = 'Invalid use of keyword';
  STextFalse = 'False';
  STextTrue = 'True';
  SParameterNotFound = 'Parameter ''%s'' not found';
  SInvalidVersion = 'Unable to load bind parameters';
  SParamTooBig = 'Parameter ''%s'', cannot save data larger than %d bytes';
  SBadFieldType = 'Field ''%s'' is of an unsupported type';
  SAggActive = 'Property may not be modified while aggregate is active';
  SProviderSQLNotSupported = 'SQL not supported: %s';
  SProviderExecuteNotSupported = 'Execute not supported: %s';
  SExprNoAggOnCalcs = 'Field ''%s'' is not the correct type of calculated field to be used in an aggregate, use an internalcalc';
  SRecordChanged = 'Record not found or changed by another user';
  SDataSetUnidirectional = 'Operation not allowed on a unidirectional dataset';
  SUnassignedVar = 'Unassigned variant value';
  SRecordNotFound = 'Record not found';
  SFileNameBlank = 'FileName property cannot be blank';
  SFieldNameTooLarge = 'Fieldname %s exceeds %d chars';

{ For FMTBcd }

  SBcdOverflow = 'BCD overflow';
  SInvalidBcdValue = '%s is not a valid BCD value';
  SInvalidFormatType = 'Invalid format type for BCD';

{ For SqlTimSt }

  SCouldNotParseTimeStamp = 'Could not parse SQL TimeStamp string';
  SInvalidSqlTimeStamp = 'Invalid SQL date/time values';

  SDeleteRecordQuestion = 'Delete record?';
  SDeleteMultipleRecordsQuestion = 'Delete all selected records?';
  STooManyColumns = 'Grid requested to display more than 256 columns';

  { For reconcile error }
  SSkip = 'Skip';
  SAbort = 'Abort';
  SMerge = 'Merge';
  SCorrect = 'Correct';
  SCancel  = 'Cancel';
  SRefresh = 'Refresh';
  SModified = 'Modified';
  SInserted = 'Inserted';
  SDeleted  = 'Deleted';
  SCaption = 'Update Error - %s';
  SUnchanged = '<Unchanged>';
  SBinary = '(Binary)';
  SAdt = '(ADT)';
  SArray = '(Array)';
  SFieldName = 'Field Name';
  SOriginal = 'Original Value';
  SConflict = 'Conflicting Value';
  SValue = ' Value';
  SNoData = '<No Records>';
  SNew = 'New';

implementation

uses SysUtils,
     {$IFNDEF FPC}
        {$IFDEF DELPHI_6}FMTBcd,{$ENDIF}
     {$ENDIF}
     PSQLTypes;

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
    if AnsiCompareText('SELECT', Str) = 0 then
      Result := stSelect else
    if AnsiCompareText('AND', Str) = 0 then
      Result := stAnd else
    if AnsiCompareText('OR', Str) = 0 then
      Result := stOr else
    if AnsiCompareText('LIKE', Str) = 0 then
      Result := stLike else
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
    if CurSection = stFrom then
      Result := stTableName else
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
        repeat Inc(p) until CharInSet(p^, [Literal,#0]);
        if p^ = #0 then
        begin
          p := Mark;
          Inc(p);
        end else
        begin
          Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Mark := PChar(Token);
          Token := AnsiExtractQuotedStr(Mark, Literal);
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
        if CharInSet(p^, ['/','*']) then
        begin
          if p^ = '*' then
          begin
            repeat Inc(p) until (p = #0) or ((p^ = '*') and (p[1] = '/'));
          end else
            while not CharInSet(p^, [#0, #10, #13]) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      ' ', #10, #13, ',', '(':
      begin
        if Assigned(TokenStart) then
        begin
          SetString(Token, TokenStart, p - TokenStart);
          Result := GetSQLToken(Token);
          Exit;
        end else
          while CharInSet(p^, [' ', #10, #13, ',', '(']) do Inc(p);
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

function AddParamSQLForDetail(Params: TParams; SQL: string; Native: Boolean; QuoteChar: string = ''): string;
const
  SWhere = ' where ';     { do not localize }
  SAnd = ' and ';         { do not localize }

  function GenerateParamSQL: string;  
  var
    I: Integer;
    ParamName: string;
  begin
    for I := 0 to Params.Count -1 do
    begin
      if QuoteChar = '"' then
        ParamName := '"' + StringReplace(Params[I].Name, '"', '""', [rfReplaceAll] ) + '"'
      else
        ParamName := QuoteChar + Params[I].Name +QuoteChar;
      if I > 0 then Result := Result + SAnd;
      if Native then
        Result := Result + format('%s = ?', [ParamName])
      else
        Result := Result + format('%s = :%s', [ParamName, ParamName]);
    end;
    if pos(SWhere, LowerCase(Result)) > 0 then
      Result := SAnd + Result
    else
      Result := SWhere + Result;
  end;

  function AddWhereClause: string;
  var
    Start: PChar;
    Rest, FName: string;
    SQLToken, CurSection: TSQLToken;
  begin
    Start := PChar(SQL);
    CurSection := stUnknown;
    repeat
      SQLToken := NextSQLToken(Start, FName, CurSection);
    until SQLToken in [stFrom, stEnd];
    if SQLToken = stFrom then
      NextSQLToken(Start, FName, CurSection);
    Rest := string(Start);
    if Rest = '' then
      Result := SQL + ' ' + GenerateParamSQL
    else
      Result := Copy(SQL, 1, pos(Rest, SQL)) + ' ' + GenerateParamSQL + Rest;
  end;

begin
  Result := SQL;
  if (Params.Count > 0) then
    Result := AddWhereClause;
end;

// SQL might be a direct tablename;
function GetTableNameFromQuery(const SQL: string): string;
begin
  if pos( 'select', lowercase(SQL) ) < 1 then
    Result := SQL
  else
    Result := GetTableNameFromSQL(SQL);
end;

function GetTableNameFromSQL(const SQL: string): string;
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
      // stValue is returned if TableNames contain quote chars.
      if (SQLToken = stTableName) or (SQLToken = stValue) then
      begin
        Result := Token;
        while (Start[0] = '.') and not (SQLToken in [stEnd]) do
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          Result := Result + '.' + Token;
        end;
        Exit;
      end;
    until (CurSection <> stFrom) or (SQLToken in [stEnd, stTableName]);
  end;
end;

function IsMultiTableQuery(const SQL: string): Boolean;
const
  SInnerJoin = 'inner join ';       { do not localize }
  SOuterJoin = 'outer join ';       { do not localize }
var
  Start: PChar;
  SResult, Token: string;
  SQLToken, CurSection: TSQLToken;
begin
  SResult := '';
  Start := PChar(SQL);
  CurSection := stUnknown;
  Result := True;
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
      // stValue is returned if TableNames contain quote chars.
      if (SQLToken = stTableName) or (SQLToken = stValue) then
      begin
        SResult := Token;
        while (Start[0] = '.') and not (SQLToken in [stEnd]) do
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          SResult := SResult + '.' + Token;
        end;
        if (Start[0] = ',') or (Start[1] = ',') then
          exit;
        NextSqlToken(Start, Token, CurSection);
        if Assigned(AnsiStrPos(Start, PChar(SInnerJoin))) or
           Assigned(AnsiStrPos(Start, PChar(SOuterJoin))) then
          Exit;
        SQLToken := NextSqlToken(Start, Token, CurSection);
        if SQLToken = stTableName then
          Exit;
        Result := False;
        Exit;
      end;
    until (CurSection <> stFrom) or (SQLToken in [stEnd, stTableName]);
  end;
end;

function GetIndexForOrderBy(const SQL: string; DataSet: TDataSet): TIndexDef;

  function AddField(const Fields, NewField: string): string;
  begin
    Result := Fields;
    if Fields <> '' then
      Result := Fields + ';' + NewField else
      Result := NewField;
  end;

var
  Start: PChar;
  Token, LastField, SaveField: string;
  SQLToken, CurSection: TSQLToken;
  FieldIndex: Integer;
begin
  Result := nil;
  Start := PChar(SQL);
  CurSection := stUnknown;
  repeat
    SQLToken := NextSQLToken(Start, Token, CurSection);
    if SQLToken in SQLSections then CurSection := SQLToken;
  until SQLToken in [stEnd, stOrderBy];
  if SQLToken = stOrderBy then
  begin
    Result := TIndexDef.Create(nil);
    try
      LastField := '';
      repeat
        SQLToken := NextSQLToken(Start, Token, CurSection);
        if SQLToken in SQLSections then
          CurSection := SQLToken else
          case SQLToken of
            stTableName: ;
            stFieldName:
            begin
              LastField := Token;
              { Verify that we parsed a valid field name, not something like "UPPER(Foo)" }
              if not Assigned(Dataset.FindField(LastField)) then continue;
              Result.Fields := AddField(Result.Fields, LastField);
              SaveField := LastField;
            end;
            stAscending: ;
            stDescending:
              Result.DescFields := AddField(Result.DescFields, SaveField);
            stNumber:
            begin
              FieldIndex := StrToInt(Token);
              if DataSet.FieldCount >= FieldIndex then
                LastField := DataSet.Fields[FieldIndex - 1].FieldName else
              if DataSet.FieldDefs.Count >= FieldIndex then
                LastField := DataSet.FieldDefs[FieldIndex - 1].Name
              else
                { DB2 specific syntax "FETCH FIRST n ROWS ONLY" is blocked here,
                  so commenting out the following line }
                //SysUtils.Abort;
                continue;
              Result.Fields := AddField(Result.Fields, LastField);
            end;
          end;
      until (CurSection <> stOrderBy) or (SQLToken = stEnd);
    finally
      if Result.Fields = '' then
      begin
        Result.Free;
        Result := nil;
      end;  
    end;
  end;
end;

function GetFieldInfo(const Origin: string; var FieldInfo: TFieldInfo): Boolean;
var
  Current: PChar;
  Values: array[0..4] of string;
  I: Integer;

  function GetPChar(const S: string): PChar;
  begin
    if S <> '' then Result := PChar(Pointer(S)) else Result := '';
  end;

  procedure Split(const S: string);
  begin
    Current := PChar(Pointer(S));
  end;

  function NextItem: string;
  var
    C: PChar;
    I: PChar;
    Terminator: Char;
    Ident: array[0..1023] of Char;
  begin
    Result := '';
    C := Current;
    I := Ident;
    while CharInSet(C^, ['.',' ',#0]) do
      if C^ = #0 then Exit else Inc(C);
    Terminator := '.';
    if C^ = '"' then
    begin
      Terminator := '"';
      Inc(C);
    end;
    while not CharInSet(C^, [Terminator, #0]) do
    begin
      if CharInSet(C^, LeadBytes) then
      begin
        I^ := C^;
        Inc(C);
        Inc(I);
      end
      else if C^ = '\' then
      begin
        Inc(C);
        if CharInSet(C^, LeadBytes) then
        begin
          I^ := C^;
          Inc(C);
          Inc(I);
        end;
        if C^ = #0 then Dec(C);
      end;
      I^ := C^;
      Inc(C);
      Inc(I);
    end;
    SetString(Result, Ident, I - Ident);
    if (Terminator = '"') and (C^ <> #0) then Inc(C);
    Current := C;
  end;

  function PopValue: PChar;
  begin
    if I >= 0 then
    begin
      Result := GetPChar(Values[I]);
      Dec(I);
    end else Result := '';
  end;

begin
  Result := False;
  if (Origin = '') then Exit;
  Split(Origin);
  I := -1;
  repeat
    Inc(I);
    Values[I] := NextItem;
  until (Values[I] = '') or (I = High(Values));
  if I = High(Values) then Exit;
  Dec(I);
  FieldInfo.OriginalFieldName := StrPas(PopValue);
  FieldInfo.TableName := StrPas(PopValue);
  FieldInfo.DatabaseName := StrPas(PopValue);
  Result := (FieldInfo.OriginalFieldName <> '') and (FieldInfo.TableName <> '');
end;

const
  StringFieldTypes = [ftString, ftFixedChar, ftWideString, ftGuid];
  BlobFieldTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle,
    ftTypedBinary, ftOraBlob, ftOraClob];

function IsNumeric(DataType: TFieldType): Boolean;
begin
  Result := DataType in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
    ftBCD, ftAutoInc, ftLargeint{$IFDEF DELPHI_6}, ftFMTBcd{$ENDIF}];
end;

function IsTemporal(DataType: TFieldType): Boolean;
begin
  Result := DataType in [ftDate, ftTime, ftDateTime{$IFDEF DELPHI_6}, ftTimeStamp{$ENDIF}];
end;

{ TFilterExpr }

constructor TFilterExpr.Create(DataSet: TDataSet; Options: TFilterOptions;
  ParseOptions: TParserOptions; const FieldName: string; DepFields: TBits;
  FieldMap: TFieldMap);
begin
  FFieldMap := FieldMap;
  FDataSet := DataSet;
  FOptions := Options;
  FFieldName := FieldName;
  FParserOptions := ParseOptions;
  FDependentFields := DepFields;
end;

destructor TFilterExpr.Destroy;
var
  Node: PExprNode;
begin
  SetLength(FExprBuffer, 0);
  while FNodes <> nil do
  begin
    Node := FNodes;
    FNodes := Node^.FNext;
    if (Node^.FKind = enFunc) and (Node^.FArgs <> nil) then
      Node^.FArgs.Free;
    Dispose(Node);
  end;
end;

function TFilterExpr.FieldFromNode(Node: PExprNode): TField;
begin
  Result := GetFieldByName(Node^.FData);
  if not (Result.FieldKind in [fkData, fkInternalCalc]) then
    DatabaseErrorFmt(SExprBadField, [Result.FieldName]);
end;

function TFilterExpr.GetExprData(Pos, Size: Integer): PChar;
begin
  SetLength(FExprBuffer, FExprBufSize + Size);
  Move(FExprBuffer[Pos], FExprBuffer[Pos + Size], FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
end;

function TFilterExpr.GetFilterData(Root: PExprNode): TExprData;
begin
  FExprBufSize := CANExprSize;
  SetLength(FExprBuffer, FExprBufSize);
  PutExprNode(Root, coNOTDEFINED);
  PWord(@FExprBuffer[0])^ := CANEXPRVERSION;                { iVer }
  PWord(@FExprBuffer[2])^ := FExprBufSize;                  { iTotalSize }
  PWord(@FExprBuffer[4])^ := $FFFF;                         { iNodes }
  PWord(@FExprBuffer[6])^ := CANEXPRSIZE;                   { iNodeStart }
  PWord(@FExprBuffer[8])^ := FExprNodeSize + CANEXPRSIZE;   { iLiteralStart }
  Result := FExprBuffer;
end;

function TFilterExpr.NewCompareNode(Field: TField; Operator: TCANOperator;
  const Value: Variant): PExprNode;
var
  ConstExpr: PExprNode;
begin
  ConstExpr := NewNode(enConst, coNOTDEFINED, Value, nil, nil);
  ConstExpr^.FDataType := Field.DataType;
  ConstExpr^.FDataSize := Field.Size;
  Result := NewNode(enOperator, Operator, Unassigned,
    NewNode(enField, coNOTDEFINED, Field.FieldName, nil, nil), ConstExpr);
end;

function TFilterExpr.NewNode(Kind: TExprNodeKind; Operator: TCANOperator;
  const Data: Variant; Left, Right: PExprNode): PExprNode;
var
  Field : TField;
begin
  New(Result);
  with Result^ do
  begin
    FNext := FNodes;
    FKind := Kind;
    FPartial := False;
    FOperator := Operator;
    FData := Data;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
  if Kind = enField then
  begin
    Field := GetFieldByName(Data);
    if Field = nil then
      DatabaseErrorFmt(SFieldNotFound, [Data]);
    Result^.FDataType := Field.DataType;
    Result^.FDataSize := Field.Size;
  end;
end;

{$IFNDEF FPC}
function TFilterExpr.PutConstBCD(const Value: Variant;
  Decimals: Integer): Integer;
var
  C: Currency;
  BCD: TBcd;
begin
  if VarType(Value) = varString then
    C := StrToCurr(string(TVarData(Value).VString)) else
    C := Value;
  CurrToBCD(C, BCD, 32, Decimals);
  Result := PutConstNode(ftBCD, @BCD, 18);
end;
{$ENDIF}

{$IFDEF DELPHI_6}
function TFilterExpr.PutConstFMTBCD(const Value: Variant;
  Decimals: Integer): Integer;
var
  BCD: TBcd;
begin

  if VarType(Value) = varString then
    BCD := StrToBcd(string(TVarData(Value).VString)) else
    BCD := VarToBcd(Value);
  Result := PutConstNode(ftBCD, @BCD, 18);
end;

function TFilterExpr.PutConstSQLTimeStamp(const Value: Variant): Integer;
var
  TimeStamp: TSQLTimeStamp;
begin
  if VarType(Value) = varString then
    TimeStamp := StrToSQLTimeStamp(string(TVarData(Value).VString)) else
    TimeStamp := VarToSQLTimeStamp(Value);
  Result := PutConstNode(ftTimeStamp, @TimeStamp, 16);
end;

function TFilterExpr.PutConstInt64(DataType: TFieldType;
  const Value: Variant): Integer;
var
  IntValue: LargeInt;
begin
  IntValue := Value;
  Result := PutConstNode(DataType, @IntValue, SizeOf(IntValue));
end;
{$ENDIF}

function TFilterExpr.PutConstBool(const Value: Variant): Integer;
var
  B: WordBool;
begin
  B := Value;
  Result := PutConstNode(ftBoolean, @B, SizeOf(WordBool));
end;

function TFilterExpr.PutConstDate(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToDate(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftDate, @TimeStamp.Date, 4);
end;

function TFilterExpr.PutConstDateTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  DateData: Double;
begin
  if VarType(Value) = varString then
  {$IFDEF DELPHI_7}
    begin
      if not TryStrToDate(string(TVarData(Value).VString), DateTime) then
        DateTime := VarToDateTime(Value);
    end
  {$ELSE}
    try
      DateTime := StrToDateTime(string(TVarData(Value).VString))
    except
      DateTime := VarToDateTime(Value)
    end
  {$ENDIF UNDER_DELPHI_6}
  else
    DateTime := VarToDateTime(Value);
  DateData := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
  Result := PutConstNode(ftDateTime, @DateData, 8);
end;

function TFilterExpr.PutConstFloat(const Value: Variant): Integer;
var
  F: Double;
begin
  if VarType(Value) = varString then
    F := StrToFloat(string(TVarData(Value).VString)) else
    F := Value;
  Result := PutConstNode(ftFloat, @F, SizeOf(Double));
end;

function TFilterExpr.PutConstInt(DataType: TFieldType;
  const Value: Variant): Integer;
var
  I, Size: Integer;
begin
  if VarType(Value) = varString then
    I := StrToInt(string(TVarData(Value).VString)) else
    I := Value;
  Size := 2;
  case DataType of
    ftSmallint:
      if (I < -32768) or (I > 32767) then DatabaseError(SExprRangeError);
    ftWord:
      if (I < 0) or (I > 65535) then DatabaseError(SExprRangeError);
  else
    Size := 4;
  end;
  Result := PutConstNode(DataType, @I, Size);
end;

function TFilterExpr.PutConstNode(DataType: TFieldType; Data: PAnsiChar;
  Size: Integer): Integer;
begin
  Result := PutNode(nodeCONST, coCONST2, 3);
  SetNodeOp(Result, 0, FFieldMap[DataType]);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
end;

function TFilterExpr.PutConstStr(const Value: string): Integer;
var
  Str: string;
  Buffer: array[0..255] of AnsiChar;
begin
  if Length(Value) >= SizeOf(Buffer) then
    Str := Copy(Value, 1, SizeOf(Buffer) - 1) else
    Str := Value;
  FDataSet.Translate(PAnsiChar(AnsiString(Str)), Buffer, True);
  Result := PutConstNode(ftString, Buffer, Length(Str) + 1);
end;

function TFilterExpr.PutConstTime(const Value: Variant): Integer;
var
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
begin
  if VarType(Value) = varString then
    DateTime := StrToTime(string(TVarData(Value).VString)) else
    DateTime := VarToDateTime(Value);
  TimeStamp := DateTimeToTimeStamp(DateTime);
  Result := PutConstNode(ftTime, @TimeStamp.Time, 4);
end;

function TFilterExpr.PutData(Data: PAnsiChar; Size: Integer): Integer;
begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
end;

function TFilterExpr.PutConstant(Node: PExprNode): Integer;
begin
  Result := 0;
  case Node^.FDataType of
    ftSmallInt, ftInteger, ftWord, ftAutoInc:
      Result := PutConstInt(Node^.FDataType, Node^.FData);
    ftFloat, ftCurrency:
      Result := PutConstFloat(Node^.FData);
    ftString, ftWideString, ftFixedChar, ftGuid:
      Result := PutConstStr(Node^.FData);
    ftDate:
      Result := PutConstDate(Node^.FData);
    ftTime:
      Result := PutConstTime(Node^.FData);
    ftDateTime:
      Result := PutConstDateTime(Node^.FData);
    ftBoolean:
      Result := PutConstBool(Node^.FData);
{$IFDEF DELPHI_6}
    ftTimeStamp:
      Result := PutConstSQLTimeStamp(Node^.FData);
    ftFMTBcd:
      Result := PutConstFMTBCD(Node^.FData, Node^.FDataSize);
    ftLargeint:
      Result := PutConstInt64(Node^.FDataType, Node^.FData);
{$ENDIF}
{$IFNDEF FPC}
    ftBCD:
      Result := PutConstBCD(Node^.FData, Node^.FDataSize);
{$ENDIF}
    else
      DatabaseErrorFmt(SExprBadConst, [Node^.FData]);
  end;
end;


function TFilterExpr.PutExprNode(Node: PExprNode; ParentOp: TCANOperator): Integer;
const
  ReverseOperator: array[coEQ..coLE] of TCANOperator = (coEQ, coNE, coLT,
    coGT, coLE, coGE);
  BoolFalse: WordBool = False;
var
  Field: TField;
  Left, Right, Temp : PExprNode;
  LeftPos, RightPos, ListElem, PrevListElem, I: Integer;
  Operator: TCANOperator;
  CaseInsensitive, PartialLength, L:  Integer;
  S: string;
begin
  Result := 0;
  case Node^.FKind of
    enField:
      begin
        Field := FieldFromNode(Node);
        if (ParentOp in [coOR, coNOT, coAND, coNOTDEFINED]) and
           (Field.DataType = ftBoolean) then
        begin
          Result := PutNode(nodeBINARY, coNE, 2);
          SetNodeOp(Result, 0, PutFieldNode(Field, Node));
          SetNodeOp(Result, 1, PutConstNode(ftBoolean, @BoolFalse, SizeOf(WordBool)));
        end
        else
          Result := PutFieldNode(Field, Node);
      end;
    enConst:
      Result := PutConstant(Node);
    enOperator:
      case Node^.FOperator of
        coIN:
          begin
            Result := PutNode(nodeBINARY, coIN, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
            ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
            SetNodeOp(Result, 1, ListElem);
            PrevListElem := ListElem;
            for I := 0 to Node^.FArgs.Count - 1 do 
            begin
              LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
              if I = 0 then 
                begin
                  SetNodeOp(PrevListElem, 0, LeftPos);
                  SetNodeOp(PrevListElem, 1, 0);
                end
              else
                begin
                  ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
                  SetNodeOp(ListElem, 0, LeftPos);
                  SetNodeOp(ListElem, 1, 0);
                  SetNodeOp(PrevListElem, 1, ListElem);
                  PrevListElem := ListElem;
                end;
              end;
          end;
        coNOT,
        coISBLANK,
        coNOTBLANK:
          begin
            Result := PutNode(nodeUNARY, Node^.FOperator, 1);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft,Node^.FOperator));
          end;
        coEQ..coLE,
        coAND,coOR,
        coADD..coDIV,
        coLIKE,
        coASSIGN:
          begin
            Operator := Node^.FOperator;
            Left := Node^.FLeft;
            Right := Node^.FRight;
            if (Operator in [coEQ..coLE]) and (Right^.FKind = enField) and
               (Left^.FKind <> enField) then
            begin
              Temp := Left;
              Left := Right;
              Right := Temp;
              Operator := ReverseOperator[Operator];
            end;

            Result := 0;
            if (Left^.FKind = enField) and (Right^.FKind = enConst)
               and ((Node^.FOperator = coEQ)  or (Node^.FOperator = coNE)
               or (Node^.FOperator = coLIKE)) then
            begin
              if VarIsNull(Right^.FData) then
              begin
                case Node^.FOperator of
                  coEQ: Operator := coISBLANK;
                  coNE: Operator := coNOTBLANK;
                else
                  DatabaseError(SExprBadNullTest);
                end;
                Result := PutNode(nodeUNARY, Operator, 1);
                SetNodeOp(Result, 0, PutExprNode(Left,Node^.FOperator));
              end
              else if (Right^.FDataType in StringFieldTypes) then
              begin
                S := Right^.FData;
                L := Length(S);
                if L <> 0 then
                begin
                  CaseInsensitive := 0;
                  PartialLength := 0;
                  if foCaseInsensitive in FOptions then CaseInsensitive := 1;
                  if Node^.FPartial then PartialLength := L else
                    if not (foNoPartialCompare in FOptions) and (L > 1) and
                      (S[L] = '*') then
                    begin
                      Delete(S, L, 1);
                      PartialLength := L - 1;
                    end;
                  if (CaseInsensitive <> 0) or (PartialLength <> 0) then
                  begin
                    Result := PutNode(nodeCOMPARE, Operator, 4);
                    SetNodeOp(Result, 0, CaseInsensitive);
                    SetNodeOp(Result, 1, PartialLength);
                    SetNodeOp(Result, 2, PutExprNode(Left,Node^.FOperator));
                    SetNodeOp(Result, 3, PutConstStr(S));
                  end;
                end;
              end;
            end;

            if Result = 0 then
            begin
              if (Operator = coISBLANK) or (Operator = coNOTBLANK) then
              begin
                Result := PutNode(nodeUNARY, Operator, 1);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                SetNodeOp(Result, 0, LeftPos);
              end else
              begin
                Result := PutNode(nodeBINARY, Operator, 2);
                LeftPos := PutExprNode(Left,Node^.FOperator);
                RightPos := PutExprNode(Right,Node^.FOperator);
                SetNodeOp(Result, 0, LeftPos);
                SetNodeOp(Result, 1, RightPos);
              end;
            end;
          end;
      end;
    enFunc:
      begin
        Result := PutNode(nodeFUNC, coFUNC2, 2);
        SetNodeOp(Result, 0,  PutData(PAnsiChar(ansistring(Node^.FData)),
          Length(string(Node^.FData)) + 1));
        if Node^.FArgs <> nil then
        begin
          ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
          SetNodeOp(Result, 1, ListElem);
          PrevListElem := ListElem;
          for I := 0 to Node^.FArgs.Count - 1 do
          begin
            LeftPos := PutExprNode(Node^.FArgs.Items[I],Node^.FOperator);
            if I = 0 then
            begin
              SetNodeOp(PrevListElem, 0, LeftPos);
              SetNodeOp(PrevListElem, 1, 0);
            end
            else
            begin
              ListElem := PutNode(nodeLISTELEM, coLISTELEM2, 2);
              SetNodeOp(ListElem, 0, LeftPos);
              SetNodeOp(ListElem, 1, 0);
              SetNodeOp(PrevListElem, 1, ListElem);
              PrevListElem := ListElem;
            end;
          end;
        end else
          SetNodeOp(Result, 1, 0);
      end;
  end;
end;


function TFilterExpr.PutFieldNode(Field: TField; Node: PExprNode): Integer;
var
  Buffer: array[0..255] of ansiChar;
begin
  if poFieldNameGiven in FParserOptions then
    FDataSet.Translate(PAnsiChar(Ansistring(Field.FieldName)), Buffer, True)
  else
    FDataSet.Translate(PAnsiChar(Ansistring(Node^.FData)), Buffer, True);
  Result := PutNode(nodeFIELD, coFIELD2, 2);
  SetNodeOp(Result, 0, Field.FieldNo);
  SetNodeOp(Result, 1, PutData(Buffer, StrLen(Buffer) + 1));
end;

function TFilterExpr.PutNode(NodeType: NodeClass; OpType: TCANOperator;
  OpCount: Integer): Integer;
var
  Size: Integer;
  Data: PChar;
begin
  Size := CANHDRSIZE + OpCount * SizeOf(Word);
  Data := GetExprData(CANEXPRSIZE + FExprNodeSize, Size);
  PInteger(@Data[0])^ := Integer(NodeType); { CANHdr.nodeClass }
  PInteger(@Data[4])^ := Integer(OpType);   { CANHdr.coOp }
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
end;

procedure TFilterExpr.SetNodeOp(Node, Index, Data: Integer);
begin
  PWordArray(PChar(FExprBuffer) + (CANEXPRSIZE + Node +
    CANHDRSIZE))^[Index] := Data;
end;

function TFilterExpr.GetFieldByName(Name: string) : TField;
var
  I: Integer;
  F: TField;
  FieldInfo: TFieldInfo;
begin
  Result := nil;
  if poFieldNameGiven in FParserOptions then
    Result := FDataSet.FieldByName(FFieldName)
  else if poUseOrigNames in FParserOptions then
  begin
    for I := 0 to FDataset.FieldCount - 1 do
    begin
      F := FDataSet.Fields[I];
      if GetFieldInfo(F.Origin, FieldInfo) and
         (AnsiCompareStr(Name, FieldInfo.OriginalFieldName) = 0) then
      begin
        Result := F;
        Exit;
      end;
    end;
  end;
  if Result = nil then
    Result := FDataSet.FieldByName(Name);
  if (Result <> nil) and (Result.FieldKind = fkCalculated) and (poAggregate in FParserOptions) then
    DatabaseErrorFmt(SExprNoAggOnCalcs, [Result.FieldName]);
  if (poFieldDepend in FParserOptions) and (Result <> nil) and
     (FDependentFields <> nil) then
    FDependentFields[Result.FieldNo-1] := True;
end;

constructor TExprParser.Create(DataSet: TDataSet; const Text: string;
  Options: TFilterOptions; ParserOptions: TParserOptions; const FieldName: string;
  DepFields: TBits; FieldMap: TFieldMap);
begin
  FDecimalSeparator := PSQLTypes.PSQL_FS.DecimalSeparator;
  FFieldMap := FieldMap;
  FStrTrue := STextTrue;
  FStrFalse := STextFalse;
  FDataSet := DataSet;
  FDependentFields := DepFields;
  FFilter := TFilterExpr.Create(DataSet, Options, ParserOptions, FieldName,
    DepFields, FieldMap);
  if Text <> '' then
    SetExprParams(Text, Options, ParserOptions, FieldName);
end;

destructor TExprParser.Destroy;
begin
  FFilter.Free;
end;

procedure  TExprParser.SetExprParams(const Text: string; Options: TFilterOptions;
  ParserOptions: TParserOptions; const FieldName: string);
var
  Root, DefField: PExprNode;
begin
  FParserOptions := ParserOptions;
  if FFilter <> nil then
    FFilter.Free;
  FFilter := TFilterExpr.Create(FDataSet, Options, ParserOptions, FieldName,
    FDependentFields, FFieldMap);
  FText := Text;
  FSourcePtr := PChar(Text);
  FFieldName := FieldName;
  NextToken;
  Root := ParseExpr;
  if FToken <> etEnd then DatabaseError(SExprTermination);
  if (poAggregate in FParserOptions) and (Root^.FScopeKind <> skAgg) then
     DatabaseError(SExprNotAgg);
  if (not (poAggregate in FParserOptions)) and (Root^.FScopeKind = skAgg) then
     DatabaseError(SExprNoAggFilter);
  if poDefaultExpr in ParserOptions then
  begin
    DefField := FFilter.NewNode(enField, coNOTDEFINED, FFieldName, nil, nil);
    if (IsTemporal(DefField^.FDataType) and (Root^.FDataType in StringFieldTypes)) or
       ((DefField^.FDataType = ftBoolean ) and (Root^.FDataType in StringFieldTypes)) then
      Root^.FDataType := DefField^.FDataType;

    if not ((IsTemporal(DefField^.FDataType) and IsTemporal(Root^.FDataType))
       or (IsNumeric(DefField^.FDataType) and IsNumeric(Root^.FDataType))
       or ((DefField^.FDataType in StringFieldTypes) and (Root^.FDataType in StringFieldTypes))
       or ((DefField^.FDataType = ftBoolean) and (Root^.FDataType = ftBoolean))) then
      DatabaseError(SExprTypeMis);
    Root := FFilter.NewNode(enOperator, coASSIGN, Unassigned, Root, DefField);
  end;

  if not (poAggregate in FParserOptions) and not(poDefaultExpr in ParserOptions)
     and (Root^.FDataType <> ftBoolean ) then
     DatabaseError(SExprIncorrect);

  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
end;

function TExprParser.NextTokenIsLParen : Boolean;
var
  P : PChar;
begin
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P^ = '(';
end;

function EndOfLiteral(var P : PChar): Boolean;
var
  FName: String;
  PTemp: PChar;
begin
  Inc(P);
  Result := P^ <> '''';
  if Result then
  begin      // now, look for 'John's Horse'
    if AnsiStrScan(P, '''') <> Nil then     // found another '
    begin
      PTemp := P;  // don't advance P
      while CharInSet(PTemp[0], [ ' ', ')' ]) do Inc(PTemp);
      if NextSQLToken(PTemp, FName, stValue) in [stFieldName, stUnknown] then
      begin   // 'John's Horse' case: not really end of literal
        Result := False;
        Dec(P);
      end;
    end;
  end;
end;

procedure TExprParser.NextToken;
type
  ASet = Set of AnsiChar;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;

  function IsKatakana(const Chr: Byte): Boolean;
  begin
    Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
  end;

  procedure Skip(TheSet: ASet);
  begin
    while TRUE do
    begin
      if CharInSet(P^, LeadBytes) then
        Inc(P, 2)
      else if CharInSet(P^, TheSet) or IsKatakana(Byte(P^)) then
        Inc(P)
      else
        Exit;
    end;
  end;

begin
  FPrevToken := FToken;
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  if (P^ <> #0) and (P^ = '/') and (P[1] <> #0) and (P[1] = '*')then
  begin
    P := P + 2;
    while (P^ <> #0) and (P^ <> '*') do Inc(P);
    if (P^ = '*') and (P[1] <> #0) and (P[1] =  '/')  then
      P := P + 2
    else
      DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while CharInSet(P^, ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']']) do Inc(P);
        end
        else
          Skip(['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']']);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
        if CompareText(FTokenString, 'LIKE') = 0 then   { do not localize }
          FToken := etLIKE
        else if CompareText(FTokenString, 'IN') = 0 then   { do not localize }
          FToken := etIN
        else if CompareText(FTokenString, 'IS') = 0 then    { do not localize }
        begin
          while (P^ <> #0) and (P^ <= ' ') do Inc(P);
          TokenStart := P;
          Skip(['A'..'Z', 'a'..'z']);
          SetString(FTokenString, TokenStart, P - TokenStart);
          if CompareText(FTokenString, 'NOT')= 0 then  { do not localize }
          begin
            while (P^ <> #0) and (P^ <= ' ') do Inc(P);
            TokenStart := P;
            Skip(['A'..'Z', 'a'..'z']);
            SetString(FTokenString, TokenStart, P - TokenStart);
            if CompareText(FTokenString, 'NULL') = 0 then
              FToken := etISNOTNULL
            else
              DatabaseError(SInvalidKeywordUse);
          end
          else if CompareText (FTokenString, 'NULL') = 0  then  { do not localize }
          begin
            FToken := etISNULL;
          end
          else
            DatabaseError(SInvalidKeywordUse);
        end;
      end;
    '[':
      begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        if P = nil then DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    '''':
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then DatabaseError(SExprStringError);
          if P^ = '''' then
            if EndOfLiteral(P) then 
              Break;
          if L < SizeOf(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
        FNumericLit := False;
      end;
    '-', '0'..'9':
      begin
        if (FPrevToken <> etLiteral) and (FPrevToken <> etName) and
           (FPrevToken <> etSymbol)and (FPrevToken <> etRParen) then
          begin
            TokenStart := P;
            Inc(P);
            while CharInSet(P^, ['0'..'9', FDecimalSeparator, 'e', 'E', '+', '-']) do
              Inc(P);
            if ((P-1)^ = ',') and (FDecimalSeparator = ',') and (P^ = ' ') then
              Dec(P);
            SetString(FTokenString, TokenStart, P - TokenStart);
            FToken := etLiteral;
            FNumericLit := True;
          end
        else
         begin
           FToken := etSUB;
           Inc(P);
         end;
      end;
    '(':
      begin
        Inc(P);
        FToken := etLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := etRParen;
      end;
    '<':
      begin
        Inc(P);
        case P^ of
          '=':
            begin
              Inc(P);
              FToken := etLE;
            end;
          '>':
            begin
              Inc(P);
              FToken := etNE;
            end;
        else
          FToken := etLT;
        end;
      end;
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then
        begin
          Inc(P);
          FToken := etGE;
        end else
          FToken := etGT;
      end;
    '+':
      begin
        Inc(P);
        FToken := etADD;
      end;
    '*':
      begin
        Inc(P);
        FToken := etMUL;
      end;
    '/':
      begin
        Inc(P);
        FToken := etDIV;
      end;
    ',':
      begin
        Inc(P);
        FToken := etComma;
      end;
    #0:
      FToken := etEnd;
  else
    DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  FSourcePtr := P;
end;

function TExprParser.ParseExpr: PExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coOR, Unassigned,
      Result, ParseExpr2);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := ftBoolean;
  end;
end;

function TExprParser.ParseExpr2: PExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coAND, Unassigned,
      Result, ParseExpr3);
    GetScopeKind(Result, Result^.FLeft, Result^.FRight);
    Result^.FDataType := ftBoolean;
  end;
end;

function TExprParser.ParseExpr3: PExprNode;
begin
  if TokenSymbolIs('NOT') then
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, coNOT, Unassigned,
      ParseExpr4, nil);
    Result^.FDataType := ftBoolean;
  end else
    Result := ParseExpr4;
  GetScopeKind(Result, Result^.FLeft, Result^.FRight);
end;


function TExprParser.ParseExpr4: PExprNode;
const
  Operators: array[etEQ..etLT] of TCANOperator = (
    coEQ, coNE, coGE, coLE, coGT, coLT);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr5;
  if (FToken in [etEQ..etLT]) or (FToken = etLIKE)
     or (FToken = etISNULL) or (FToken = etISNOTNULL)
     or (FToken = etIN) then
  begin
    case FToken of
      etEQ..etLT:
        Operator := Operators[FToken];
      etLIKE:
        Operator := coLIKE;
      etISNULL:
        Operator := coISBLANK;
      etISNOTNULL:
        Operator := coNOTBLANK;
      etIN:
        Operator := coIN;
      else
        Operator := coNOTDEFINED;
    end;
    NextToken;
    Left := Result;
    if Operator = coIN then
    begin
      if FToken <> etLParen then 
        DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
      NextToken;
      Result := FFilter.NewNode(enOperator, coIN, Unassigned,
                 Left, nil);
      Result.FDataType := ftBoolean;
      if FToken <> etRParen then
      begin
        Result.FArgs := TList.Create;
        repeat
          Right := ParseExpr;
          if IsTemporal(Left.FDataType) then
            Right.FDataType := Left.FDataType;
          Result.FArgs.Add(Right);
          if (FToken <> etComma) and (FToken <> etRParen) then
            DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
          if FToken = etComma then NextToken;
        until (FToken = etRParen) or (FToken = etEnd);
        if FToken <> etRParen then
          DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        NextToken;
      end else
        DatabaseError(SExprEmptyInList);
    end else
    begin
      if (Operator <> coISBLANK) and (Operator <> coNOTBLANK) then
        Right := ParseExpr5
      else
        Right := nil;
      Result := FFilter.NewNode(enOperator, Operator, Unassigned,
        Left, Right);
      if Right <> nil then
      begin
        if (Left^.FKind = enField) and (Right^.FKind = enConst) then
          begin
            Right^.FDataType := Left^.FDataType;
            Right^.FDataSize := Left^.FDataSize;
          end
        else if (Right^.FKind = enField) and (Left^.FKind = enConst) then
          begin
            Left^.FDataType := Right^.FDataType;
            Left^.FDataSize := Right^.FDataSize;
          end;
      end;
      if (Left^.FDataType in BlobFieldTypes) and (Operator = coLIKE) then
      begin
        if Right^.FKind = enConst then Right^.FDataType := ftString;
      end
      else if (Operator <> coISBLANK) and (Operator <> coNOTBLANK)
         and ((Left^.FDataType in (BlobFieldTypes + [ftBytes])) or
         ((Right <> nil) and (Right^.FDataType in (BlobFieldTypes + [ftBytes])))) then
        DatabaseError(SExprTypeMis);
      Result.FDataType := ftBoolean;
      if Right <> nil then
      begin
        if IsTemporal(Left.FDataType) and (Right.FDataType in StringFieldTypes) then
          Right.FDataType := Left.FDataType
        else if IsTemporal(Right.FDataType) and (Left.FDataType in StringFieldTypes) then
          Left.FDataType := Right.FDataType;
      end;
      GetScopeKind(Result, Left, Right);
    end;
  end;
end;

function TExprParser.ParseExpr5: PExprNode;
const
  Operators: array[etADD..etDIV] of TCANOperator = (
    coADD, coSUB, coMUL, coDIV);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr6;
  while FToken in [etADD, etSUB] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr6;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;

function TExprParser.ParseExpr6: PExprNode;
const
  Operators: array[etADD..etDIV] of TCANOperator = (
    coADD, coSUB, coMUL, coDIV);
var
  Operator: TCANOperator;
  Left, Right: PExprNode;
begin
  Result := ParseExpr7;
  while FToken in [etMUL, etDIV] do
  begin
    if not (poExtSyntax in FParserOptions) then
      DatabaseError(SExprNoArith);
    Operator := Operators[FToken];
    Left := Result;
    NextToken;
    Right := ParseExpr7;
    Result := FFilter.NewNode(enOperator, Operator, Unassigned, Left, Right);
    TypeCheckArithOp(Result);
    GetScopeKind(Result, Left, Right);
  end;
end;


function TExprParser.ParseExpr7: PExprNode;
var
  FuncName: string;
begin
  case FToken of
    etSymbol:
      if (poExtSyntax in FParserOptions)
         and  NextTokenIsLParen and TokenSymbolIsFunc(FTokenString) then
        begin
          Funcname := FTokenString;
          NextToken;
          if FToken <> etLParen then 
            DatabaseErrorFmt(SExprNoLParen, [TokenName]); 
          NextToken;
          if (CompareText(FuncName,'count') = 0) and (FToken = etMUL) then 
          begin
            FuncName := 'COUNT(*)';
            NextToken;
          end;
          Result := FFilter.NewNode(enFunc, coNOTDEFINED, FuncName,
                    nil, nil);
          if FToken <> etRParen then
          begin
            Result.FArgs := TList.Create;
            repeat
              Result.FArgs.Add(ParseExpr);
              if (FToken <> etComma) and (FToken <> etRParen) then
                DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]); 
              if FToken = etComma then NextToken;
            until (FToken = etRParen) or (FToken = etEnd);
          end else 
            Result.FArgs := nil;

          GetFuncResultInfo(Result);
        end
      else if TokenSymbolIs('NULL') then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, {$IFDEF DELPHI_6}Variants.{$ENDIF}Null, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrTrue) then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 1, nil, nil);
          Result.FScopeKind := skConst;
        end
      else if TokenSymbolIs(FStrFalse) then
        begin
          Result := FFilter.NewNode(enConst, coNOTDEFINED, 0, nil, nil);
          Result.FScopeKind := skConst;
        end
      else
        begin
          Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, nil, nil);
          Result.FScopeKind := skField;
        end;
    etName:
      begin
        Result := FFilter.NewNode(enField, coNOTDEFINED, FTokenString, nil, nil);
        Result.FScopeKind := skField;
      end;
    etLiteral:
      begin
        Result := FFilter.NewNode(enConst, coNOTDEFINED, FTokenString, nil, nil);
        if FNumericLit then Result^.FDataType := ftFloat else
           Result^.FDataType := ftString;
        Result.FScopeKind := skConst;
      end;
    etLParen:
      begin
        NextToken;
        Result := ParseExpr;
        if FToken <> etRParen then DatabaseErrorFmt(SExprNoRParen, [TokenName]);
      end;
  else
    DatabaseErrorFmt(SExprExpected, [TokenName]);
    Result := nil;
  end;
  NextToken;
end;

procedure  TExprParser.GetScopeKind(Root, Left, Right : PExprNode);
begin
  if (Left = nil) and (Right = nil) then Exit;
  if Right = nil then
  begin
    Root.FScopeKind := Left.FScopeKind;
    Exit;
  end;
  if ((Left^.FScopeKind = skField) and (Right^.FScopeKind = skAgg))
     or ((Left^.FScopeKind = skAgg) and (Right^.FScopeKind = skField)) then
    DatabaseError(SExprBadScope);
  if (Left^.FScopeKind = skConst) and (Right^.FScopeKind = skConst) then
    Root^.FScopeKind := skConst
  else if (Left^.FScopeKind = skAgg) or (Right^.FScopeKind = skAgg) then
    Root^.FScopeKind := skAgg
  else if (Left^.FScopeKind = skField) or (Right^.FScopeKind = skField) then
    Root^.FScopeKind := skField;
end;

procedure TExprParser.GetFuncResultInfo(Node : PExprNode);
begin
  Node^.FDataType := ftString;
  if (CompareText(Node^.FData, 'COUNT(*)') <> 0 )
     and (CompareText(Node^.FData,'GETDATE') <> 0 )
     and ( (Node^.FArgs = nil ) or ( Node^.FArgs.Count = 0) ) then
      DatabaseError(SExprTypeMis);

  if (Node^.FArgs <> nil) and (Node^.FArgs.Count > 0) then
     Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  if (CompareText(Node^.FData , 'SUM') = 0) or
     (CompareText(Node^.FData , 'AVG') = 0) then
  begin
    Node^.FDataType := ftFloat;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'MIN') = 0) or
          (CompareText(Node^.FData , 'MAX') = 0) then
  begin
    Node^.FDataType := PExprNode(Node^.FArgs.Items[0])^.FDataType;
    Node^.FScopeKind := skAgg;
  end
  else if  (CompareText(Node^.FData , 'COUNT') = 0) or
           (CompareText(Node^.FData , 'COUNT(*)') = 0) then
  begin
    Node^.FDataType := ftInteger;
    Node^.FScopeKind := skAgg;
  end
  else if (CompareText(Node^.FData , 'YEAR') = 0) or
          (CompareText(Node^.FData , 'MONTH') = 0) or
          (CompareText(Node^.FData , 'DAY') = 0) or
          (CompareText(Node^.FData , 'HOUR') = 0) or
          (CompareText(Node^.FData , 'MINUTE') = 0) or
          (CompareText(Node^.FData , 'SECOND') = 0 ) then
  begin
    Node^.FDataType := ftInteger;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'GETDATE') = 0  then
  begin
    Node^.FDataType := ftDateTime;
    Node^.FScopeKind := skConst;
  end
  else if CompareText(Node^.FData , 'DATE') = 0  then
  begin
    Node^.FDataType := ftDate;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end
  else if CompareText(Node^.FData , 'TIME') = 0  then
  begin
    Node^.FDataType := ftTime;
    Node^.FScopeKind := PExprNode(Node^.FArgs.Items[0])^.FScopeKind;
  end;
end;

function TExprParser.TokenName: string;
begin
  if FSourcePtr = FTokenPtr then Result := SExprNothing else
  begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (CompareText(FTokenString, S) = 0);
end;


function TExprParser.TokenSymbolIsFunc(const S: string) : Boolean;
begin
  Result := (CompareText(S, 'UPPER') = 0) or
            (CompareText(S, 'LOWER') = 0) or
            (CompareText(S, 'SUBSTRING') = 0) or
            (CompareText(S, 'TRIM') = 0) or
            (CompareText(S, 'TRIMLEFT') = 0) or
            (CompareText(S, 'TRIMRIGHT') = 0) or
            (CompareText(S, 'YEAR') = 0) or
            (CompareText(S, 'MONTH') = 0) or
            (CompareText(S, 'DAY') = 0) or
            (CompareText(S, 'HOUR') = 0) or
            (CompareText(S, 'MINUTE') = 0) or
            (CompareText(S, 'SECOND') = 0) or
            (CompareText(S, 'GETDATE') = 0) or
            (CompareText(S, 'DATE') = 0) or
            (CompareText(S, 'TIME') = 0) or
            (CompareText(S, 'SUM') = 0) or
            (CompareText(S, 'MIN') = 0) or
            (CompareText(S, 'MAX') = 0) or
            (CompareText(S, 'AVG') = 0) or
            (CompareText(S, 'COUNT') = 0);

end;

procedure  TExprParser.TypeCheckArithOp(Node: PExprNode);
begin
  with Node^ do
  begin
    if IsNumeric(FLeft.FDataType) and IsNumeric(FRight.FDataType)  then
      FDataType := ftFloat
    else if (FLeft.FDataType in StringFieldTypes) and
       (FRight.FDataType in StringFieldTypes) and (FOperator = coADD) then
      FDataType := ftString
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = coADD) then
      FDataType := ftDateTime
    else if IsTemporal(FLeft.FDataType) and IsNumeric(FRight.FDataType) and
       (FOperator = coSUB) then
      FDataType := FLeft.FDataType
    else if IsTemporal(FLeft.FDataType) and IsTemporal(FRight.FDataType) and
       (FOperator = coSUB) then
      FDataType := ftFloat
    else if (FLeft.FDataType in StringFieldTypes) and IsTemporal(FRight.FDataType) and
       (FOperator = coSUB) then
    begin
      FLeft.FDataType := FRight.FDataType;
      FDataType := ftFloat;
    end
    else if ( FLeft.FDataType in StringFieldTypes) and  IsNumeric(FRight.FDataType )and
         (FLeft.FKind = enConst)  then
      FLeft.FDataType := ftDateTime
    else
      DatabaseError(SExprTypeMis);
  end;
end;


end.



