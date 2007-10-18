unit pg_dump_dll;

interface

uses Sysutils, Windows, Forms, Classes, Dialogs;

{
!!! Юзать только в динамической версии!!! В статической программа сыпется после второго запуска!!!
}

type
//pdmvm = pasho_dolzhen_misho_vedro_marozhina
  Tpdmvm_dump = function ( app_exe : PChar;
                           database : PChar;                           pwd : PChar;                           err_str : PChar;                           out_file : pchar;                           err_file : pchar;                           params : PChar):longint;cdecl;
  Tpdmbvm_GetLastError = procedure(out_buffer : PChar);cdecl;

  Tpdmvm_restore = function ( app_exe : PChar;
                              filename : PChar;                              pwd : PChar;                              out_file : pchar;                              err_file : pchar;                              params : PChar):longint;cdecl;
  Tpdmbvm_GetVersionAsInt = function ():integer;cdecl;//mi:2007-01-15
  Tpdmbvm_SetErrorCallBackProc = procedure(ProcAddr : pointer);cdecl;//mi:2007-01-15

  TpdmvmParams = class
  private
    FParams : TStringList;
    FArr : PChar;

    procedure ClearMem();
  public
    constructor Create();
    destructor Destroy();override;

    procedure Add(aStr : string);
    procedure Clear();

    function GetPCharArray() : PChar;
  end;
  
var
  pdmvm_dump : Tpdmvm_dump;
  pdmvm_restore : Tpdmvm_restore;
  pdmvm_dump_getlasterror : Tpdmbvm_GetLastError;//mi:2006-10-12 тип одинаковый, но ето функции с разных DLL
  pdmvm_restore_getlasterror : Tpdmbvm_GetLastError;//mi:2006-10-12 тип одинаковый, но ето функции с разных DLL
  pdmbvm_GetVersionAsInt : Tpdmbvm_GetVersionAsInt;
  pdmbvm_SetErrorCallBackProc : Tpdmbvm_SetErrorCallBackProc;

//5432
function pg_dump( host_str, user_str, file_str, database : string;
                  aPort : integer;
                  var err_str : string;
                  var v : integer):longint;
function pg_restore():longint;

implementation

procedure ErrorCallBackProc(module:PChar; s:PChar);cdecl;//mi:2007-01-15
begin
  raise Exception.Create(Format('VERY HORRIBLE DIE: %s - %s', [string(module), string(s)]));
end;

//----------------------------------------------------------------------------------------------------------------------
//returns:
// 0 - OK
// 1 - common error (in pg_dump.c)
// 2 - error while connecting (wrong username, password, host or something else)
// 3 - stdout reassigning error
// -1 - error loading DLL library
function pg_dump( host_str, user_str, file_str, database : string;
                  aPort : integer;
                  var err_str : string;
                  var v : integer):longint;
var
  h : Cardinal;
  ss : array[0..255] of char;//error buffer
  p : TpdmvmParams;
begin
  Result := 65535;
  h := LoadLibrary('pg_dump.dll');
  try
  @pdmvm_dump := GetProcAddress(h, PChar('pdmvm_dump'));
  @pdmvm_dump_getlasterror := GetProcAddress(h, PChar('pdmbvm_GetLastError'));
  @pdmbvm_GetVersionAsInt := GetProcAddress(h, PChar('pdmbvm_GetVersionAsInt'));//mi:2007-01-15
  @pdmbvm_SetErrorCallBackProc := GetProcAddress(h, PChar('pdmbvm_SetErrorCallBackProc'));//mi:2007-01-15

  if not assigned(@pdmvm_dump) then
  begin
    Result := -1;
    FreeLibrary(h);
    exit;
  end;

  v := pdmbvm_GetVersionAsInt();

  pdmbvm_SetErrorCallBackProc(@ErrorCallBackProc);//mi:2007-01-15

  p := TpdmvmParams.Create();
  try
  //тут параметры дожны быть как их argv агрумент функции main отдаёт
  //длинный
  p.Add('--host=' + host_str);
  //длинный
  p.Add('--username=' + user_str);
  //длинный
    p.Add('--file=' + file_str);

//  короткий (за 2 раза, ибо ето как 2 параметра через пробел)
//  p.Add('-F');
//  p.Add('c');

  //длинный
  p.Add('--port=' + IntToStr(aPort));
  //
  p.Add('--verbose');


  //fake param for testing pdmvm_dump_getlasterror
//  p.Add('--table=g123f22fff');
//  p.Add('-g123f22fff');

  try
    Result := pdmvm_dump(PChar(Application.ExeName),
                         PChar(database),
                         PChar('ghbdtn'),
                         ss,
                         pchar('a.out'),
                         pchar('a.err'),
                         p.GetPCharArray());
  except
    on e:EAccessViolation do
    begin
      if Pos('00000159', e.Message) <> 0 then
      begin
        pdmvm_dump_getlasterror(ss);
        ShowMessage(Format('die_horribly(): %s', [ss]));
      end
      else if Pos('0000015D', e.Message) <> 0 then
        ShowMessage('exit_nicely(), look at the log!');
    end;
    on e:Exception do
    begin
      ShowMessage('AAA: ' + e.Message);
    end;
  end;

  finally
  p.Free();
  end;
  err_str := ss;
  finally
  FreeLibrary(h);
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
//returns:
// 0 - OK
// 1 - common error (in pg_restore.c or deeper)
// 3 - stdout reassigning error
// -1 - error loading DLL library
function pg_restore():longint;
var
  h : Cardinal;
  p : TpdmvmParams;
begin
  h := LoadLibrary('pg_restore.dll');
  try
  @pdmvm_restore := GetProcAddress(h, PChar('pdmvm_restore'));
  @pdmvm_restore_getlasterror := GetProcAddress(h, PChar('pdmbvm_GetLastError'));
  if not assigned(@pdmvm_restore) then
  begin
    Result := -1;
    FreeLibrary(h);
    exit;
  end;

  p := TpdmvmParams.Create();
  try
  //тут параметры дожны быть как их argv агрумент функции main отдаёт
  //длинный
  p.Add('--host=zymotic');
  //длинный
  p.Add('--username=postgres');

//  короткий (за 2 раза, ибо ето как 2 параметра через пробел)
  p.Add('-F');
  p.Add('c');

  //
  p.Add('--verbose');
  p.Add('--dbname=ss');

  Result := pdmvm_restore(PChar(Application.ExeName),
                          PChar('a.out'),//in file
                          PChar('ghbdtn'),
                          pchar('ar.out'),//out file
                          pchar('ar.err'),//out file
                          p.GetPCharArray());

  finally
  p.Free();
  end;

  finally
  FreeLibrary(h);
  end;
end;



//----------------------------------------------------------------------------------------------------------------------
{ TpdmvmParams }
procedure TpdmvmParams.Add(aStr: string);
begin
  FParams.Add(aStr);
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TpdmvmParams.Clear;
begin
  FParams.Clear();
end;
//----------------------------------------------------------------------------------------------------------------------
procedure TpdmvmParams.ClearMem;
var
  s : PChar;
  p : PInteger; //32-bit pointer
begin
  if FArr <> nil then
  begin
    p := Pointer(FArr);

    repeat
      s := pchar(p^);
      if s <> PChar(0) then
        FreeMem(s);
      Inc(p);
    until s <> PChar(0);

    FreeMem(FArr);
    FArr := nil;
  end;
end;
//----------------------------------------------------------------------------------------------------------------------
constructor TpdmvmParams.Create;
begin
  FParams := TStringList.Create();
  FArr := nil;
end;
//----------------------------------------------------------------------------------------------------------------------
destructor TpdmvmParams.Destroy;
begin
  ClearMem();
  FParams.Free();

  inherited;
end;
//----------------------------------------------------------------------------------------------------------------------
function TpdmvmParams.GetPcharArray: PChar;
var
  s : PChar;
  p : PInteger; //32-bit pointer
  i : integer;
begin
  ClearMem();

  GetMem(FArr, sizeof(pchar) * (FParams.Count + 1));
  p := Pointer(FArr);
  for i:=0 to FParams.Count - 1 do
  begin
    GetMem(s, Length(FParams[i]) + 1);
    StrCopy(s, PChar(FParams[i]));
    p^ := Integer(s);
    Inc(p);
  end;
  p^ := 0;

  Result := FArr;
end;

end.
