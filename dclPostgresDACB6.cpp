//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("PSQLupdsqled.pas", Psqlupdsqled, PSQLUpdateSQLEditForm);
USEFORMNS("PSQLConnFrm.pas", Psqlconnfrm, PSQLConnForm);
USEFORMNS("PSQLfldlinks.pas", Psqlfldlinks, PSQLLinkFields);
USEFORMNS("PSQLStoredProcFrm.pas", Psqlstoredprocfrm, PSQLStoredProcProp);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
