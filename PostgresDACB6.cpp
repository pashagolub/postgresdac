//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("psqlAboutFrm.pas", Psqlaboutfrm, AboutComp);
USEFORMNS("PSQLConnFrm.pas", Psqlconnfrm, ConnForm);
USEFORMNS("PSQLfldlinks.pas", Psqlfldlinks, LinkFields);
USEFORMNS("PSQLupdsqled.pas", Psqlupdsqled, PSQLUpdateSQLEditForm);
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
