//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("dclPostgresDACB5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEFORMNS("PSQLupdsqled.pas", Psqlupdsqled, PSQLUpdateSQLEditForm);
USEUNIT("PSQLCOMP.pas");
USEFORMNS("PSQLConnFrm.pas", Psqlconnfrm, PSQLConnForm);
USEFORMNS("PSQLfldlinks.pas", Psqlfldlinks, PSQLLinkFields);
USEUNIT("PSQLMigrator.pas");
USEFORMNS("PSQLStoredProcFrm.pas", Psqlstoredprocfrm, PSQLStoredProcProp);
USEPACKAGE("PostgresDACB5.bpi");
USEPACKAGE("dcldb50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("Vclbde50.bpi");
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
