//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("PostgresDACB5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dcldb50.bpi");
USEPACKAGE("Vcldb50.bpi");
USEPACKAGE("Vclbde50.bpi");
USEFORMNS("psqlAboutFrm.pas", Psqlaboutfrm, AboutComp);
USEUNIT("PSQLAccess.pas");
USEUNIT("PSQLCOMP.pas");
USEFORMNS("PSQLConnFrm.pas", Psqlconnfrm, ConnForm);
USEUNIT("PSQLCP.pas");
USEUNIT("PSQLDbTables.pas");
USEFORMNS("PSQLfldlinks.pas", Psqlfldlinks, LinkFields);
USEUNIT("PSQLTypes.pas");
USEFORMNS("PSQLupdsqled.pas", Psqlupdsqled, PSQLUpdateSQLEditForm);
USEUNIT("psqlBatch.pas");
USEUNIT("PSQLMacroQuery.pas");
USEUNIT("PSQLMigrator.pas");
USEUNIT("PSQLMonitor.pas");
USEUNIT("PSQLDump.pas");
USEUNIT("PSQLTools.pas");
USEFORMNS("PSQLStoredProcFrm.pas", Psqlstoredprocfrm, PSQLStoredProcProp);
USEUNIT("PSQLCopy.pas");
USEUNIT("PSQLCommon.pas");
USEUNIT("PSQLExtMask.pas");
USEUNIT("PSQLDirectQuery.pas");
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
