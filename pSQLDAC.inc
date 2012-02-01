{SVN revision: $Id$}

{.$DEFINE M_DEBUG}//remove "." to go into M_DEBUG mode
                  //don't do this until you exactly know what you are doing

{$IFDEF M_DEBUG}
{$D+} {$L+}
{$HINTS ON}
{$ENDIF}

//---------------------------
// License definition
//---------------------------
{$DEFINE MICROOLAP_BUSINESS_LICENSE}

//---------------------------
// FreePascal/Lazarus
//---------------------------
//
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

//---------------------------
// Delphi/Builder versions
//---------------------------
//DELPHI 16 (Delphi XE2)
{$IFDEF VER230}
  {$DEFINE FMX_AVAILABLE}
  {$DEFINE DELPHI_16}
  {$DEFINE DELPHI_15}
  {$DEFINE DELPHI_14}
  {$DEFINE DELPHI_12}
  {$DEFINE DELPHI_11}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 15 (Delphi XE)
{$IFDEF VER220}
  {$DEFINE DELPHI_15}
  {$DEFINE DELPHI_14}
  {$DEFINE DELPHI_12}
  {$DEFINE DELPHI_11}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 14 (Delphi Weaver)
{$IFDEF VER210}
  {$DEFINE DELPHI_14}
  {$DEFINE DELPHI_12}
  {$DEFINE DELPHI_11}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 12 (2009, Tiburon)
{$IFDEF VER200}
  {$DEFINE DELPHI_12}
  {$DEFINE DELPHI_11}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 11(2007)
{$IFDEF VER185}
  {$DEFINE DELPHI_11}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 10
{$IFDEF VER180}
  {$DEFINE DELPHI_10}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 9
{$IFDEF VER170}
  {$DEFINE DELPHI_9}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6}
{$ENDIF}

//DELPHI 7
{$IFDEF VER150}
  {$DEFINE DELPHI_7}
  {$DEFINE DELPHI_6} 
{$ENDIF}

//DELPHI 6
{$IFDEF VER140}
  {$DEFINE DELPHI_6}
  {$DEFINE UNDER_DELPHI_6}
{$ENDIF}

//DELPHI 5 and C++BUILDER 5
{$IFDEF VER130}
  {$DEFINE DELPHI_5}
  {$DEFINE UNDER_DELPHI_6}
  {$DEFINE MSWINDOWS}
{$ENDIF}

//DELPHI 4
{$IFDEF VER120}
  {$DEFINE DELPHI_4}
{$ENDIF}

//C++BUILDER 4
{$IFDEF VER125}
  {$DEFINE DELPHI_4}
{$ENDIF}

{$IFDEF DELPHI_7}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF BCB}
   {$ObjExportAll On} 
{$ENDIF}

