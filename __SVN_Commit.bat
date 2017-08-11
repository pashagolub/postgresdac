@echo off
set MAKE_PAUSE=not
call __DelAllTemp.bat

echo ******************************************************************
echo ************************ TortoiseSVN *****************************
echo ******************************************************************
echo ============       (c) MiSHuTka, 2006-2007            ============

FOR /F "usebackq delims==" %%i IN (`cd`) DO SET PATHLOCAL=%%i

TortoiseProc.exe /command:commit /path:"%PATHLOCAL%" /notempfile /closeonend:2
