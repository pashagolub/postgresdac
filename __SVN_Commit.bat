@call __DelAllTemp.bat yes

@echo ******************************************************************
@echo ************************ TortoiseSVN *****************************
@echo ******************************************************************

@FOR /F "usebackq" %%i IN (`cd`) DO SET PATHLOCAL=%%i

@"C:\Program Files\TortoiseSVN\bin\TortoiseProc.exe" /command:commit /path:"%PATHLOCAL%" /notempfile /closeonend:2

