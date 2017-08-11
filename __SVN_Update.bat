@call __DelAllTemp.bat yes

@echo ******************************************************************
@echo ************************ TortoiseSVN *****************************
@echo ******************************************************************

@FOR /F "usebackq delims==" %%i IN (`cd`) DO SET PATHLOCAL=%%i

TortoiseProc.exe /command:update /path:"%PATHLOCAL%" /notempfile /closeonend:2
