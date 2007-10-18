@call __DelAllTemp.bat yes

@echo ******************************************************************
@echo ************************ TortoiseSVN *****************************
@echo ******************************************************************

@"C:\Program Files\TortoiseSVN\bin\TortoiseProc.exe" /command:update /path:"../PostgresDAC" /notempfile /closeonend:2
