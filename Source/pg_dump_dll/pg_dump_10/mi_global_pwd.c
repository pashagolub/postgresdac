#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <stdarg.h>
#include "pg_config.h"
#include "mi_global_pwd.h"

//for stderr reassigning
FILE * log_stream;

ErrorCallBackProc_ptr ErrorCallBackProc = NULL;
LogCallBackProc_ptr LogCallBackProc = NULL;

//prototypes
void mi_die_not_so_horribly(int code);

//implementation
void mi_die_not_so_horribly(int code)
{
  if(log_stream)
  	fclose(log_stream);
  if(ErrorCallBackProc)
    ErrorCallBackProc(code);
};

int pdmbvm_GetVersionAsInt(void)
{
  return PG_VERSION_NUM;
};
void pdmbvm_SetErrorCallBackProc(void * ptr)
{
  ErrorCallBackProc = (ErrorCallBackProc_ptr) ptr;
};

void pdmbvm_SetLogCallBackProc(void * ptr)
{
  LogCallBackProc = (LogCallBackProc_ptr) ptr;
};
