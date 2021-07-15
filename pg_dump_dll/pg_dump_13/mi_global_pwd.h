
#ifndef MI_GLOBAL_PWD_H
#define MI_GLOBAL_PWD_H

__declspec(dllexport) void pdmbvm_SetErrorCallBackProc(void * ptr);
__declspec(dllexport) void pdmbvm_SetLogCallBackProc(void * ptr);
__declspec(dllexport) int pdmbvm_GetVersionAsInt(void);


extern FILE * log_stream;

typedef void (__cdecl *ErrorCallBackProc_ptr)(int);
typedef void (__cdecl *LogCallBackProc_ptr)(char *);
extern LogCallBackProc_ptr LogCallBackProc;

void mi_die_not_so_horribly(int code);

#endif   /* MI_GLOBAL_PWD_H */
