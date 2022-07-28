#include "mi_global_pwd.h"

//app_exe - name of the host application
//err_file - name of the file, to which stderr will be reassigned

__declspec(dllexport) int v3_restore (char * app_exe, char * err_file, char * params); //this NULL ended array of pointers

int v3_restore (char * app_exe, char * err_file, char * params)
{
	char ** mas;
	int cnt = 0;
	int c = 1; //app_exe will be the first parameter
	char ** arr;
	char ** arr2;
	int r;

	//count params number
	arr = (char **) params;
	while (*arr)
	{
	  c++;
	  arr++;
	}

	mas = (char ** ) malloc(c * sizeof(char *));

	arr = (char **) params;
	arr2 = mas;

	* arr2 = strdup(app_exe);
	arr2++;
	cnt++;
	
  	
	while (*arr)
	{
		*arr2 = strdup(*arr);
		arr2++;
		cnt++;
		arr++;
	}

	log_stream = NULL;

	if (err_file)
	{
		log_stream = freopen(err_file, "w", stderr);
		if (!log_stream)
		{
			r = 3;
			goto _end;
		}
	}


	r =  main(cnt, mas);

_end:
	//clear grabbed memory
	arr2 = mas;
	while(cnt--)
	{
		free(*arr2);
		arr2 ++;
	}
	free(mas);

	//closing file
	if(log_stream)
	{
		fflush(stderr);
		fclose(stderr);
		freopen("CON", "w", stderr);		
	}

	return r;
};
