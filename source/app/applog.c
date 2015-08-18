#include "stddefx.h"


#include "app.h"
#include "misc.h"

#include <time.h>

/********/
/* USES */
/********/


static size_t logCounter=0;

#ifndef BORLANDC
static FILE *ActiveLogFile()
{
	return NULL;
}

static void SetIncrLogCounter()
{
	logCounter++;
}

#else
#include <windows.h>

static BOOL  TestValue(DWORD messId)
{
	if (messId == ERROR_SUCCESS)
		return FALSE;

	return TRUE;
}

static void SetIncrLogCounter()
{
	/* check if logging is active 
         * logging is active if the key below
         * is found.
	 * if logging is not active return NULL;
	 */
	HKEY hKey;
	DWORD regLogCounter;
	DWORD regLogCounterSize = sizeof(regLogCounter);
	char  logFileName[1024];
	logCounter=0;
	if (TestValue(
             RegOpenKeyEx(HKEY_CURRENT_USER,"Software\\PCRaster\\Mutate",
		0,KEY_ALL_ACCESS, &hKey)) ) {
		/* no such key */
		return;
	}
	if (TestValue( 
              RegQueryValueEx(hKey,"LogCounter",0,0,&regLogCounter,
		&regLogCounterSize)))
		return;
	regLogCounter++;
	if (TestValue( 
              RegSetValueEx(hKey,"LogCounter",0,REG_DWORD,&regLogCounter,
		sizeof(regLogCounter))))
		return;
	logCounter = regLogCounter;
	RegCloseKey(hKey);
}

static FILE *ActiveLogFile()
{
	/* check if logging is active 
         * logging is active if the key below
         * is found.
	 * if logging is not active return NULL;
	 */
	HKEY hKey;
	DWORD len=1024;
	char  logFileName[1024];
	if (TestValue(
             RegOpenKeyEx(HKEY_CURRENT_USER,"Software\\PCRaster\\Mutate",
		0,KEY_READ, &hKey)) ) {
		/* no such key */
		return NULL;
	}
	if (TestValue( 
              RegQueryValueEx(hKey,"DataRoot",0,0,logFileName,&len))
		)
		return NULL;
	RegCloseKey(hKey);

	return fopen(logFileName,"a");
}

#endif

static void WriteStartTag(
	FILE *f,
	const char *tagName)
{
	time_t timer=time(NULL);
	struct tm *tblock = localtime(&timer);
	fprintf(f,"<%s time=\"%s\" time2=\"%u\" session_id=\"%u\">\n",
		tagName,
		asctime(tblock),
		(size_t)timer,
		logCounter);
}

void AppLogError(const char *errorMsg)
{
       FILE *f = ActiveLogFile();
       if (!f)
       		return;
       WriteStartTag(f,"command_error");
       fprintf(f,"%s ",errorMsg);
       fprintf(f,"\n</command_error>\n");
       fclose(f);
}

extern void (* appLogErrorFunc)(const char *msg);

/* Start a logging entry. 
 */
void AppLogStart(
	int argc,		/* number of arguments */
	char **argv)		/* read-only input line, actually const */
{
       int i;
       FILE *f = ActiveLogFile();
	char *appDir,*appFile;
       if (!f)
       		return;
	SetIncrLogCounter();
       /* CW_MUTATE SEND: DATE,DIRECTORY,ARGS to logger */
       WriteStartTag(f,"command_start");
	SplitFilePathName(argv[0],&appDir, &appFile);
       fprintf(f,"<app_dir>%s</app_dir>\n",appDir);
       fprintf(f,"<app_file>%s</app_file>\n",appFile);
       for (i=1; i < argc; i++)
       	fprintf(f,"%s ",argv[i]);
       fprintf(f,"\n</command_start>\n");
       fclose(f);

       appLogErrorFunc = AppLogError;
}

void AppLogFile(const char *asciiFile)
{
       int c;
       FILE *fContents = fopen(asciiFile,"r"); 
       FILE *f = ActiveLogFile();
       if (f == NULL || fContents == NULL)
       		return;
       /* CW_MUTATE SEND: DATE,DIRECTORY,ARGS to logger */
       WriteStartTag(f,"command_file");
       while ( (c = fgetc(fContents)) != EOF )
	fputc(c,f);
       fprintf(f,"</command_file>\n");
       fclose(f);
       fclose(fContents);
}
