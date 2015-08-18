#include "stddefx.h"


#include <stdio.h>
#include <stdlib.h> /* exit */
#include <stdarg.h>
#include <string.h> /* strlen */

#include "misc.h"

#define INDENT ((char)' ') /* indent is one space */
#define BUF_SIZE ((size_t)2048)

/* ptrs[errorNestLevel] always points in errorBuf
 * to point were the next message can be put
 */
static char errorBuf[BUF_SIZE];
static char errorMsg[BUF_SIZE]; /* the complete message */
static char *ptrs[16];
static int  errorNestLevel = 0;

void ResetError() {
  memset(errorBuf,0, BUF_SIZE);
  memset(errorMsg,0, BUF_SIZE);
  errorNestLevel=0;
}

void (* appLogErrorFunc)(const char *msg)=NULL;

static void printStderr(const char *msg)
{
	fprintf(stderr, "%s", msg);
}

/* controls if Error() and siblings will exit
 * This global variable 
 * controls if Error(), vfError() and RetError calls exit. If exitOnError is non-zero
 * then that exit is called with that non-zero value. If exitOnError is zero then
 * Error() and siblings will return. exitOnError is 
 * initialized to zero.  Put exitOnError=1 in your main() for quick and dirty
 * programming.
 */
int exitOnError=0;

/* controls what Error() and siblings will print as error prefix 
 * This global variable 
 * controls what Error(), vfError() and RetError will print as prefix. If errorPrefixMsg is NULL
 * then these functions use the prefix 'ERROR:'. If errorPrefixMsg is not NULL 
 * Error() and siblings will use the prefix pointed to by errorPrefixMsg.
 */
const char *errorPrefixMsg=NULL;

/* controls the actions to be taken if Error and siblings are called
 * This global variable holds a pointer to a function, that will
 * message the user with formatted string. Default is printing to stderr.
 * In a GUI this is typical a hook to an error dialog box
 */
void (*errorHandler)(const char *msg) = printStderr;

/* Buffer an error message for Error()
 * ErrorNested() buffers messages that will be printed in
 * reverse order if Error() is called.
 * All leading and trailing isspace() characters are removed
 * and messages are indended for each call to ErrorNested().
 *
 * IMPORTANT
 * Global (not static) functions that use ErrorNested() should have 
 * the use of ErrorNested() documented.
 *
 * EXAMPLE
 * .so examples/errnest.tr
 *
 * will print this on stderr
 * : 
 *
 * .so examples/errout.tr
 */
void ErrorNested(
	const char *fmt,  /* Format control, see printf() for a description */
	... )             /* Optional arguments */
{
    va_list marker;

    /* Write text to a string and output the string. */

    va_start( marker,VA_START_ARG(fmt));
    vfErrorNested(fmt, marker );
    va_end( marker );
}

/* vfprintf-flavour of ErrorNested()
 * See ErrorNested() for full documentation.
 */
void vfErrorNested(
	const char *fmt,  /* Format control, see printf() for a description */
	va_list marker )             /* Optional arguments */
{
        char *buf;

        PRECOND (errorNestLevel < 15);

        if (!errorNestLevel)
	{ /* reset bufs */
	  errorBuf[0] = '\0';
	  ptrs[errorNestLevel] = errorBuf;
	}
	buf = ptrs[errorNestLevel];

        /* print message in buf */
        (void)vsprintf(buf, fmt, marker );

        /* remove leading and trailing space and newlines 
         */
        (void)LeftRightTrim(buf);

	ptrs[errorNestLevel+1] = ptrs[errorNestLevel]+strlen(buf)+1;
	errorNestLevel++;
	POSTCOND(ptrs[errorNestLevel] < errorBuf+BUF_SIZE);
}

/* vfprintf-flavour of Error()
 * See Error() for full documentation.
 */
void vfError(
	const char *fmt,  /* Format control, see printf() for a description */
	va_list marker )             /* Optional arguments */
{
    char buf[BUF_SIZE];
    size_t msgPtr;
    int  i;
    const char *pref;

    /* put error message in msg
     */ 
    (void)vsprintf(buf, fmt, marker );
    (void)LeftRightTrim(buf);
    pref = (errorPrefixMsg == NULL) ? "ERROR:" : errorPrefixMsg;
    (void)sprintf(errorMsg, "%s %s\n", pref,buf);

    /* let msgPtr point to insertion point 
     * the '\0'
     */
    msgPtr = strlen(errorMsg);
    PRECOND(errorMsg[msgPtr] == '\0');

    for(i = (errorNestLevel-1); i >= 0; i--)
    {
    	char *p = ptrs[i];
    	int  j,id = errorNestLevel-i; /* # of indents */
	while(*p != '\0')
	{ 
	  if (p == ptrs[i])  /* start */
	   for (j = 0; j < id; j++)
		errorMsg[msgPtr++] = INDENT;
	  errorMsg[msgPtr++] = *p;
	  if (*p == '\n') /*  newline in a level */
	   for (j = 0; j < id; j++)
		errorMsg[msgPtr++] = INDENT;
	  p++;
	}
	errorMsg[msgPtr++] = '\n';
    }
    errorNestLevel = 0; /* reset NestedErrors */

	
    if (appLogErrorFunc)
	(*appLogErrorFunc)(errorMsg);

    /* call error handler */
    errorHandler(errorMsg);

    if (exitOnError)
    	exit(exitOnError);
#ifdef DEBUG
    if ( getenv("PCR_ERROREXIT") != NULL)
    		exit(1);
#endif
}

/* Writes error message to errorHandler and (optional) exits
 * The format string is prefixed by 'ERROR: ' or other prefix set
 *  in the global variable errorPrefixMsg.
 * All leading and trailing isspace() characters are removed and a
 * closing newline is always printed.
 * All messages buffered by calls to ErrorNested() will be printed too,
 * after the message given here, in the format described in ErrorNested()
 * documentation.
 *
 * The function kept in the global variable errorHandler is called with
 * the formatted string.
 *
 * The global variable int exitOnError
 * controls if Error (and siblings) calls exit. If exitOnError is non-zero
 * then that exit is called with that non-zero value. exitOnError is 
 * initialized to zero. Put exitOnError=1 in your main() for quick and dirty
 * programming.
 *
 * The function kept in the global variable errorHandler is called with
 * the formatted string.
 *
 * If the misc-library is compiled in DEBUG mode then the environment variable
 * PCR_ERROREXIT is checked also. If it is set then Error (and siblings) calls
 * exit with value 1.
 *
 * REMARK
 * Keep error messages short but descriptive.
 */
void Error(
	const char *fmt,  /* Format control, see printf() for a description */
	... )             /* Optional arguments */
{
    va_list marker;

    /* Write text to a string and output the string. */

    va_start( marker,VA_START_ARG(fmt));
    vfError(fmt, marker );
    va_end( marker );
}

/* Writes error message to stderr and (optional) exits or returns
 * See Error() for full documentation. RetError is exactly like
 * Error but returns a value.
 * RETURNS
 *  the value of returnValue.
 */
int RetError(
	int  returnValue, /* value to be returned */
	const char *fmt,  /* Format control, see printf() for a description */
	... )             /* Optional arguments */
{
    va_list marker;

    /* Write text to a string and output the string. */

    va_start( marker,VA_START_ARG(fmt));
    vfError(fmt, marker );
    va_end( marker );
    return returnValue;
}

/* Buffers error message to be processed by Error() and returns a value
 * See ErrorNested() for full documentation. RetErrorNested is exactly like
 * ErrorNested but returns a value.
 * RETURNS
 *  the value of returnValue.
 */
int RetErrorNested(
	int  returnValue, /* value to be returned */
	const char *fmt,  /* Format control, see printf() for a description */
	... )             /* Optional arguments */
{
    va_list marker;

    /* Write text to a string and buffer the string. */

    va_start( marker,VA_START_ARG(fmt));
    vfErrorNested(fmt, marker );
    va_end( marker );
    return returnValue;
}

/* Writes warning message to stderr 
 * The format string is prefixed by 'WARNING: '.
 * A newline is printed if the format string has no ending
 * newline. 
 */
void Warning(
	const char *fmt,  /* Format control, see printf() for a description */
	... )             /* Optional arguments */
{
    va_list marker;

    (void)fprintf(stderr,"WARNING: ");

    /* Write text to a string and output the string. */

    va_start( marker,VA_START_ARG(fmt));
    (void)vfprintf(stderr, fmt, marker );
    va_end( marker );

    if (fmt[strlen(fmt)-1] != '\n')
		(void)fprintf(stderr, "\n");
}

#ifdef WIN32

# include <windows.h>

char *Win32GetLastError(void)
{
 static char win32MsgBuf[1024];

 FormatMessage(  FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
                 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                 (LPTSTR) &win32MsgBuf, 1023, NULL);
 return win32MsgBuf;
}

//! return 1 for failure
int Win32SetLastError(long errorCode)
{
  SetLastError(errorCode);
  return 1;
}

#endif
