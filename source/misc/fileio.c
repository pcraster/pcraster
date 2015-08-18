#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include <string.h>

/*************/
/* EXTERNALS */
/*************/


/**********************/
/* LOCAL DECLARATIONS */
/**********************/


/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

/* Wrapper around fread
 * returns
 * -1 in case of error.
 * short (item) count: conforming to fread() return value. If a short item 
 * count is encountered, first ferror is checked and Error is called if ferror is true and
 * -1 is returned, if ferror is false and shortItemCountIsError is set then Error() is also called
 * and -1 returned.
 */
int FileRead(
	void *ptr, /* buffer filled */
	size_t size, /* size of each element */
	size_t nmemb, /* number of elements */
	FILE  *f,     /* file to read from */
	BOOL   shortItemCountIsError) /* TRUE if an error must be emitted if there are less
	                               * than nmemb members read, FALSE otherwise
	                               */
{
	int r = fread(ptr,size,nmemb,f);
	if (r != (int)nmemb)
	{
	 if (ferror(f))
	   return RetError(-1,"file read error:%s",strerror(errno));
	 else
	  if (shortItemCountIsError)
	   return RetError(-1,"file read error: expected file to be larger");
	}
	return r;
}

/* Read data at specific location in file
 * FileReadAtPos reads data a specific location by combining
 * a call to fseek() and FileRead(). Error is called in case of an
 * error.
 * returns
 *  -1 if fseek() failed or ferror on fread.
 *  short (item) count: conforming to fread() return value. If a short item 
 * count is encountered, first ferror is checked and Error is called if ferror is true and
 * -1 is returned, if ferror is false and shortItemCountIsError is set then Error() is also called
 * and -1 returned.
 */
int FileReadAtPos(
	void *ptr, /* buffer filled */
	size_t size, /* size of each element */
	size_t nmemb, /* number of elements */
	long   pos,   /* file position from start */
	FILE  *f,     /* file to read from */
	BOOL   shortItemCountIsError) /* TRUE if an error must be emitted if there are less
	                               * than nmemb members read, FALSE otherwise
	                               */
{
	if (fseek(f, pos,SEEK_SET))
	{
		Error("file read error:%s",strerror(errno));
		return -1;
	}
	return FileRead(ptr,size,nmemb,f,shortItemCountIsError);
}

/* Write data 
 * FileWrite is wrapper for fwrite()
 * returns
 *  -1 in case of failure.
 *  item count: correct number of items (nmemb) if success.
 */
int FileWrite(
	const void *ptr, /* buffer to written */
	size_t size, /* size of each element */
	size_t nmemb, /* number of elements */
	FILE  *f)     /* file to read from */
{
	int r;
	r = fwrite(ptr,size,nmemb,f);
	if (r != (int)nmemb)
	{
		Error("file write error:%s",strerror(errno));
		return -1;
	}
	return r;
}

/* Write data at specific location in file
 * FileWriteAtPos reads data a specific location by combining
 * a call to fseek() and fwrite(). Error is called in case of an
 * error.
 * returns
 *  -1:if fseek() or fwrite() failed.
*  item count: correct number of items (nmemb) if success.
*/

int FileWriteAtPos(
	const void *ptr, /* buffer to be written */
	size_t size, /* size of each element */
	size_t nmemb, /* number of elements */
	long   pos,   /* file position from start */
	FILE  *f)     /* file to read from */
{
	if (fseek(f, pos,SEEK_SET))
	{
		Error("file write error:%s",strerror(errno));
		return -1;
	}
	return FileWrite(ptr,size,nmemb,f);
}

/* wrapper around fopen
 * FileOpen opens a file, in case of an error Error() is called
 * returns a valid file descriptor or NULL in case of an error
 */
FILE *FileOpen(
	const char *fileName, /* file name, verified */
	const char *perm)     /* standard fopen() argument */
{
	FILE *f;
	f = fopen(fileName,perm);
	if (f == NULL)
	{
		PRECOND(errno != EINVAL); /* bad perm argument */
		Error("file open error on '%s':%s",fileName,strerror(errno));
	}
	return f;
}

/* determines size of an open file
 * FileSize returns the current size of a file.
 * The file pointer is set to the end of the file after a call to FileSize.
 * 
 * returns  size of file or -1 in case of error (Error() is then called
 */
long FileSize(
	FILE *f) /* the file */
{
	long l;
	if (fseek(f,0L,SEEK_END)) {
		Error("generic file read error:%s",strerror(errno));
		return -1;
	}
	l = ftell(f);
	if (l == -1) {
		Error("generic file read error:%s",strerror(errno));
		return -1;
	}
	return l;
}


/* Read line with fgets() and check result
 * FileGetString wraps fgets() with error detection.
 * returns:
 * -1 in case of error, Error() is called.
 * 1 in case of end of file
 * 0 in case of succes
 */

int  FileGetString(
   	char *s,   /* buffer to fill */
   	int  size, /* maximum number of chars read */
   	FILE *f)   /* file to read from */
{
	if (fgets(s,size,f) == NULL)
	{
	 if (ferror(f))
	 {
		Error("file read error:%s",strerror(errno));
		return -1;
	 }
	 return 1;
	}
	return 0;
}
