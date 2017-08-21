#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <sys/stat.h>
#ifndef _MSC_VER
# include <unistd.h>
# include <dirent.h>
#endif

#include <string.h>
#include "misc.h"

/* global header (opt.) and filestat's prototypes "" */


/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/*********************/
/* LOCAL DEFINITIONS */
/*********************/
static char privateBuffer[1024];

/******************/
/* IMPLEMENTATION */
/******************/

/* stat a file, for regular file yes or no
 * returns
 *
 * 0 if it is a (a link to) regular file
 *
 * 1 if it exists but it is not (a link to) a regular file
 *
 * 2 if the file does not exist. errno is set to ENOENT
 */
int FileStat(
    const char *fileName) /* the file name */
{
#ifdef _MSC_VER
    struct __stat64 s;

    if( _stat64(fileName, &s)) {
        return 2;
    }

    return !(s.st_mode & _S_IFREG);
#else
   struct stat s;

   if(stat(fileName, &s)) {
       return 2;
   }

   return !S_ISREG(s.st_mode);
#endif
}


/* stat a file, for regular file yes or no and check for valid filename
 * FileStatValid combines a call to FileStat and FileNameValid.
 * Very important in DOS. For example topo.imap is not valid DOS file name
 * but it if topo.ima exists it will be read as topo.ima
 * returns
 *
 * 0 if it is a (a link to) regular file
 *
 * 1 if it exists but it is not (a link to) a regular file
 *
 * 2 if the file does not exist. errno is set to ENOENT
 *
 * 3 if the file exists (in truncated form), but the fileName is not valid
 */
int FileStatValid(
	const char *fileName) /* the file name */
{
	int r = FileStat(fileName);
	if ( (!r) && (!FileNameValid(fileName)))
		r = 3;
	return r;
}

/* compare filenames.
 * FileNamesEq compares two filenames obeying the naming
 * conventions of the current platform (case sensitive or not).
 * returns 1 if filenames are equal, 0 of not
 */
int FileNamesEq(
	const char *fileName1,  /* first fileName without spaces */
	const char *fileName2)  /* second fileName  without spaces */
{
	PRECOND(fileName1 != NULL);
	PRECOND(fileName2 != NULL);
#ifdef DOS_FS
	return StrCaseEq(fileName1, fileName2);
#else
# ifdef UNIX_FS
	return StrEq(fileName1, fileName2);
# else
#       error no filesystem defined (UNIX_FS or DOS_FS)
# endif
#endif
}

/* check filename extension.
 * FileNameExt checks if fileName ends with a period (.) followed
 * by the given extension. The checks obeys the
 * conventions of filenames on the current platform (case sensitive or not).
 * returns 1 if fileName has that extension, 0 of not.
 */
int FileNameExt(
	const char *fileName,  /* fileName without spaces */
	const char *extension) /* the extension without the period */
{
	const char *p;
	PRECOND(fileName != NULL);
	PRECOND(extension != NULL);
	/* find the position of the extension
	 */
	if ( (p=strrchr(fileName, '.')) == NULL)
		return 0;
	return FileNamesEq(p+1,extension);
}

/* check if a filename is valid on the current platform.
 * FileNameValid checks if a fileName is valid on the current platform.
 * It test for DOS: the 8.3 convention if DOS_FS_8_3 is defined (not on Win32)
 * For all platforms:  aux/con (portability issue)
 * returns 1 if fileName is valid, 0 if not.
 */
int FileNameValid(
	const char *fileName)  /* fileName without spaces */
{
#ifdef DOS_FS_8_3
	const char *p = fileName;
	int   l=0;
	int   state=0; /* 0 is prefix, 1 is ext */
	int  maxAllow[2] = {8,3};
	PRECOND(fileName != NULL);
#endif
	if (StrCaseEq(fileName,"aux") || StrCaseEq(fileName,"con"))
		return 0;

#ifdef DOS_FS
#ifdef DOS_FS_8_3
	while (*p != '\0')
	{
	     if (l > maxAllow[state])
	     	return 0;
	     switch(*p) {
		case DIR_PATH_DELIM_CHAR:
			l = 0; /* new DOS_PREFIX */
			break;
		case '.':
		       l = 0; state++;
		       if (state >= 2)
				return 0;
		       break;
		default: l++;
	   }
	   p++;
	 }
	return l <= maxAllow[state]; 
#else 
	return 1;
#endif
#else
# ifdef UNIX_FS
	PRECOND(fileName != NULL);
	return 1;
# else
#        error no filesystem defined (UNIX_FS or DOS_FS)
# endif
#endif
}

/* construct path name from directory and file name
 * MakeFilePathName makes a full path name of a
 * directory and file name part by inserting the platform
 * specific directory name seporator. If dirName is an empty
 * string then no directory part (and seporator) is inserted.
 * returns
 * ptr to buf or to private buffer
 */
char *MakeFilePathName(
	char *buf,       /* buffer to copy result in. If NULL use 1024 char
	                  * private buffer if NULL
	                  */
	const char *dirName,  /* path to file, directory name */
	const char *fileName) /* name of file */
{
	 char *b = (buf == NULL) ? privateBuffer : buf;
	 if (*dirName == '\0')
	  (void)strcpy(b,fileName);
	 else
	 (void)sprintf(b,"%s%c%s",dirName,DIR_PATH_DELIM_CHAR,fileName);
	return b;
}

/* split a full path name in a directory and file part
 * SplitFilePathName splits a full path name in a
 * directory and file name part. Both returned arguments are empty
 * strings if that part is not present.
 */
void SplitFilePathName(
	const char *fullPathName, /* the full path name */
	char **dirName,  /* ptr to directory part, is ptr into privateBuffer */
	char **fileName) /* ptr to file part, is ptr into fullPathName */
{
	 char *split = strrchr(fullPathName,DIR_PATH_DELIM_CHAR);
	 if (split != NULL)
	 	*fileName = split+1;
	 else
		*fileName = (char *)fullPathName;
	 strcpy(privateBuffer,fullPathName);
	 *dirName = privateBuffer;
	 split = strrchr(privateBuffer,DIR_PATH_DELIM_CHAR);
	 if (split != NULL)
	 	*split = '\0';
	 else
	        privateBuffer[0] = '\0'; /* no dirName */
}


/* change directory seperators in-situ
 * This function changes all occurenses of '\' or '/' to
 * delimiting character '/' or '\' on the current platform.
 * returns
 *   the str argument
 */
char *ReplaceDirPathDelimChar(
	char *str) /* rw string to be altered */
{
	size_t i,n = strlen(str);
#	ifdef UNIX_FS
	 int    otherDelimChar = '\\';
#	else
	 int    otherDelimChar = '/';
#	endif
	PRECOND(n >= 1); /* non-empty string plus end " */

	for (i=0; i < n; i++)
	{
		if (str[i] == otherDelimChar)
		    str[i] = DIR_PATH_DELIM_CHAR;
	}
	return str;
}

#ifdef NEVER
int main(
	int argc,
	char *argv[]
	)
{
	int i;
	for(i = 1; i < argc; i++)
	 (void)printf("%s is a %s filename\n", argv[i],
	      FileNameValid(argv[i]) ? "VALID" : "INVALID");
	EXIT(0);
	return 0;
}
#endif
