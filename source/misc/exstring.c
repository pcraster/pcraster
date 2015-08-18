#include "stddefx.h"

#include <string.h>
#include <ctype.h>
#include "misc.h"

#ifdef BORLANDC
# undef isspace
#endif

/* wrapper around strcmp()
 * returns 1 if strings are equal, 0 of not
 */
int StrEq(
	const char *s1,  /* first string */
	const char *s2)  /* second string. */
{
	PRECOND(s1 != NULL);
	PRECOND(s2 != NULL);
	return ! strcmp(s1,s2);
} /* StrEq */

/* compare strings like strncmp , ignoring the case of the characters
 * returns 1 if strings are equal on first count chars, 0 of not
 */
int StrNCaseEq(
	const char *s1,  /* first string */
	const char *s2,  /* second string. */
	size_t count)    /* number of characters compared */
{
	size_t i,l1,l2;
	PRECOND(s1 != NULL);
	PRECOND(s2 != NULL);

	l1 = strlen(s1);
	l2 = strlen(s2);
	if ( count > MIN(l1, l2) && l1 != l2)
		/* compare more chars then avail and not the same length */
		return 0; /* not the same lenght is not equal */

	count = MIN(count, MIN(l1,l2));
	for (i = 0; i < count; i++)
	     if ( toupper(s1[i]) != toupper(s2[i]) )
	     	return 0;
	return 1; /* all chars equal thus string equal */

} /* StrNCaseEq */

/* compare strings, ignoring the case of the characters
 * returns 1 if strings are equal, 0 of not
 */
int StrCaseEq(
	const char *s1,  /* first string */
	const char *s2)  /* second string. */
{
	PRECOND(s1 != NULL);
	PRECOND(s2 != NULL);

	return StrNCaseEq(s1,s2, MAX(strlen(s1), strlen(s2)) );

} /* StrCaseEq */

/* normal strncpy plus adding '\\0' if necessary.
 * If src is longer than count characters then an additional
 * '\\0' is copied to dest[count]. If src is not longer than
 * count characters then Strncpy0 is identical to strncpy. Due
 * to this additional '\\0', dest should have enough space for
 * count+1 characters.
 * returns destination string dest
 */
char *Strncpy0(
	char *dest,       /* write-only. Destination string. */
	const char *src,  /* 0-terminated source string. */
	size_t count)     /* Number of characters maximally copied. */
{
	size_t end;

	PRECOND(dest != NULL);
	PRECOND(src  != NULL);

	(void)strncpy(dest,src, count);
	end = (strlen(src) > count ? count : strlen(src));
	if (dest[end] != '\0')
		dest[end] = '\0';
        return(dest);

} /* Strncpy0 */

/* copy string in allocated buffer
 * The buffer is allocated by ChkTmpMalloc
 * returns ptr to allocated buffer containing the copied string or NULL 
 *
 * WARNING
 *  Use ChkTmpFree to free buffer (not free()).
 */
char *StrcpyChkTmpMalloc(
	const char *str)   /* string to copy in new allocated space */
{
	char *s;
	PRECOND(str != NULL);
	s = (char *)ChkTmpMalloc(strlen(str)+1);
	if (s == NULL)
		return NULL;
	return strcpy(s, str);
}

/* copy string in allocated buffer
 * The buffer is allocated by ChkMalloc
 * returns ptr to allocated buffer containing the copied string or NULL 
 */
char *StrcpyChkMalloc(
	const char *str)   /* string to copy in new allocated space */
{
	PRECOND(str != NULL);
	return (char *)MemcpyChkMalloc(str,strlen(str)+1);
}

/* copy bytes in allocated buffer
 * The buffer is allocated by ChkMalloc
 * returns ptr to allocated buffer containing the copied memory region or NULL 
 */
void *MemcpyChkMalloc(
	const void *src,   /* memory to copy in new allocated space */
	size_t n) /* number of bytes to copy */
{
	void *s;
	PRECOND(src != NULL);
	s = (char *)ChkMalloc(n);
	if (s == NULL)
		return NULL;
	return memcpy(s, src,n);
}

/* Checks if string contains space characters.
 * Applies the isspace() function to every character of the string.
 * returns 0 if string contains a space symbol, non-zero otherwise. 
 */
int NoSpaces(
	const char *string) /* Searched string */
{
	size_t i=0;

	while( string[i] != '\0')
		if (isspace(string[i++]))
			return(0);
	return(1);
} /* NoSpaces */

/* Checks if string is empty.
 * Test if strlen() is 0. 
 * returns 0 if strlen is not 0, non-zero otherwise. 
 */
int EmptyString(
	const char *str) /* Checked string. */
{
	return(str[0] == '\0');
}

/* Removes all space characters 
 * All isspace() characters are removed from the string, shifting
 * all non-isspace() characters to fill the gaps
 * Returns the modified string
 */
char *DelSpaces(
	char *str) /* read-write. String to be modified */
{
	size_t i,d;


	for(i=d=0; str[i] != '\0' ; i++)
		if (! isspace(str[i]))
			str[d++] = str[i];
	str[d] = '\0';
	return(str);
} /* DelSpaces */

/* Count the occurrence of some characters in a set
 * Returns the number of occurrences
 */
size_t CountChars(
	const char *str, /* String to count  */
	const char *set) /* set of characters */
{
	size_t n=0;
	const char *p = str;
	while( (p = strpbrk(p,set)) !=  NULL)
		{n++;p++;}
	return n;
} /* CountChars */

/* Removes a subset of characters 
 * All characters in set are removed from the string, shifting
 * all other characters to fill the gaps
 * Returns the modified string
 */
char *DelChars(
	char *str, /* read-write. String to be modified */
	const char *set)
{
	size_t i,d;

	for(i=d=0; str[i] != '\0' ; i++)
		if ( strchr(set, str[i]) == NULL )
			str[d++] = str[i];
	str[d] = '\0';
	return(str);
} /* DelChars */

/* Trim string.
 * Removes leading and trailing isspace() characters. 
 * Returns modified string.
 */
char *LeftRightTrim(
	char *str) /* String to be modified */
{
	int i, n;
	char *p;

	PRECOND(str != NULL);

	n = (int)strlen(str);
	if (n == 0)
		return(str);

	/* remove trailing spaces */
	for  (i = n-1; i >= 0 && isspace(str[i]); i--)
		str[i]  = '\0';

	/* remove leading spaces */
	for (p = str; isspace(*p) ; p++)
		;
  /* strcpy(str,p) -> memmove(str,p,strlen(p)+1) copy incl. \0  */
  return memmove(str,p,strlen(p)+1);
} /* LeftRightTrim */

/* Trim string and replace space by single space.
 * Removes leading and trailing isspace() characters and 
 * substitutes sequences of isspace() chararacters with one  
 * space (that is ' ' not '\t').
 * Returns modified string.
 */
char *LeftRightTabTrim(
	char *str) /* String to be modified */
{
	int i, n;

	PRECOND(str != NULL);

	n = (int)strlen(str);
	if (n == 0)
		return(str);

	(void)LeftRightTrim(str);

	/* substitute sequences of isspace() char with one ' ' */
	for(n=i=0; str[i] != '\0'; i++)
		if (isspace(str[i]))
		{
			PRECOND(n > 0); /* str[0] is not space,
			                 * because leading spaces are removed
					 */
			if (!isspace(str[n-1]))
				str[n++] = ' ';
		}
		else
			str[n++] = str[i];
	str[n] = '\0';

	return(str);

} /* LeftRightTabTrim */


/* Trim string and replace space by single space, and count tokens.
 * Removes leading and trailing isspace() characters and 
 * substitutes sequences of isspace() chararacters with one  
 * space (that is ' ' not '\t').
 * A token is a string of non-isspace() characters terminated by a space
 * or '\0'
 * Returns the number of tokens.
 */
int TokenSpaceTrim(
	char *s)  /* read-write. String to be modified and counted */
{
	int i;    /* index over s */
	int d;    /* destination index */
	int t=0;  /* #tokens */

	/* remove leading spaces */
	for(i=0; isspace(s[i]); i++)
	{
		/* inc i is all we want */;
	}
	/* copy string */
	for(d=0; s[i] != '\0'; )
	{
		if (isspace(s[i]))
		{
		   s[d++] = ' ';
		   t++;
		   while (isspace(s[i]) )
			i++;
		}
		else
		  s[d++] = s[i++];
	}
	/* adjust for trailing spaces */
	if (isspace(s[d-1]))
	{
		d--;
		t--;
	}
	/* put string terminator */
	s[d] = '\0';
	t = ( d == 0 ) ? 0 : t + 1;
	return(t);
} /* TokenSpaceTrim */

/* Check if character is in [a-zA-Z0-9_] 
 * Checks if c is an alpabetic, numerical or underscore
 * returns
 *
 *  0 if not
 *
 *  non-zero if it is
 */
int IsAlphaNumericUnderscore(int c)
{
	return isalpha(c) || isdigit(c) || (c == '_');
}


#ifdef NEVER
#define EXIT exit
int main(
	int argc,
	char *argv[])
{
	if (argc!= 4)
	{
	 (void)printf("USAGE test str1 str2 count\n");
	 EXIT(1);
	}

	(void)printf("%s %s %d\n",argv[1], argv[2], 
	       StrNCaseEq(argv[1], argv[2], (size_t)atoi(argv[3])));
	EXIT(0);
	return 0;

}
#endif 
