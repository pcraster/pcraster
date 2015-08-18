#include "stddefx.h" 

/*
 * simplex.c 
 * A simple lexical analyzer to get proccess numbers and
 * special symbols.
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <ctype.h>
#include <string.h>

#include "misc.h" 

/* global header (opt.) and simplex's prototypes "" */


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
/* characters that can appear in a 
 * number
 */
#define TOKEN_LEN 256
#define NUMBER_CHARS "0123456789.+-dDeE"

#define LEX_NUMBER      300
#define LEX_ILL_TOKEN   -1
#define LEX_READ_ERROR  -2
#define LEX_TOKEN_TOBIG -3

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
static FILE *inFd;
static long lineNr;
static char buf[TOKEN_LEN];
static char specialSym[TOKEN_LEN];
static int  lastToken; /* always remember last valid token
                        * for LexUngetToken()
                        */
static BOOL tokenPending = FALSE; /* for LexUngetToken */

/******************/
/* IMPLEMENTATION */
/******************/

static int TerminateToken(
	int  len)
{
	double dum;
	buf[len] = '\0';
	lastToken = CnvrtREAL8(&dum, buf) ? LEX_NUMBER : LEX_ILL_TOKEN;
	return lastToken;
}

/* initialize a simple lexical analyzer
 * LexInstall initializes a simple lexical analyzer.
 * The string of specialSymbols contains characters that
 * are in the token set besides the predefined number token.
 * specialSymbols may not contain any white space symbols.
 * If specialSymbols contains  symbols that are also in a number: 
 * 0 to 9 or one of .+-dDeE then these symbols will not be part of
 * the number recognition routine, they are returned separately as individual
 * tokens.
 *
 * EXAMPLE
 * .so examples/testlex.tr
 */ 
void LexInstall(
	FILE *fd,                    /* file to read from */
	const char *specialSymbols)  /* a maximum of 255 special symbols  */
{
#ifdef DEBUG
	int i,n;
	n = strlen(specialSymbols);
	PRECOND(n < TOKEN_LEN-1);
	for(i=0; i < n; i++)
	{
		PRECOND(!isspace(specialSymbols[i]));
	/*
		PRECOND(strchr(NUMBER_CHARS, specialSymbols[i])==NULL); 
	 */
	}
#endif
	(void)strcpy(specialSym, specialSymbols);
	inFd = fd;
	lineNr = 1;
	buf[0] = '\0';
        tokenPending = FALSE; /* for LexUngetToken */
}

/* get value of last parsed token
 * returns the value of the last parsed token
 * or an empty string if no token was parsed
 *
 * EXAMPLE
 * .so examples/testlex.tr
 */
const char *LexGetTokenValue(void)
{
	return (const char *)buf;
}

/* return line number of last parsed token
 * returns line number of last parsed token
 *
 * EXAMPLE
 * .so examples/testlex.tr
 */
long LexGetLineNr(void)
{
	return lineNr;
}

/* get next token
 * LexGetToken scans the file, set by LexInstall,
 * for the next token. A token is a special symbol
 * or a number. Tokens are separated by white space
 * and special symbols. A special symbol is a token
 * of 1 character set by LexInstall.
 * LexGetToken keeps track of the current line number
 * All special return constants are defined in misc.h.
 *
 * returns
 *
 * 0 on end-of-file.
 * 
 * LEX_NUMBER if a number is scanned.
 * 
 * LEX_ILL_TOKEN if a token is scanned that is not a number or special symbol.
 * 
 * LEX_READ_ERROR a read error occurred.
 * 
 * LEX_TOKEN_TOBIG token is longer then 255 characters.
 * 
 * Or the special character set by LexInstall
 *
 * EXAMPLE
 * .so examples/testlex.tr
 */
int LexGetToken(void)
{
	int c, i=0;

        if (tokenPending)
        {
          tokenPending = FALSE;
          return lastToken;
        }
	buf[0] = '\0'; /* ensure that LexGetTokenValue returns sensible things */
	while(1)
	{
read_char:
	 c = fgetc(inFd);
	 switch (c) {
	  case 0x1a: /* skip DOS Ctrl-Z at end of file */
	  	goto read_char;
	  case '\n': 
		if (i > 0)
		{
		        /* unget since token is found on this line */
			if (ungetc(c, inFd) == EOF) 
				return LEX_READ_ERROR;
			return TerminateToken( i);
		}
		else
			lineNr++;
	        break;
	  case EOF :
	  	if (i > 0)
			return TerminateToken( i);
		return feof(inFd) ? 0 : LEX_READ_ERROR;
	  default:
	        if (isspace(c))
	  	{
	  	   if (i > 0)
			return TerminateToken( i);
		} 
		else { if ( strchr(specialSym,c)!=NULL) 
		       { if (i > 0) 
		         {
		            /* unget: something is pending, 
		             * special symbol next time 
		             */
			     if (ungetc(c, inFd) == EOF) 
				return LEX_READ_ERROR;
			    return TerminateToken( i);
			 }
			 else 
			 {buf[0] = (char)c; buf[1] = '\0';
			  lastToken = c;
			  return c; /* return the special symbol */
			 }
		       } else 
		       {
		         buf[i++] = c;
		         if (i >= (TOKEN_LEN-1))
		         {
		          buf[i-1] = '\0'; /* cut off */
		          return LEX_TOKEN_TOBIG;
		         }
		       }
                }
       }
     }
}

/* unget the last token
 * LexUngetToken enables the parser to return the last token
 * again. Precondition is that the last LexGetToken action did
 * return a token.
 */
void LexUngetToken(void)
{
	PRECOND(buf[0] != '\0');
	tokenPending = TRUE;
}

/* Skip a number of lines in the Lex input
 * LexSkipLines scans the file, set by LexInstall,
 * for a certain number of new line characters. 
 * The end-of-file is also viewed as a new line.
 * returns
 *
 * The number of lines that were skipped, which can be smaller
 * than nrLines if enf-of-file is encountered, or LEX_READ_ERROR 
 * (which is a negative value)
 */
int LexSkipLines(int nrLines) /* > 0, 1 means skip current line only */
{
	int c, i=0;

	PRECOND(nrLines > 0);

	/* ensure that LexGetTokenValue 
         * returns sensible things 
        */
	buf[0] = '\0'; 

	while(1)
	{
read_char:
	 c = fgetc(inFd);
	 switch (c) {
	  case '\n': 
		lineNr++;
		i++;
		if (i == nrLines)
			return i;
	        break;
	  case EOF :
		return feof(inFd) ? i : LEX_READ_ERROR;
	  case 0x1a: /* skip DOS Ctrl-Z at end of file */
	  	goto read_char;
       }
       }
       /*NOTREACHED*/ 
       return LEX_READ_ERROR; /* never reached */
}

/* analyses token for error and prints message
 * LexError analyses the token value and prints
 * an error message to ErrorNested if token has
 * one of the following values:
 *
 * 0 end-of-file.
 *
 * LEX_READ_ERROR a read error occurred.
 *
 * LEX_TOKEN_TOBIG token is longer then 255 characters.
 *
 * returns 1 if an error message is printed, 0 if not
 */
int LexError(
	int token) /* return value of LexGetToken */
{
	switch(token) {
         case 0:
             ErrorNested("Unexpected end of file");
             return 1;
         case LEX_READ_ERROR:
             ErrorNested("General read error");
	     return 1;
         case LEX_TOKEN_TOBIG:
             ErrorNested("value or string is more than %d characters", TOKEN_LEN-1);
	     return 1;
	 default:
	     return 0;
	}
}

