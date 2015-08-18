#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <ctype.h> /* isspace */
#include <math.h> /* floor */
#include "misc.h" 

/* global header (opt.) and cnvrtnum's prototypes "" */


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

/******************/
/* IMPLEMENTATION */
/******************/
/* tests if endptr point to string with NOT only space
 */
static BOOL NotOnlySpace(
	const char *s)
{
	while (*s != '\0')
	 if (! isspace(*(s++)))
	 	return TRUE;
	return FALSE;
}


/* Converts a string to an INT4 number 
 * This function converts 
 * a string to an INT4 number with domain checking. A string 
 * may contain leading or trailing spaces. 
 * It may be in floating
 * point format as long as is an integer (e.g. 1.0, 1E3 but not 1.1.
 * Returns TRUE if the number is an integer between INT4_MIN and 
 * INT4_MAX, FALSE otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtINT4(
	INT4      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to an INT4 */
{
	double d;

	PRECOND(result != NULL);
	PRECOND(str != NULL);

	/* is it a valid double ? */
	if (! CnvrtDouble(&d,str))
		return FALSE;

	/* is there a fraction ? */
	if ( (d != floor(d)) ||
	     (d < ((double)INT4_MIN)) ||
	     (d > ((double)INT4_MAX)) )
		return FALSE;
	*result = (INT4)d;
	 return TRUE;
}

/* Converts a string to an UINT1 number
 * This function converts
 * a string to an UINT1 number with domain checking. A string
 * may contain leading or trailing spaces. 
 * It may be in floating
 * point format as long as is an integer (e.g. 1.0, 1E2 but not 1.1.
 * Returns TRUE if
 * number is an integer between UINT1_MIN and UINT1_MAX, FALSE
 * otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtUINT1(
	UINT1      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to an UINT1 */
{
	INT4 v;

	PRECOND(result != NULL);
	PRECOND(str != NULL);

	if (CnvrtINT4(&v, str) &&
	    ((INT4)UINT1_MIN) <= v && v <= ((INT4)UINT1_MAX))
	{
		*result = (UINT1)v;
		return TRUE;
	}
	return FALSE;
}

/* Converts a string to an REAL8 number.
 * This function converts
 * a string to an REAL8 number with domain checking. A string
 * may contain leading or trailing spaces. 
 * Returns TRUE if
 * number is a real number wtih an absolute value 
 * between REAL8_MIN and REAL8_MAX, FALSE
 * otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtREAL8(
	REAL8      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to an REAL8 */
{
	double v;
	char *endPtr;

	PRECOND(result != NULL);
	PRECOND(str != NULL);
	if (*str == '\0') return FALSE;
	errno=0;
	v = strtod(str, &endPtr);
	if ( errno == ERANGE || NotOnlySpace(endPtr)) 
	{
		return FALSE;
	}
	 *result = (REAL8)v;
	 return TRUE;
}

/* Converts a string to an REAL4 number.
 * This function converts
 * a string to an REAL8 number with domain checking. A string
 * may contain leading or trailing spaces. 
 * Returns TRUE if
 * number is a real number with an absolute value 
 * between REAL8_MIN and REAL4_MAX, FALSE
 * otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtREAL4(
	REAL4      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to an REAL8 */
{
	REAL8 v;

	PRECOND(result != NULL);
	PRECOND(str != NULL);

	if ( !CnvrtREAL8(&v, str) || fabs(v) > ((REAL8)REAL4_MAX) )
		return FALSE;
	*result = (REAL4)v;
	return TRUE;
}

/* Converts a string to a double number.
 * This function converts
 * a string to a double number with domain checking. A string
 * may contain leading or trailing spaces. 
 * Returns TRUE if
 * number is a real number in the double domain, FALSE
 * otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtDouble(
	double      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to a double */
{
	return CnvrtREAL8(result, str);
}

/* Converts a string to an int number
 * This function converts
 * a string to an int number with domain checking. A string
 * may contain leading or trailing spaces. 
 * It may be in floating
 * point format as long as is an integer (e.g. 1.0, 1E2 but not 1.1.
 * Returns TRUE if
 * number is an integer between INT2_MIN and INT2_MAX, FALSE
 * otherwise.
 * If FALSE is returned and errno is set to ERANGE then the conversion
 * error is like the ERANGE setting of strtod.
 */ 
BOOL CnvrtInt(
	int      *result, /* write-only. resulting number */
	const char *str)    /* string to convert to an int */
{
	INT4 v;

	PRECOND(result != NULL);
	PRECOND(str != NULL);

	if (CnvrtINT4(&v, str) &&
	    ((INT4)INT_MIN) <= v && v <= ((INT4)INT_MAX))
	{
		*result = (int)v;
		return TRUE;
	}
	return FALSE;
}

#ifdef TEST_MAIN
int main(void)
{
	char *tests[] = { "123", " 123", "", "999999999999999999", "9.12",
	                   "-5", "-5 ", "-3.89", "\t 7 \t",
	                   "--5", "-5t", "-3.89", "\t 7 \tx"};
	int i;
	UINT1 val1;
	INT4 val4;
	REAL8 val8;
	BOOL r1,r4,r8;
	for(i=0; i < ARRAY_SIZE(tests); i++)
	{
		r1 = CnvrtUINT1(&val1, tests[i]);
		r4 = CnvrtINT4(&val4, tests[i]);
		r8 = CnvrtREAL8(&val8, tests[i]);
		(void)printf("|%s| 1( %d ,%d) 4(%d,%d) 8(%d,%g)\n",tests[i],
		              r1,(int)val1,r4,(int)val4,r8,(double)val8);
	}
	return 0;
}
#endif /* TEST_MAIN */


/* converts a value with missing value detection
 * returns TRUE if legal value or mv, FALSE otherwise
 */
BOOL CnvrtValueMV(
	REAL8 *vNum,      /* write-only, value number or MV_REAL8
	                   * undefined if return value is 0.
	                   */
	const char *vStr,  /* value string */
	const char *mvStr, /* mv string */
	BOOL   number,     /* test on mv number ? */
	double mvDbl)      /* mv number, only used if number is TRUE */
{
	if (CnvrtDouble(vNum, vStr))
	{ /* value is a number
	   */
	  if (number && mvDbl == (*vNum))
	    SET_MV_REAL8(vNum);
	  return 1;
	}
	else
	{ /* value is a string */
	    SET_MV_REAL8(vNum);
	    return StrEq(vStr,mvStr);
        }
}
