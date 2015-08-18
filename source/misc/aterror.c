#include "stddefx.h" 

/**************************************************************************/
/*  aterror.c                                                             */ 
/*    A sort of atexit in case of an error                                */
/*                                                                        */
/*                                                                        */
/**************************************************************************/
#include "misc.h"

/********/
/* USES */
/********/

/***************/
/* EXTERNALS   */
/***************/


/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
/* type of functions used by AtError (LIBRARY_INTERNAL)
 */
typedef void (*FUNC)(void);
#define MAX_FUNCS 64


/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
static FUNC funcs[MAX_FUNCS];
/*  boolean to identify is funcs array is initialized
 *  properly
 */
static BOOL firstTimeCalled = TRUE;

/******************/
/* IMPLEMENTATION */
/******************/

/* acts like atexit */
int AtError(void (*func)(void))
{
	size_t i;
	if (firstTimeCalled)
	{
		for (i = 0; i < MAX_FUNCS; i++)
			funcs[i] = NULL;
		firstTimeCalled = FALSE;
	}
	for(i=0; i < MAX_FUNCS ; i++)
		if (funcs[i] == NULL)
		{ /* free slot */
			funcs[i] = func;
			break;
		}
	PRECOND(i != MAX_FUNCS);
	return(i == MAX_FUNCS);
}

int NoLongerAtError(void (*func)(void))
{
	size_t i;
	PRECOND(!firstTimeCalled);
	for(i=0; i < MAX_FUNCS ; i++)
		if (funcs[i] == func)
		{ /* give slot free */
			funcs[i] = NULL;
			break;
		}
	return(i == MAX_FUNCS);
}

void ExecAtError(void)
{
	size_t i;
	if(firstTimeCalled) /* then there's something */
		for(i=0; i < MAX_FUNCS ; i++)
			if (funcs[i] != NULL)
			{
				funcs[i]();
				funcs[i] = NULL;
			}
}
