#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h>
#include "misc.h"
#include "move.h"
#include "table.h"

/* apps. called */

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

int MoveColumn(
	const char *outputName, 
	const char *inputName, 
	size_t colNr)
{
	LOOK_UP_TABLE *t;
	FILE *f;
	size_t r;

	PRECOND(outputName != NULL);
	PRECOND(inputName != NULL);
	PRECOND(colNr > 0 );

        colNr--;

	f = fopen(inputName,"r");
	if (f == NULL)
		return RetError(1,"Failure to open '%s'", inputName);
	t = ReadLookupTable(f,NULL,(size_t)0, VS_UNDEFINED);
	fclose(f);
	if (t == NULL)
		return RetError(1,"While reading '%s'", inputName);
	if (t->nrKeys < colNr)
	{
		Error("Can't move columns '%d', '%s' has only '%d' columns", 
		 colNr+1,inputName, t->nrKeys+1);
		FreeLookupTable(t);
		return 1;
	}
	for (r = 0; r < t->nrRecords; r++)
	{ 
		LOOK_UP_KEY k = t->records[r][colNr];
		memmove(t->records[r]+colNr, t->records[r]+(colNr+1),
		        (t->nrKeys-colNr)*sizeof(LOOK_UP_KEY));
		t->records[r][t->nrKeys] = k;
	}
	return WriteLookupTable(outputName, t);
}
