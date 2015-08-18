#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "table.h"

/* global header (opt.) and test's prototypes "" */

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

/* Frees table and deallocates the memory.
 */
void FreeLookupTable(LOOK_UP_TABLE *t)	/* table to deallocate */
{
	PRECOND(t != NULL);

	if(t->records != NULL)
		Free2d((void **)t->records, t->nrRecords);
	Free(t->keyVs);
	Free(t);
}

/* allocates the records of a lookup tables
 * Allocates records as an array of nrRecords arrays of nrKeys+1 keys
 * return 0 on succes, non-zero on failure
 */
int AllocLookupTable(
	LOOK_UP_TABLE *t) /* with nrRecords and nrKeys defined */
{
	PRECOND(t->nrRecords > 0);
	t->records = (LOOK_UP_KEY **)Malloc2d(t->nrRecords,t->nrKeys+1,sizeof(LOOK_UP_KEY));
	t->keyVs = (CSF_VS *)ChkMalloc((t->nrKeys+1)*sizeof(CSF_VS));
	return t->records == NULL || t->keyVs == NULL;
}
