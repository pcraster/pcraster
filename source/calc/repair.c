#include "stddefx.h" 


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "p_calc_list.h" 	/* RNeighbor, CNeighbor */
#include <string.h>	/* memset */

/***************/
/* EXTERNALS   */
/***************/

/* look if RepairLdd modified something
 * repairLddModifiedMap is TRUE if the last call
 * to RepairLdd has modified its data, FALSE otherwise
 */
BOOL  repairLddModifiedMap = FALSE;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

static void PutRepair(
	UINT1_T code,
	int r,
	int c,
	MAP_UINT1 *ldd)
{
	ldd->Put(code, r, c, ldd);
	repairLddModifiedMap = TRUE;
}

static int MarkCatch(
	UINT1   **visitMap,  /* read write */
	int r,                /* row start */
	int c,                /* col start */
	MAP_UINT1 *ldd)        /* read write */
{
	NODE *list = LinkChkNd(NULL,r,c);
	if (list == NULL)
		return 1;
	while (list != NULL)
	{
		visitMap[list->rowNr][list->colNr] = 1;
		if (ReplaceFirstByUpsNbs(&list,ldd))
			return 1;
	}
	return 0;
}

/* NOTE that a cycle can have non-cyclic paths leading
 * to the cycle
 */
static int FixCycle(
	MAP_UINT1 *ldd,        /* read write */
	UINT1     **visitMap,  /* read write */
	int r,                /* row point in cycle */
	int c)                /* col point in cycle */
{
	UINT1 l;
	ldd->Get(&l, r, c, ldd);
	while (!visitMap[r][c]) 
	{
		int DSr, DSc;
		visitMap[r][c] = 1;
		PRECOND(l != LDD_PIT);

		DSr = DownStrR(r, l);
		DSc = DownStrC(c, l);
		if (visitMap[DSr][DSc])
		{
			PutRepair(LDD_PIT, r, c, ldd);
			return MarkCatch(visitMap,r,c,ldd);
		}
		PRECOND(ldd->Get(&l, DSr, DSc, ldd));
		ldd->Get(&l, DSr, DSc, ldd);
		r = DSr;
		c = DSc;
         }
         POSTCOND(FALSE); /* never reached */
         return 0;
}

/* Modifies LDD, repairs it when cells point to MV or outside map.  
 * When a cell does point to a MV or outside the map, the cell will
 * become a pit. Also modulos 10 for old ldd's
 * Returns 0, modifies the output map.
 */
int RepairLdd(
     MAP_UINT1 *ldd,			/* write-only output ldd  */ 
     const MAP_UINT1 *inLdd)		/* input ldd  */ 
{
	UINT1 lddCurr;		/* current and next ldd value */
	int nrRows = inLdd->NrRows(inLdd);
	int nrCols = inLdd->NrCols(inLdd);
	int r, c;
	UINT1 **visitMap = (UINT1 **)Malloc2d((size_t)nrRows,(size_t)nrCols,sizeof(UINT1));
	if (visitMap == NULL)
		return 1;
	(void)memset(visitMap[0], 0x0, (size_t)(nrRows*nrCols));

	/* algorithm wants inLdd->Get() to return FALSE in case of MV */
	inLdd->SetGetTest(GET_MV_TEST, inLdd);
	ldd->SetGetTest(GET_MV_TEST, ldd);

	repairLddModifiedMap = FALSE;

	/* copy matrix and check for old and invalid codes */	
	for (r = 0; r < nrRows; r++)
	  for(c = 0; c < nrCols; c++)
	     if(inLdd->Get(&lddCurr, r, c, inLdd))
		{	/* determine downstream neighbor 
		         */
			if (!IS_VALID_LDD_CODE(lddCurr))
			{ /* old code */
				lddCurr %= (UINT1)10;
	                        repairLddModifiedMap = TRUE;
			}
			if (!IS_VALID_LDD_CODE(lddCurr))
			{  /* 0 */
				ldd->PutMV(r,c,ldd);
	                        repairLddModifiedMap = TRUE;
			}
			else
				ldd->Put(lddCurr, r, c, ldd);
		}
		else
			ldd->PutMV(r,c,ldd);
	
	/* 
	 * now repair 
	 * - make pits off cell that flow outside
	 * - mark all valid paths by travelling upstream from 
	 *   pit values
	 */
	for (r = 0; r < nrRows; r++)
	  for(c = 0; c < nrCols; c++)
	     if( ldd->Get(&lddCurr, r, c, ldd))
	     {
	     	int rDS = DownStrR(r, lddCurr);
	     	int cDS = DownStrC(c, lddCurr);
	     	UINT1 lDS;
	     	if (! ldd->Get(&lDS, rDS, cDS, ldd))
	     	{ /* (r,c) flows outside map */
			PutRepair(LDD_PIT,r,c,ldd);
			lddCurr = LDD_PIT;
	     	}
	     	if (lddCurr == LDD_PIT)
	     		if (MarkCatch(visitMap, r, c, ldd))
	     			goto error;
	     }

	/* all unmarked points
	 * are part of a cycle
	 */
	for (r = 0; r < nrRows; r++)
	  for(c = 0; c < nrCols; c++)
	     if( ldd->Get(&lddCurr, r, c, ldd) &&
	         (!visitMap[r][c])
	       )
		if (FixCycle(ldd, visitMap, r, c))
			goto error;

	Free2d((void **)visitMap, (size_t)nrRows);
	return 0;
error:
	Free2d((void **)visitMap, (size_t)nrRows);
	return 1;
}

/* Cuts ldd by boolean and then repairs it
 * Cuts ldd by boolean and then repairs it by
 * calling LddRepair
 * returns
 * return value of LddRepair
 */
int MaskLdd(
     MAP_UINT1 *ldd,			/* write-only output ldd  */ 
     const MAP_UINT1 *inLdd,		/* input ldd  */ 
     const MAP_UINT1 *mask)		/* mask  */ 
{
	UINT1 lddCurr,m;	/* current and next ldd value */
	int nrRows = inLdd->NrRows(inLdd);
	int nrCols = inLdd->NrCols(inLdd);
	int r, c;

	/* algorithm wants inLdd->Get() to return FALSE in case of MV */
	inLdd->SetGetTest(GET_MV_TEST, inLdd);

	/* for every cell check where it flows to */	
	for (r = 0; r < nrRows; r++)
	    for(c = 0; c < nrCols; c++)
	    { 	
		if(inLdd->Get(&lddCurr, r, c, inLdd)
		   && mask->Get(&m,r,c,mask)
		   && m == 1 )
			ldd->Put(lddCurr, r, c, ldd);
		else
			ldd->PutMV(r, c, ldd);
	    }
	return RepairLdd(ldd, ldd);
}
