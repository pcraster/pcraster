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
#include "p_calc_list.h"

/***************/
/* EXTERNALS   */
/***************/

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Performs clump for one cell.
 * Neighbors of cell are checked, if they have the same value and they
 * have the right location, they are linked to the list for checking
 * their neighbors.
 * Returns 1 when memory allocation fails, 0 otherwise.
 */
static int PerformClump(
	MAP_INT4 *out,		/* read-write output map */
	const MAP_INT4 *in,	/* input map */
	int r, 			/* rowNr of cell to be clumped */
	int c, 			/* colNr of cell to be clumped */
	INT4 currClumpNr,	/* unique new ID */
	int nrRows,		/* number of rows of maps */
	int nrCols)		/* number of columns */
/* Each call creates exactly one Clump */
{
	NODE 	*coordList;
	INT4 	currClumpValue;		/* current clump value */

	PRECOND(in->GetGetTest(in) == GET_MV_TEST);
	PRECOND(out->GetGetTest(out) == GET_MV_TEST);

	in->Get(&currClumpValue, r, c, in);
	coordList = LinkToList(NULL, r, c);
	if(coordList == NULL)
		return 1;
	while(coordList != NULL)
	{
		int 	i;
		int 	rowNr = coordList->rowNr;
		int 	colNr = coordList->colNr;

		/* remove a co-ordinate (r,c) entry from the list */ 
		coordList = RemFromList(coordList);
		out->Put(currClumpNr, r, c, out);
		for( i = 1; i <= NR_LDD_DIR; i++)
		{
			INT4 	clumpVal, outVal;
			int 	rNext = RNeighbor(rowNr, i);	
			int 	cNext = CNeighbor(colNr, i);	

			if(in->Get(&clumpVal, rNext, cNext, in) &&
			0 <= rNext && rNext < nrRows && 
			0 <= cNext && cNext < nrCols &&
			(i != LDD_PIT) &&
			(appDiagonal || Corner(i) == FALSE) &&
			clumpVal == currClumpValue)
			{
				if(!out->Get(&outVal, rNext, cNext, out))
				{
					NODE *tmp;
					tmp = LinkToList(coordList,
						rNext, cNext);
					if(tmp == NULL)
					{
						FreeList(coordList);
						return 1;
					}
					coordList = tmp;
					out->Put(currClumpNr, rNext,
					cNext, out);
				}	
			}
		}	
	}
	return 0;
}

/* Performs a clump on cells that are neighbors.
 * If option -d is used, then also corner neighbors of a cell
 * are considered, otherwise they are not for this cell.
 * Returns 0 if termination was successful, 1 otherwise.
 */
int Clump(
	MAP_INT4 *out,		/* read-write output map */ 
	const MAP_INT4 *in)	/* input map */
{     
	int 	r, c;
	INT4 	currClumpNr = 1;
	int 	nrRows = in->NrRows(in);
	int 	nrCols = in->NrCols(in);

	/* out has initial value MV */
	out->PutAllMV(out);

	in->SetGetTest(GET_MV_TEST, in);
	out->SetGetTest(GET_MV_TEST, out);

	for(r = 0; r < nrRows; r++)
	{
	    AppRowProgress(r);
	    for(c = 0; c < nrCols; c++)
	    {	
		INT4 inVal, outVal;
		if(in->Get(&inVal, r, c, in) &&
		out->Get(&outVal, r, c, out) == FALSE)
		{	
		    if(PerformClump(out, in, r, c, currClumpNr, nrRows,
		    					nrCols))
			return 1;
		    currClumpNr++;
		}
	    }
	}
	AppEndRowProgress();
	return 0;
}
/* End of file */
