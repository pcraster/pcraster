#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
# include "p_calc_list.h" 

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

static void AssignOrder(
	MAP_INT4 *order,		/* read-write output state map */
	int r,			/* row current cell */
	int c,			/* column current cell */
     	const MAP_UINT1 *ldd)		/* ldd map  */ 
{
	int 	i;
	INT4 highNBstreamOrder = 0;
	INT4 nrHighNBstreamOrder = 2; /* makes end condition easier */

	/* sum all upstream fluxes */
	FOR_ALL_LDD_NBS(i)
	{
		INT4 orderVal;
		UINT1 	lddVal;
		int rNB, cNB;
		rNB = RNeighbor(r, i);
		cNB = CNeighbor(c, i);

		if ( ldd->Get(&lddVal, rNB, cNB, ldd) &&
		    FlowsTo(lddVal, rNB, cNB, r, c)   )
		{ /* (rNB,cNB) flows in (r,c) */
		  /*  order of NB should be known: */
		  PRECOND(order->Get(&orderVal, rNB, cNB, order));
		  order->Get(&orderVal, rNB, cNB, order);
		  if (highNBstreamOrder == orderVal)
			nrHighNBstreamOrder++;
		  if (highNBstreamOrder < orderVal)
		  {
			highNBstreamOrder = orderVal;
			nrHighNBstreamOrder=1;
		  }
		}
	}
	/* works even if no upstream cells, due to proper initialixation */
	if (nrHighNBstreamOrder >= 2)
		highNBstreamOrder++;
	order->Put(highNBstreamOrder, r, c, order);
}

static int CalcOrder(
     MAP_INT4 *order,		/* Read-write output state map  */ 
     int r,			/* pit row coordinate */
     int c,			/* pit column coordinate */
     const MAP_UINT1 *ldd) 	/* ldd map */
{
	NODE 	*list;

	PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
	
	list = LinkChkNd(NULL, r, c); 	/* pit is 1st element */
	if(list == NULL)
		return 1;		/* memory allocation failed */

	while(list  != NULL) 
	{ 
		r = list->rowNr;	/* row of cell to check */
		c = list->colNr;	/* column of cell to check */

		if ( IS_VISITED(list) )
		{ /* it's catchment is processed 
		   * ups NBs contain order numbers
		   */
		  AssignOrder(order, r, c, ldd);
		  list = RemFromList(list);
		}
		else
		{ /* add ups NB cell to process first */
			if ((list = AddUpsNbsMarkFirst(list, ldd)) == NULL)
				return 1;
		}
	}
	return 0;
}

int StreamOrder(
     MAP_INT4 *order,		/* Read-write output flux map  */ 
     const MAP_UINT1 *ldd) 	/* ldd map */
{
     	UINT1 	lddVal;
     	int 	r, c , nrRows, nrCols;

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	/* function wants MAP->Get() to return FALSE in case of MV */
	ldd->SetGetTest(GET_MV_TEST, ldd);

	/* For each pit in the ldd map 
	 * traverse upstream to do the calculation
 	 */
	for(r = 0; r < nrRows; r ++)
	 for(c = 0; c < nrCols; c ++)
	{
	   	if(ldd->Get(&lddVal, r, c, ldd))
	   	{
	   	 if (lddVal == LDD_PIT)
	   	 {
     		  int res = CalcOrder(order, r, c, ldd);
     		  if (res)
     		  	return res;
		 }
		}
		else
		 order->PutMV(r,c, order);
	}
	return 0;			/* successful exited */ 
}
