#include "stddefx.h" 
/*
 */
/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "app.h"

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

/* sum values of upstream neighbours
 * Returns always 0
 */
int Upstream(
     MAP_REAL8 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd, 		/* ldd map		*/
     const MAP_REAL8 *in)		/* value map */
{
	UINT1 	l;
     	REAL8 	v;
     	int 	r, c;
     	int 	nrRows = ldd->NrRows(ldd);
     	int 	nrCols = ldd->NrCols(ldd);


	/* algorithm wants ldd->Get(), value->Get() and points->Get()
 	 * to return FALSE if a value is a missing value
 	 */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	in->SetGetTest(GET_MV_TEST, in);

	for(r = 0; r < nrRows; r++)
	{
	 AppRowProgress(r);
	 for(c = 0; c < nrCols; c++)
	{
	 if(ldd->Get(&l, r, c, ldd)&&
	   (in->Get(&v, r, c, in))
	   )
	   {
	    REAL8 sum = 0;
	    int d;
	    FOR_ALL_LDD_NBS(d)
	    {
		int rNext = RNeighbor(r, d);
		int cNext = CNeighbor(c, d); 
	        if( ldd->Get(&l, rNext, cNext, ldd)&&
		    FlowsTo(l, rNext, cNext, r, c) )
		    {
		     if (in->Get(&v,rNext,cNext,in))
		     	sum += v;
		     else
		     {  /* mv in in-map make it's downstream MV 
		         */
		         goto putMV;
		     }
		    }
	    } /* eofor all directions */
	    out->Put(sum,r, c, out);
	  }
	 else
putMV:
	  out->PutMV(r, c, out);
	} /* eofor cols */
       } /* eofor rows */
	AppEndRowProgress();
	return 0;			/* successful terminated */ 
}
