#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "mathx.h"	/* pow , sqrt */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
# include "p_calc_list.h"
# include "app.h"  	/* appOutput, appUnitTrue */

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

/*  Calculates the output values for the output map.
 *  CalcOut assumes a ldd.map and a points.map from UINT1 type present.
 *  The search is done with "breadth-first" strategy. (This means that
 *  all neighbors that flow into the current cell, area put in front
 *  of the current list.)
 *  Returns integer 0 if exit is successful, 1 otherwise.
 */
static int CalcOut(
     MAP_REAL8 *out,		/* write-only output map */ 
     int r,			/* pit row coordinate */
     int c,			/* pit column coordinate */
     const MAP_UINT1 *ldd, 	/* ldd map */
     const MAP_UINT1 *points, 	/* points map */
     const MAP_REAL8 *friction,	/* friction map */
     REAL8  w,                  /* part of friction formula 
                                 * non-diagonal case
                                 */
     REAL8  dw,                 /* part of friction formula 
                                 * diagonal case
                                 */
     BOOL useWeightedFriction)
{
	NODE 	*list = LinkChkNd(NULL, r, c); /* add pit */
	UINT1 	pntVal;

     	if(list == NULL)
     		return 1;

	/* process pit (starting point) first
	 * in this way DS is always defined
	 * in while loop
	 */
	points->Get(&pntVal, r,c,points);
	if (pntVal == 1)
	 out->Put((REAL8)0,r,c,out);
	else
	 out->PutMV(r,c,out);
	if (ReplaceFirstByUpsNbs(&list, ldd))
		return 1;

    	while (list != NULL)
	{   
	 c = list->colNr;
	 r = list->rowNr;

	 points->Get(&pntVal, r,c,points);
	 switch(pntVal) {
	  case MV_UINT1:
		out->PutMV(r,c,out);
		break;
	  case 1:
	        /* new start point */
		out->Put((REAL8)0,r,c,out);
		break;
	  default: PRECOND(pntVal == 0); /* only bools allowed */
	       {
	        int rDS,cDS;
	        REAL8 f,fDS, oDS;
	        UINT1 l;
	        ldd->Get(&l,r,c,ldd);
	        rDS = DownStrR(r,l);
	        cDS = DownStrC(c,l);
	        if ( friction->Get(&f, r,c,friction) &&
	             friction->Get(&fDS, rDS,cDS,friction) &&
	                  out->Get(&oDS, rDS,cDS,out)
	           )
	        {
	        	REAL8 o;
            if (useWeightedFriction)
	        	  o = oDS + ((Corner(l)?dw:w)*(fDS+f));
            else
              o = oDS + ((Corner(l)?Diagonal():Side())*(f));
	        	if (f < 0)
	        	{
			 FreeList(list);
			 return	RetError(1,"ldddist: Domain error on parameters");
	        	}
	                out->Put(o, r,c,out);
	        }
	        else
	                out->PutMV(r,c,out);
	       }
	    } /* eoswitch */
	    if (ReplaceFirstByUpsNbs(&list, ldd))
		return 1;
	} /* eowhile */
	return 0;
}

/* Determines the distance from cell to first downstream nonzero point. 
 * Assumes a ldd map, friction map and points map to be present.
 * UNSOUND ldd will result in uninitialized cells on out.
 * Returns 0 if termination is successful, non-zero otherwise 
 */
int Ldddist(
     MAP_REAL8 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd, 		/* ldd map		*/
     const MAP_UINT1 *points,		/* boolean points map */
     const MAP_REAL8 *friction, /* friction map */
     BOOL  useWeightedFriction) /* true -> old ldddist, false in traveltime use */
{
     	int 	r, c;
     	int 	nrRows = ldd->NrRows(ldd);
     	int 	nrCols = ldd->NrCols(ldd);
     	/* dist * (fDS+f) / 2 = w * (fDS*f)
     	 */
     	REAL8 	w = Side() / 2; 
     	REAL8 	dw = Diagonal() / 2; 

	/* algorithm wants ldd->Get(), friction->Get() and points->Get()
 	 * to return FALSE if a value is a missing value
 	 */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	points->SetGetTest(GET_MV_TEST, points);
	friction->SetGetTest(GET_MV_TEST, friction);
	out->SetGetTest(GET_MV_TEST, out);

	/* For every pit in the ldd map calculate the distance to first
	 * down-stream nonzero point for every cell in the catchment.
	 */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
		{
			UINT1 lddVal;
			if(ldd->Get(&lddVal, r, c, ldd))
			{
			  if (lddVal == LDD_PIT)
     			   if (CalcOut(out, r, c, ldd, points, friction,w,dw, useWeightedFriction))
				return 1;
			}
			else
				out->PutMV(r,c,out);
		}
	return 0;
}
