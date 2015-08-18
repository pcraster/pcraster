#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "mathx.h"      /* pow , sqrt */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
# include "p_calc_list.h"
# include "app.h"        /* appOutput, appUnitTrue */

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
     MAP_REAL8 *total,            /* write-only output map */ 
     int r,                  /* pit row coordinate */
     int c,                  /* pit column coordinate */
     const MAP_UINT1 *ldd,       /* ldd map */
     const MAP_REAL8 *amount)     /* amount map */
{
 NODE       *list = LinkChkNd(NULL, r, c); /* add pit */

 if(list == NULL)
     return 1;

 /* process pit (starting point) first */
 total->Put((REAL8)0,r,c,total);

 if (ReplaceFirstByUpsNbs(&list, ldd))
      return 1;

 while (list != NULL) {
    int rDS,cDS;
    REAL8 amountDS,totalDS;
    UINT1 l;

    c = list->colNr;
    r = list->rowNr;

    ldd->Get(&l,r,c,ldd);
    rDS = DownStrR(r,l);
    cDS = DownStrC(c,l);

    if ( amount->Get(&amountDS, rDS,cDS,amount) &&
         total->Get(&totalDS,   rDS,cDS,total))
       total->Put(totalDS+amountDS, r,c,total);
    else
       total->PutMV(r,c,total);
   if (ReplaceFirstByUpsNbs(&list, ldd))
      return 1;
 } /* eowhile */
 return 0;
}

/* Determines the distance from cell to first downstream nonzero point. 
 * Assumes a ldd map, amount map to be present.
 * UNSOUND ldd will result in uninitialized cells on total.
 * Returns 0 if termination is successful, non-zero otherwise 
 */
int Downstreamtotal(
     MAP_REAL8 *total,                  /* write-only output map  */ 
     const MAP_UINT1 *ldd,             /* ldd map            */
     const MAP_REAL8 *amount)            /* amount map */
{
    int       r, c;
    int       nrRows = ldd->NrRows(ldd);
    int       nrCols = ldd->NrCols(ldd);

    /* algorithm wants ldd->Get(), amount->Get()
      * to return FALSE if a value is a missing value
      */
    ldd->SetGetTest(GET_MV_TEST, ldd);
    amount->SetGetTest(GET_MV_TEST, amount);
    total->SetGetTest(GET_MV_TEST, total);

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
             if (CalcOut(total, r, c, ldd, amount))
               return 1;
        } else
              total->PutMV(r,c,total);
      }
    return 0;
}
