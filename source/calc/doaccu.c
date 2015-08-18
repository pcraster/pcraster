#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"  /* AppRowProgress */

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
# include "p_calc_list.h" 
# include "accu.h"  /* FUNCTION */

/***************/
/* EXTERNALS   */
/***************/
BOOL accuCheckDomain;
/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Accumulates the amount of the current cell and its neighbors.
 * Returns 0  or 1 in case of a domain error
 */
static int Sum(
  MAP_REAL8 *state,    /* read-write output state map */
  MAP_REAL8 *flux,    /* read-write output flux map */
  int r,      /* row current cell */
  int c,      /* column current cell */
       const MAP_UINT1 *ldd,    /* ldd map  */ 
  const MAP_REAL8 * amount,  /* amount map */
       const MAP_REAL8 *value,    /* value map */
  ACCU_FUNC f)      /* function to perform */
{
  UINT1   lddVal;
  REAL8   fluxVal, amountVal, accamount, val;
  int   i;

  if(amount->Get(&amountVal, r, c, amount) &&
     value->Get(&val, r, c, value))
  {
    /* accamount initialize with its own amount value */
    accamount = amountVal;

    /* sum all upstream fluxes */
    FOR_ALL_LDD_NBS(i)
    {
      int rNB, cNB;
      rNB = RNeighbor(r, i);
      cNB = CNeighbor(c, i);

      if ( ldd->Get(&lddVal, rNB, cNB, ldd) &&
          FlowsTo(lddVal, rNB, cNB, r, c)   )
      {  /* (r,c) is in map and no MV */
        if(flux->Get(&fluxVal, rNB, cNB, flux))
          accamount += fluxVal;
        else
        /* neighbor has MV output value
         * no need to examine others.
         */ 
        {
            state->PutMV(r, c, state);
            flux->PutMV(r, c, flux);
            return 0;
        }
      }
    }

    /* Perform function on accamount */
    fluxVal = f(accamount, val);
    if (fluxVal < 0 && accuCheckDomain)
      return 1;
    flux->Put(fluxVal, r, c, flux);
    state->Put(accamount - fluxVal, r, c, state);
  }
  else
  {
    flux->PutMV(r, c, flux);
    state->PutMV(r,c, state);
  }
  return 0;
}

/* Calculates the output values for the output map.
 * CalcStateFlux assumes a spatial ldd map and an amount map to be present.
 * The search is done with "depth-first" strategy. (This means that when
 * a neighbor is found, that flows into the current cell, this neighbor
 * is put in front of the current list. First the catchment of this
 * neighbor is checked.)
 * Returns 1 if the function in case of memory error,
 *         2 in case of domain error
 *         0 otherwise.
 */
static int CalcStateFlux(
     MAP_REAL8 *state,    /* Read-write output state map  */ 
     MAP_REAL8 *flux,    /* Read-write output flux map  */ 
     int r,      /* pit row coordinate */
     int c,      /* pit column coordinate */
     const MAP_UINT1 *ldd,   /* ldd map */
     const MAP_REAL8 *amount,  /* amount map */
     const MAP_REAL8 *val,  /* value for function */
     ACCU_FUNC f)    /* function to perform */
{
  NODE   *list;

  PRECOND(ldd->GetGetTest(ldd) == GET_MV_TEST);
  
  list = LinkChkNd(NULL, r, c);   /* pit is 1st element */
  if(list == NULL)
    return 1;    /* memory allocation failed */

  while(list  != NULL) 
  { 
    r = list->rowNr;  /* row of cell to check */
    c = list->colNr;  /* column of cell to check */


    if ( IS_VISITED(list) )
    { /* it's catchment is processed 
       * ups NBs contain inflow
       */
      if (Sum(state, flux, r, c, ldd, amount, val, f))
        return 2;
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

/* Sums the amount of the catchment for each cell.
 * The amount values of the cells in the catchment are accumulated and
 * this is the output value. Assumes an UINT1 ldd map and a REAL8 
 * amount map to be present. (If the ldd map is unsound and 
 * has no pit, the output map will be filled with missing values.)
 * Writes the new state and the flux for each cell in output maps.
 * UNSOUND Ldd's will result in un-initialized cell on output maps
 * Returns 1 if the function in case of memory error,
 *         2 in case of domain error
 *         0 otherwise.
 */
int PerformAccu(
     MAP_REAL8 *state,    /* Read-write output state map  */ 
     MAP_REAL8 *flux,    /* Read-write output flux map  */ 
     const MAP_UINT1 *ldd,   /* ldd map */
     const MAP_REAL8 *amount,  /* amount map */
     const MAP_REAL8 *value,  /* value map for function*/
     ACCU_FUNC f)    /* function to perform */
{
  UINT1   lddVal;
  int   r, c , nrRows, nrCols;

  nrRows = ldd->NrRows(ldd);
  nrCols = ldd->NrCols(ldd);

  /* Fill outBuf with MV, this is the initial value */
  flux->PutAllMV(flux);
  state->PutAllMV(state);

  /* function wants MAP->Get() to return FALSE in case of MV */
  ldd->SetGetTest(GET_MV_TEST, ldd);
  flux->SetGetTest(GET_MV_TEST, flux);
  state->SetGetTest(GET_MV_TEST, state);
  amount->SetGetTest(GET_MV_TEST, amount);
  value->SetGetTest(GET_MV_TEST, value); 

  /* For every pit in the ldd map calculate the accumulated
    * amount for every cell in its catchment.
    */
  for(r = 0; r < nrRows; r ++)
   for(c = 0; c < nrCols; c ++)
  {
       if(ldd->Get(&lddVal, r, c, ldd))
       {
        if (lddVal == LDD_PIT)
        {
           int res;
           res = CalcStateFlux(state, flux, r, c, ldd, amount, value, f);
           if (res)
             return res;
     }
    }
    else
    {
     flux->PutMV(r,c, flux);
     state->PutMV(r,c, state);
    }
  }
  return 0;      /* successful exited */ 
}
