#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"  /* sqrt, sqr */
#include "misc.h"
#include "calc.h"
#include "app.h"

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

int Move(
       MAP_UINT1 *result,   /* Read-write output map  */
       const MAP_UINT1 *in,
       const MAP_REAL8 *x,  /* Digital Elevation Model */
       const MAP_REAL8 *y)  /* Digital Elevation Model */
{
  REAL8   xColVal, yRowVal;
  UINT1   inVal;
  int   nrRows, nrCols, r, c;

  x->SetGetTest(GET_MV_TEST, x);
  y->SetGetTest(GET_MV_TEST, y);
  in->SetGetTest(GET_MV_TEST, in);
  result->SetGetTest(GET_NO_MV_TEST, result);
  nrRows = in->NrRows(in);
  nrCols = in->NrCols(in);

  result->PutAllMV(result);

  for (r = 0; r < nrRows; r++)
    for (c = 0; c < nrCols; c++)
  {
    if ( in->Get(&inVal, r, c, in) &&
         x->Get(&xColVal, r, c, x)    &&
         y->Get(&yRowVal, r, c, y)   )
    {
      int yRowDest = r +
          (int)floor(YProjectionFactor()*(yRowVal/Side()));
      int xColDest = c +(int)floor(xColVal/Side());
      UINT1 resVal;

      if (result->Get(&resVal, yRowDest, xColDest, result))
        /* in the map */
         if (resVal != 1) /* then it's mv or false */
               result->Put(inVal, yRowDest, xColDest, result);
     }
  }
  return 0;
}

static INT4 getShift(
    const MAP_REAL8 *m)
{
  REAL8 r;
  m->Get(&r, 0, 0, m);
  return POSSIBLE_DATA_LOSS(INT4,r);
}

int Shift(
       MAP_REAL8       *result,   /* Read-write output map  */
       const MAP_REAL8 *in,
       const MAP_REAL8 *y,  /* Integer Northing */
       const MAP_REAL8 *x)  /* Integer Easting */
{
  INT4    xColVal, yRowVal;
  REAL8   inVal;
  int   nrRows, nrCols, r, c;

  // nonspatials
  xColVal=getShift(x);
  yRowVal=getShift(y);

  in->SetGetTest(GET_MV_TEST, in);
  result->SetGetTest(GET_NO_MV_TEST, result);
  nrRows = in->NrRows(in);
  nrCols = in->NrCols(in);

  result->PutAllMV(result);

  for (r = 0; r < nrRows; r++)
    for (c = 0; c < nrCols; c++)
  {
      int yRowSrc = r + yRowVal;
      int xColSrc = c + xColVal;
      if (in->Get(&inVal, yRowSrc, xColSrc, in))
           result->Put(inVal, r, c, result);
  }
  return 0;
}

int Shift0(
       MAP_REAL8       *result,   /* Read-write output map  */
       const MAP_REAL8 *in,
       const MAP_REAL8  *y,  /* Integer Northing */
       const MAP_REAL8  *x)  /* Integer Easting */
{
  INT4    xColVal, yRowVal;
  REAL8   inVal;
  int   nrRows, nrCols, r, c;

  // nonspatials
  xColVal=getShift(x);
  yRowVal=getShift(y);

  in->SetGetTest(GET_MV_TEST, in);
  result->SetGetTest(GET_NO_MV_TEST, result);
  nrRows = in->NrRows(in);
  nrCols = in->NrCols(in);

  result->PutAllMV(result);

  for (r = 0; r < nrRows; r++)
    for (c = 0; c < nrCols; c++)
  {
      int yRowSrc  = r + yRowVal;
      int xColSrc  = c + xColVal;
      if (in->Get(&inVal, yRowSrc, xColSrc, in))
           result->Put(inVal, r, c, result);
      else
           result->Put(0.0, r, c, result);
      /*
       * BOOL 
        if (yRowDest >= 0 && yRowDest < nrRows &&
            xColVal  >= 0 && xColDest < nrCols)
      */
  }
  return 0;
}
