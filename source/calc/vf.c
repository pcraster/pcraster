#include "stddefx.h"

#include "calc.h" /* appUnitTrue, appOutput */
#include "app.h" /* appUnitTrue, appOutput */


/* gradx(): calculates the directional derivative (gradient)
 * of a scalar field f(x,y) in the x-wise direction:
 * gradx(f) = d/dx(f)
 * */


extern int vf_gradx(MAP_REAL8 *result, const MAP_REAL8 *scalar)
{
    int r,c,nrows,ncols;
    double dx,value,right,left;
    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    dx     = scalar->CellLength(scalar);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
   {
    if ( scalar->Get(&value, r, c, scalar) )      /* - v - */
    {
      if ( scalar->Get(&right, r, c+1, scalar) &&    /* - - r */
           scalar->Get(&left,  r, c-1, scalar) )    /* l - - */
      {
        /* use second order central difference to get the derivative */
        result->Put(
          (right-left) / (2*dx),
          r,c, result);
      }
      else if ( scalar->Get(&right, r, c+1, scalar) ) /* - - r */
                              /* 0 - - */
      {
        /* use first order finite difference to get the derivative */
        result->Put(
          (right-value) / dx,
          r,c, result);
      }
      else if ( scalar->Get(&left, r, c-1, scalar) ) /* - - 0 */
                              /* l - - */
      {
        /* use first order finite difference to get the derivative */
        result->Put(
          (value-left) / dx,
          r,c, result);
      }
      else
      {
        /* cell is isolated, the derivative is zero */
        result->Put(
          0,
          r,c, result);
      }
    }
    else                        /* - 0 - */
      result->PutMV(r,c, result);
   }
   return 0;
}

/* grady(): calculates the directional derivative (gradient)
 * of a scalar field f(x,y) in the y-wise direction:
 * grady(f) = d/dy(f)
 * Uses regularization, no mass conservative!
 * */
extern int vf_grady(MAP_REAL8 *result, const MAP_REAL8 *scalar)
{
    int r,c,nrows,ncols;
    double dy,value,up,down;
    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    dy     = scalar->CellLength(scalar);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
   {
    if ( scalar->Get(&value, r, c, scalar) )
    {
      if ( scalar->Get(&up, r-1, c, scalar) &&
           scalar->Get(&down, r+1, c, scalar) )
      {
        /* use second order central difference to get the derivative */
        result->Put(
          (up-down) / (2*dy),
          r,c, result);
      }
      else if ( scalar->Get(&up, r-1, c, scalar) )
      {
        /* use first order finite difference to get the derivative */
        result->Put(
          (up-value) / dy,
          r,c, result);
      }
      else if ( scalar->Get(&down, r+1, c, scalar) )
      {
        /* use first order finite difference to get the derivative */
        result->Put(
          (value-down) / dy,
          r,c, result);
      }
      else
      {
        /* cell is isolated, the derivative is zero */
        result->Put(
          0,
          r,c, result);
      }
    }
    else                        /* - 0 - */
      result->PutMV(r,c, result);
   }
   return 0;
}

/* divergence(): calculates the divergence of a vector field v=(v1, v2),
 * where v1 and v2 are the x- and y-wise vector components:
 * divergence(v) = Del dot v = d/dx(v1) + d/dy(v2)
 * Includes regularization, it is NOT mass conservative.
 * */

int vf_divergence(
  MAP_REAL8 *result,
  const MAP_REAL8 *vectorfieldx,
  const MAP_REAL8 *vectorfieldy)
{
    int r,c,nrows,ncols;
    double dx,dy;
    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    dx     = vectorfieldx->CellLength(vectorfieldx);
    dy     = vectorfieldy->CellLength(vectorfieldy);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
   {
    double valuex, valuey,right,left,up,down,res;
    if ( vectorfieldx->Get(&valuex, r, c, vectorfieldx) &&
       vectorfieldy->Get(&valuey, r, c, vectorfieldy) )
    {
      res = 0;
      if ( vectorfieldx->Get(&right, r, c+1, vectorfieldx) &&
           vectorfieldx->Get(&left,  r, c-1, vectorfieldx) )
      {
        res = (right-left) / (2*dx);
      }
      else if ( vectorfieldx->Get(&right, r, c+1, vectorfieldx) )
      {
        res = (right) / dx;
      }
      else if ( vectorfieldx->Get(&left, r, c-1, vectorfieldx) )
      {
        res = (-left) / dx;
      }

      if ( vectorfieldy->Get(&up, r-1, c, vectorfieldy) &&
           vectorfieldy->Get(&down, r+1, c, vectorfieldy) )
      {
        res = res + (up-down) / (2*dy);
      }
      else if ( vectorfieldy->Get(&up, r-1, c, vectorfieldy) )
      {
        res = res + (up) / dy;
      }
      else if ( vectorfieldy->Get(&down, r+1, c, vectorfieldy) )
      {
        res = res + (-down) / dy;
      }

      result->Put(res, r, c, result);
    }
    else
      result->PutMV(r,c, result);
  }

    return 0;
}


/* diver(): calculates the divergence of a vector field v(x,y) = (v1, v2),
 * where v1 and v2 are the x- and y-wise vector components at location (x,y):
 * divergence(v) = Del dot v = d(v1)/dx + d(v2)/dy
 * No regularization, it is mass conservative.
 * */
int vf_diver(
    MAP_REAL8 *result,
    const MAP_REAL8 *vectorfieldx,
    const MAP_REAL8 *vectorfieldy,
    const MAP_REAL8 *deltax,
    const MAP_REAL8 *deltay)
{
    int r,c,nrows,ncols;
    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
     {
        double res,value,valueLeft,valueRight,valueUp,valueDown,dx,dy;
        if ( vectorfieldx->Get(&value,r,c,vectorfieldx) &&
             vectorfieldx->Get(&valueLeft,r,c-1,vectorfieldx) &&
             vectorfieldx->Get(&valueRight,r,c+1,vectorfieldx) ) {
             deltax->Get(&dx,r,c,deltax);
             res = (valueRight-valueLeft)/(2*dx);
             }
        else if ( vectorfieldx->Get(&value,r,c,vectorfieldx) &&
              vectorfieldx->Get(&valueLeft,r,c-1,vectorfieldx) ) {
              deltax->Get(&dx,r,c,deltax);
              res = (value-valueLeft)/dx;
        }
        else if ( vectorfieldx->Get(&value,r,c,vectorfieldx) &&
              vectorfieldx->Get(&valueRight,r,c+1,vectorfieldx) ) {
              deltax->Get(&dx,r,c,deltax);
              res = (valueRight-value)/dx;
        }
        else
            res = 0;
        if ( vectorfieldy->Get(&value,r,c,vectorfieldy) &&
             vectorfieldy->Get(&valueUp,r+1,c,vectorfieldy) &&
             vectorfieldy->Get(&valueDown,r+1,c,vectorfieldy) ) {
            deltay->Get(&dy,r,c,deltay);
            result->Put(
            res + (valueUp-valueDown)/(2*dy),
            r,c,result);
        }
        else if ( vectorfieldy->Get(&value,r,c,vectorfieldy) &&
              vectorfieldy->Get(&valueUp,r+1,c,vectorfieldy) ) {
            deltay->Get(&dy,r,c,deltay);
            result->Put(
            res + (valueUp-value)/dy,
            r,c,result);
        }
        else if ( vectorfieldy->Get(&value,r,c,vectorfieldy) &&
              vectorfieldy->Get(&valueDown,r-1,c,vectorfieldy) ) {
            deltay->Get(&dy,r,c,deltay);
            result->Put(
            res + (value-valueDown)/dy,
            r,c,result);
        }
        else
            result->PutMV(r,c,result);
    }

    return 0;
}


/* lax(): a window average filter similar to Lax scheme, used to avoid
 * numerical instability
 * */
int vf_lax(MAP_REAL8 *result,
  MAP_REAL8 const* input,
  MAP_REAL8 const* fractMap)
{
    int r,c,nrows,ncols;
    double cellv,othercellv,gg,hh,frac;

    // get the nonspatial
    fractMap->Get(&frac, 0, 0, fractMap);

    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
   {
    gg = 0;
    hh = 0;
    if ( input->Get(&cellv, r, c, input) &&
         fractMap->Get(&frac, r, c, fractMap))
    {
       if ( input->Get(&othercellv, r-1,c-1, input) )
       {
        gg = gg + 2*othercellv;
        hh = hh + 2;
       }
        if ( input->Get(&othercellv, r-1,c+0, input) )
        {
        gg = gg + 3*othercellv;
        hh = hh + 3;
        }
        if ( input->Get(&othercellv, r-1,c+1, input) )
        {
          gg = gg + 2*othercellv;
        hh = hh + 2;
        }
        if ( input->Get(&othercellv, r+0,c-1, input) )
        {
        gg = gg + 3*othercellv;
        hh = hh + 3;
        }
        if ( input->Get(&othercellv, r+0,c+1, input) )
        {
        gg = gg + 3*othercellv;
        hh = hh + 3;
        }
        if ( input->Get(&othercellv, r+1,c-1, input) )
        {
        gg = gg + 2*othercellv;
        hh = hh + 2;
        }
        if ( input->Get(&othercellv, r+1,c+0, input) )
        {
        gg = gg + 3*othercellv;
        hh = hh + 3;
        }
        if ( input->Get(&othercellv, r+1,c+1, input) )
        {
        gg = gg + 2*othercellv;
        hh = hh + 2;
        }
        result->Put((1-frac) * cellv + frac * (gg/hh), r, c, result);
      }
    else
      result->PutMV(r,c,result);
   }
   return 0;
}

/* laplacian(): calculates the laplacian (div dot grad)
 * of a scalar field f(x,y): laplacian(f) = div · grad(f) = d^2(f)/dx^2
 * */
extern int vf_laplacian(MAP_REAL8 *result,
                  const MAP_REAL8 *scalar)
{
    int r,c,nrows,ncols;
    double dx,value,neighbour,gg;
    nrows  = result->NrRows(result);
    ncols  = result->NrCols(result);
    dx     = scalar->CellLength(scalar);
    for (r=0; r < nrows; r++)
     for (c=0; c < ncols; c++)
   {
    gg = 0;
    if ( scalar->Get(&value, r+0,c+0, scalar) )
    {
      if ( scalar->Get(&neighbour, r-1,c-1, scalar) )
      {
      gg = gg + 2*neighbour;
      }
      else  gg = gg + 2*value;
      if ( scalar->Get(&neighbour, r-1,c+0, scalar) )
      {
      gg = gg + 3*neighbour;
      }
      else  gg = gg + 3*value;
      if ( scalar->Get(&neighbour, r-1,c+1, scalar) )
      {
      gg = gg + 2*neighbour;
      }
      else  gg = gg + 2*value;
      if ( scalar->Get(&neighbour, r+0,c-1, scalar) )
      {
      gg = gg + 3*neighbour;
      }
      else  gg = gg + 3*value;
      if ( scalar->Get(&neighbour, r+0,c+1, scalar) )
      {
      gg = gg + 3*neighbour;
      }
      else  gg = gg + 3*value;
      if ( scalar->Get(&neighbour, r+1,c-1, scalar) )
      {
      gg = gg + 2*neighbour;
      }
      else  gg = gg + 2*value;
      if ( scalar->Get(&neighbour, r+1,c+0, scalar) )
      {
      gg = gg + 3*neighbour;
      }
      else  gg = gg + 3*value;
      if ( scalar->Get(&neighbour, r+1,c+1, scalar) )
      {
      gg = gg + 2*neighbour;
      }
      else  gg = gg + 2*value;
      result->Put(
      (gg-20*value) / (dx*dx),
      r,c, result);
    }
    else
      result->PutMV(r,c, result);
   }
   return 0;
}
