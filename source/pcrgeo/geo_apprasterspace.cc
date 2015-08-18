#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_APPRASTERSPACE
#include "geo_apprasterspace.h"
#define INCLUDED_GEO_APPRASTERSPACE
#endif

#ifndef INCLUDED_APP
#include "app.h"
#define INCLUDED_APP
#endif

geo::AppRasterSpace::AppRasterSpace(const RasterSpace& rs):
  d_rs(rs),d_trueCoords(appUnitTrue != 0)
{
  switch(appCoord) {
    case APP_UL: d_xInc=d_yInc=0.0; break;
    case APP_C : d_xInc=d_yInc=0.5; break;
    case APP_LR: d_xInc=d_yInc=1.0; break;
  }
}

//! convert (row,col) to x, y coordinates
/*! conversion according global settings
 <UL>
  <li>\a row  Row number (relates to y position).
  <li>\a col  Column number (relates to x position).
  <li>\a x  write-only. Returns x of  output co-ordinate
  <li>\a y  write-only. Returns y of output co-ordinate
 </UL>
 */
void geo::AppRasterSpace::getCoords(
  size_t row,
  size_t col,
  double &x,
  double &y) const
{
  x = col + d_xInc;
  y = row + d_yInc;
  if(d_trueCoords)
    d_rs.rowCol2Coords(y,x,x,y);
}

//! the cellsize or 1 in case of --unitcell
double geo::AppRasterSpace::cellSize() const
{
  if (d_trueCoords)
    return d_rs.cellSize();
  return 1;
}

//! the cell-area or 1 in case of --unitcell
double geo::AppRasterSpace::cellArea() const
{
  return cellSize()*cellSize();
}
