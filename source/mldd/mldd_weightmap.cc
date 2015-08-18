#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_MLDD_WEIGHTMAP
#include "mldd_weightmap.h"
#define INCLUDED_MLDD_WEIGHTMAP
#endif

// Library headers.
#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_MLDD_DAGRASTER
#include "mldd_dagraster.h"
#define INCLUDED_MLDD_DAGRASTER
#endif



/*!
  \file
  This file contains the implementation of the WeightMap class.
*/



//------------------------------------------------------------------------------

/*
namespace mldd {

class WeightMapPrivate
{
public:

  WeightMapPrivate()
  {
  }

  ~WeightMapPrivate()
  {
  }

};

} // namespace mldd
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC WEIGHTMAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF WEIGHTMAP MEMBERS
//------------------------------------------------------------------------------

mldd::WeightMap::WeightMap(
    const DagRaster& dr,
    const geo::SimpleRaster<REAL4>& dem):
  d_dr(dr),
  d_dem(dem)
{
}



mldd::WeightMap::~WeightMap()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
mldd::WeightMap& mldd::WeightMap::operator=(const WeightMap& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
mldd::WeightMap::WeightMap(const WeightMap& rhs)
{
}
*/

/*!
 * if no computation due to MV then mvMark()
 * is returned as value
 */
double mldd::WeightMap::get(const Edge& e) const
{
  geo::CellLoc s(e.source());
  geo::CellLoc d(e.target()); // downstream

  // weighted slope
  if (d_dr.nrOutflowNB(s)==1)
    return 1; // no weight only one outflow

  double dist[2] = { 1, std::sqrt(2.0) };
  bool   diagonal= d.row()!=s.row() || d.col()!=s.col();
  if (d_dem.mv(d)||d_dem.mv(s))
    return mvMark(); // mv prohibit calculation
  double slopeS  =(std::max<double>(d_dem[s]-d_dem[d],0))/dist[diagonal];
  double sumS(0);
  for(OutEdgeIterator oe=d_dr.beginOutEdge(s);oe.any();++oe) {
    geo::CellLoc d=(*oe).target(); // downstream
    if (d_dem.mv(s))
      continue;
    diagonal= d.row()!=s.row() || d.col()!=s.col();
    sumS+=(d_dem[s]-d_dem[d])/dist[diagonal];
  }

  // if flat, divide equally
  if (sumS == 0)
   return 1.0/d_dr.nrOutflowNB(s);

  // as fraction of total slope
  return slopeS/sumS;
}

void mldd::WeightMap::fillDirMap(
    geo::NB::Code dir,
    REAL4         *map) const
{
  geo::RasterDim rd(d_dr.rasterDim());
  for(geo::LinearLoc i=0; i < rd.nrCells(); ++i) {
    pcr::setMV(map[i]);
    if (d_dr.hasOutflowDir(i,dir)) {
      geo::CellLoc s(rd.convert(i));
      geo::CellLoc d(rd.target<geo::NB>(s,dir));

      double v=get(Edge(s,d));
      if (v!=mvMark())
        map[i]=v;
    }
  }
}

//! \todo get rid of
geo::RasterDim mldd::WeightMap::rasterDim() const
{
  return d_dr.rasterDim();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



