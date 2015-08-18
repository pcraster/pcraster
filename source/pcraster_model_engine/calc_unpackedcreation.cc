#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_UNPACKEDCREATION
#include "calc_unpackedcreation.h"
#define INCLUDED_CALC_UNPACKEDCREATION
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h" //nrValues
#define INCLUDED_GEO_RASTERDIM
#endif
// Module headers.
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif



/*!
  \file
  This file contains the implementation of the UnpackedCreation class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class UnpackedCreationPrivate
{
public:

  UnpackedCreationPrivate()
  {
  }

  ~UnpackedCreationPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC UNPACKEDCREATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF UNPACKEDCREATION MEMBERS
//------------------------------------------------------------------------------

calc::UnpackedCreation::UnpackedCreation( const SpatialPacking& sp,
                                          VS vs):
 d_sp(sp)
{
 d_unpacked=new Spatial(vs,CRI_X, d_sp.rasterDim().nrCells());
}

calc::UnpackedCreation::~UnpackedCreation()
{
  delete d_unpacked;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::UnpackedCreation& calc::UnpackedCreation::operator=(const UnpackedCreation& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::UnpackedCreation::UnpackedCreation(const UnpackedCreation& rhs):
  Base(rhs)
{
}
*/

calc::Field* calc::UnpackedCreation::releasePacked()
{
  Field *packed=d_sp.pack(d_unpacked);
  if (d_unpacked != packed)
      delete d_unpacked; // delete temporary
  d_unpacked=0;
  return packed;
}

calc::Field* calc::UnpackedCreation::unpacked() const
{
  PRECOND(d_unpacked);
  return d_unpacked;
}

//! Field::dest()
void* calc::UnpackedCreation::unpackedDest() const
{
  return d_unpacked->dest();
}
