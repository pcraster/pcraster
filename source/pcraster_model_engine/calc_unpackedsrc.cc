#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_UNPACKEDSRC
#include "calc_unpackedsrc.h"
#define INCLUDED_CALC_UNPACKEDSRC
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SPATIALPACKING
#include "calc_spatialpacking.h"
#define INCLUDED_CALC_SPATIALPACKING
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif



/*!
  \file
  This file contains the implementation of the UnpackedSrc class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class UnpackedSrcPrivate
{
public:

  UnpackedSrcPrivate()
  {
  }

  ~UnpackedSrcPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC UNPACKEDSRC MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF UNPACKEDSRC MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
 * \param sp packing
 * \param packed field in possible \a sp packed form
 */
calc::UnpackedSrc::UnpackedSrc( const SpatialPacking& sp,
                                const Field* packed):
 d_sp(sp),d_packed(packed),d_unpacked(0)
{
}



calc::UnpackedSrc::~UnpackedSrc()
{
  if (d_packed != d_unpacked)
      delete d_unpacked;
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::UnpackedSrc& calc::UnpackedSrc::operator=(const UnpackedSrc& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::UnpackedSrc::UnpackedSrc(const UnpackedSrc& rhs):
  Base(rhs)
{
}
*/

//! return field as a (use-only) source
const calc::Field* calc::UnpackedSrc::src()
{
  if (!d_unpacked)
    d_unpacked= d_packed->isSpatial()  ?
                 d_sp.unpack(d_packed) :
                 d_packed;
  return d_unpacked;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



