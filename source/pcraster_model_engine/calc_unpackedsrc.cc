#include "stddefx.h"
#include "calc_unpackedsrc.h"
#include "calc_spatialpacking.h"
#include "calc_field.h"

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
calc::UnpackedSrc::UnpackedSrc(const SpatialPacking &sp, const Field *packed)
    : d_sp(sp), d_packed(packed)
{
}

calc::UnpackedSrc::~UnpackedSrc()
{
  if (d_packed != d_unpacked) {
    delete d_unpacked;
  }
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
const calc::Field *calc::UnpackedSrc::src()
{
  if (d_unpacked == nullptr) {
    d_unpacked = d_packed->isSpatial() ? d_sp.unpack(d_packed) : d_packed;
  }
  return d_unpacked;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
