#ifndef INCLUDED_CALC_UNPACKEDCREATION
#define INCLUDED_CALC_UNPACKEDCREATION

#include "stddefx.h"
#include "calc_types.h"


namespace calc {
  // UnpackedCreation declarations.
}



namespace calc {

class SpatialPacking;
class Field;


//! create an unpacked Spatial, usable as unpacked destination, and return result in current packing
/*!
 * if a temporary unpacked must be created, this class will delete it
 * in its dtor, while the result will be released.
 */
class UnpackedCreation
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  UnpackedCreation&           operator=           (const UnpackedCreation& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   UnpackedCreation               (const UnpackedCreation& rhs);

  const SpatialPacking& d_sp;
  //! unpacked, created in ctor
  Field                *d_unpacked;
  //! in current packing
  const Field          *d_packed{};


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    UnpackedCreation(const SpatialPacking& sp,VS vs);

  /* virtual */    ~UnpackedCreation              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  Field* releasePacked();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Field*       unpacked()     const;
  void*        unpackedDest() const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
