#ifndef INCLUDED_CALC_UNPACKEDSRC
#define INCLUDED_CALC_UNPACKEDSRC



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // UnpackedSrc declarations.
}



namespace calc {

class SpatialPacking;
class Field;


//! enable to use as an unpacked src
/*!
 * if a temporary unpacked must be created, this class will delete it
 * in its dtor.
 */
class UnpackedSrc
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  UnpackedSrc&           operator=           (const UnpackedSrc& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   UnpackedSrc               (const UnpackedSrc& rhs);

  const SpatialPacking& d_sp;
  //! in current packing
  const Field          *d_packed;
  //! unpacked, may be identical to d_packed in case of \a d_sp == AsIsPacking
  const Field          *d_unpacked;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    UnpackedSrc( const SpatialPacking& sp,
                                 const Field* packed);

  /* virtual */    ~UnpackedSrc              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  const Field* src();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
