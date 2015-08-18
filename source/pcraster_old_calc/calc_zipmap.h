#ifndef INCLUDED_CALC_ZIPMAP
#define INCLUDED_CALC_ZIPMAP


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

namespace calc {
  // ZipMap declarations.
}



namespace calc {



//! a Spatial that is compressed (zipped)
class ZipMap : public Spatial
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ZipMap&           operator=           (const ZipMap&);
protected:
  //! Copy constructor.
                   ZipMap               (const ZipMap&);

  mutable char            *d_zlibCompressedData;
  mutable size_t           d_nrZlibCompressedData;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ZipMap               (const Spatial *f);

  /* virtual */    ~ZipMap              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void loadExternal()     const;
  ZipMap *copy() const;

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
