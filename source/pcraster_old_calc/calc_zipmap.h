#ifndef INCLUDED_OLDCALC_ZIPMAP
#define INCLUDED_OLDCALC_ZIPMAP

#include "stddefx.h"
#include "calc_spatial.h"



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

  /* virtual */    ~ZipMap              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void loadExternal()     const override;
  ZipMap *copy() const override;

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
