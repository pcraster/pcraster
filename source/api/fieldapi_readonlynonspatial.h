#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIAL
#define INCLUDED_FIELDAPI_READONLYNONSPATIAL

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_FIELDAPI_READONLY
#include "fieldapi_readonly.h"
#define INCLUDED_FIELDAPI_READONLY
#endif

namespace fieldapi {


//! template for a non spatial field, a constant number
template<class UseAsT> class ReadOnlyNonSpatial:
  public ReadOnly<UseAsT>
{

private:

  //! value
  UseAsT d_value;

  //! Assignment operator. NOT IMPLEMENTED.
  ReadOnlyNonSpatial&           operator=           (const ReadOnlyNonSpatial&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReadOnlyNonSpatial               (const ReadOnlyNonSpatial&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                ReadOnlyNonSpatial               (UseAsT value,
                                                  size_t nrRows,size_t nrCols);

       ~ReadOnlyNonSpatial               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool     get(UseAsT& value,    int rowIndex,    int colIndex) const override;
  bool     get(UseAsT& value, size_t rowIndex, size_t colIndex) const override;
  UseAsT value(               size_t rowIndex, size_t colIndex) const override;

  bool     spatial() const override;
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace fieldapi

#endif
