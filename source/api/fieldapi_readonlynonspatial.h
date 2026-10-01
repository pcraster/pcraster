#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIAL
#define INCLUDED_FIELDAPI_READONLYNONSPATIAL

#include "stddefx.h"
#include "fieldapi_readonly.h"


namespace fieldapi {


//! template for a non spatial field, a constant number
template<class UseAsT> class ReadOnlyNonSpatial:
  public ReadOnly<UseAsT>
{

private:

  //! value
  UseAsT d_value;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                ReadOnlyNonSpatial               (UseAsT value,
                                                  size_t nrRows,size_t nrCols);

                   ReadOnlyNonSpatial               (const ReadOnlyNonSpatial&) = delete;

       ~ReadOnlyNonSpatial               () override = default;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  ReadOnlyNonSpatial&           operator=           (const ReadOnlyNonSpatial&) = delete;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool     get(UseAsT& value,    int rowIndex,    int colIndex) const override;
  bool     get(UseAsT& value, size_t rowIndex, size_t colIndex) const override;
  UseAsT value(               size_t rowIndex, size_t colIndex) const override;

  bool     spatial() const override;
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



} // namespace fieldapi

#endif
