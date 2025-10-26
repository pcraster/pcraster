#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#define INCLUDED_FIELDAPI_READONLYSPATIAL

#include "stddefx.h"
#include "fieldapi_readonly.h"


namespace fieldapi {

//! template for a spatial field
template<class UseAsT, class StoredAsT> class ReadOnlySpatial:
  public ReadOnly<UseAsT>
{

public:

  //! 2d array of data (actually protected!)
  /*!
     \todo
       making d_data is a hack to implement ReadWriteData
   */
  StoredAsT **d_data;


private:
  //! Assignment operator. NOT IMPLEMENTED.
  ReadOnlySpatial&           operator=           (const ReadOnlySpatial&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReadOnlySpatial               (const ReadOnlySpatial&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                ReadOnlySpatial               (StoredAsT **data,
                                               size_t nrRows,size_t nrCols);

       ~ReadOnlySpatial               () override;

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
