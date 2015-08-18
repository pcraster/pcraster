#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#define INCLUDED_FIELDAPI_READONLYSPATIAL

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

  virtual      ~ReadOnlySpatial               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool     get(UseAsT& value,    int rowIndex,    int colIndex) const;
  bool     get(UseAsT& value, size_t rowIndex, size_t colIndex) const;
  UseAsT value(               size_t rowIndex, size_t colIndex) const;
  bool     spatial() const;
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
