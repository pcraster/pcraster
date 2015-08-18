#ifndef INCLUDED_FIELDAPI_READWRITEDATA
#define INCLUDED_FIELDAPI_READWRITEDATA



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_CELLLOC
#include "geo_cellloc.h"
#define INCLUDED_GEO_CELLLOC
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_READWRITE
#include "fieldapi_readwrite.h"
#define INCLUDED_FIELDAPI_READWRITE
#endif
#ifndef INCLUDED_FIELDAPI_READONLYSPATIAL
#include "fieldapi_readonlyspatial.h"
#define INCLUDED_FIELDAPI_READONLYSPATIAL
#endif

namespace fieldapi {



//! The Field api for results, read (input) and write (output) only
/*!
    FTTB results are always spatial
    \todo
       this is not the best solution to combine put and get,
       As the publicity of ReadOnlySpatial::d_data shows.
       Maybe multiple inheritance.
 */
template<class UseAsT, class StoredAsT> class ReadWriteData :
  public ReadWrite<UseAsT>
{

private:

  //! implements the get part
  ReadOnlySpatial<UseAsT,StoredAsT> d_ro;

  //! Assignment operator. NOT IMPLEMENTED.
  ReadWriteData&           operator=           (const ReadWriteData&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReadWriteData               (const ReadWriteData&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ReadWriteData               (
                        StoredAsT **data, size_t nrRows, size_t nrCols);

   virtual         ~ReadWriteData              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! put a value
  void put(UseAsT value, size_t rowIndex, size_t colIndex);
  //! see put(UseAsT value, size_t rowIndex, size_t colIndex);
  void put(UseAsT value, const geo::CellLoc& l) {
        put(value,l.row(),l.col());
      }
  //! put a value
  void put(UseAsT value, int rowIndex, int colIndex)
   { put(value,static_cast<size_t>(rowIndex),static_cast<size_t>(colIndex)); };
  //! put a MV value
  void putMV(size_t rowIndex, size_t colIndex);
  void putMV(const geo::CellLoc& l) {
        putMV(l.row(),l.col());
  }
  //! put a MV value
  void putMV(int rowIndex, int colIndex)
   { putMV(static_cast<size_t>(rowIndex),static_cast<size_t>(colIndex)); };

  void putAllMV();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool     get(UseAsT& value,    int rowIndex,    int colIndex) const
      { return d_ro.get(value,rowIndex,colIndex); }
  bool     get(UseAsT& value, size_t rowIndex, size_t colIndex) const
      { return d_ro.get(value,rowIndex,colIndex); }
  //! see get(UseAsT& value, size_t rowIndex, size_t colIndex);
          bool get(UseAsT& value, const geo::CellLoc& l) const {
            return get(value,l.row(),l.col());
          }
  UseAsT value( size_t rowIndex, size_t colIndex) const
      { return d_ro.value(rowIndex,colIndex); }
  UseAsT value( const geo::CellLoc& l) const
      { return d_ro.value(l.row(),l.col()); }

  bool isMV(const geo::CellLoc& l) const;

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
