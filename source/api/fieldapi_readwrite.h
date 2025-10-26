#ifndef INCLUDED_FIELDAPI_READWRITE
#define INCLUDED_FIELDAPI_READWRITE

#include "stddefx.h"
#include "geo_cellloc.h"
#include "fieldapi_common.h"
#include "fieldapi_readonly.h" // needed for copy()


namespace fieldapi {


//! The abstract Field api for results, read (input) and write (output) only
/*!
    FTTB results are always spatial
 */
template<class UseAsT> class ReadWrite :
  public Common
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ReadWrite&           operator=           (const ReadWrite&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReadWrite               (const ReadWrite&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ReadWrite               (size_t nrRows, size_t nrCols);

          ~ReadWrite              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! put a value
  virtual void put(UseAsT value, size_t rowIndex, size_t colIndex)=0;
  //! put a value
          void put(UseAsT value, const geo::CellLoc& l) {
            return put(value,l.row(),l.col());
          }
  //! put a value
  virtual void put(UseAsT value, int rowIndex, int colIndex)=0;
  //! put a MV value
  virtual void putMV(size_t rowIndex, size_t colIndex)=0;
  //! put a value
          void putMV(const geo::CellLoc& l) {
            return putMV(l.row(),l.col());
          }
  //! put a MV value
  virtual void putMV(int rowIndex, int colIndex)=0;

  //! copy value of a cell location
          void copy(const ReadOnly<UseAsT>& src, const geo::CellLoc& l);

  //! set all cells to MV
  virtual void putAllMV()=0;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! see ReadOnly::get()
  virtual bool     get(UseAsT& value,    int rowIndex,    int colIndex) const=0;
  //! see ReadOnly::get()
  virtual bool     get(UseAsT& value, size_t rowIndex, size_t colIndex) const=0;
  //! see ReadOnly::get()
          bool get(UseAsT& value, const geo::CellLoc& l) const {
            return get(value,l.row(),l.col());
          }
  //! see ReadOnly::value()
  virtual UseAsT value(               size_t rowIndex, size_t colIndex) const=0;

  //! see ReadOnly::value()
          UseAsT operator[](const geo::CellLoc& l) const {
            return value(l.row(),l.col());
          }
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
