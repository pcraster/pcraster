#ifndef INCLUDED_FIELDAPI_READONLY
#define INCLUDED_FIELDAPI_READONLY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_FIELDAPI_COMMON
#include "fieldapi_common.h"
#define INCLUDED_FIELDAPI_COMMON
#endif

namespace geo {
  class CellLoc;
}


namespace fieldapi {



//! The Field api for read (input) only
template<class UseAsT> class ReadOnly : public Common
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ReadOnly&           operator=           (const ReadOnly&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReadOnly               (const ReadOnly&);

protected:
                   ReadOnly              (size_t nrRows,size_t nrCols);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------


  virtual         ~ReadOnly              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! checked access on MV and indices
  /*!
      \returns
        false if Indeces out of range or value is MV
        true  otherwise
   */
  virtual bool get(UseAsT& value, int rowIndex, int colIndex) const=0;
  //! checked access on MV only
  /*!
      \returns
        false if value is MV
        true  otherwise
   */
  virtual bool get(UseAsT& value, size_t rowIndex, size_t colIndex) const=0;

  //! see get(UseAsT& value, size_t rowIndex, size_t colIndex);
          bool get(UseAsT& value, const geo::CellLoc& l) const {
            return get(value,l.row(),l.col());
          }

  //! unchecked access
  /*!
     used in the case where one knows it should have a non-MV value
      \returns
        the value
   */
  virtual UseAsT value(size_t rowIndex, size_t colIndex) const=0;

  //! see UseAsT value(size_t rowIndex, size_t colIndex)
          UseAsT operator[](const geo::CellLoc& l) const {
            return value(l.row(),l.col());
          };

  //! is it a spatial varying field
  virtual bool spatial() const=0;

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
