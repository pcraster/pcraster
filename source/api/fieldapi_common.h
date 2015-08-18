#ifndef INCLUDED_FIELDAPI_COMMON
#define INCLUDED_FIELDAPI_COMMON



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif

// Module headers.


namespace geo {
 class CellLoc;
}

namespace fieldapi {



//! Common features of Field Api objects
class Common : public geo::RasterDim
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Common&           operator=           (const Common&);

  //! Copy constructor. NOT IMPLEMENTED.
                   Common               (const Common&);


  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

protected:
             Common              (size_t nrRows,size_t nrCols);
public:

  virtual   ~Common              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

/*
 * //! nr of rows
 *        size_t  nrRows()     const { return d_nrRows;};
 * //! nr of rows as int value
 *        int     nrRowsInt()  const { return static_cast<int>(d_nrRows);};
 * //! nr of collumns
 *        size_t  nrCols()     const { return d_nrCols;};
 * //! nr of cols as int value
 *        int     nrColsInt()  const { return static_cast<int>(d_nrCols);};
 */
  //! global cell Length depending on --unitcell/--unittrue setting
  static double  cellLength();

  //! check if indeces are out of range
  bool outOfRange(int row, int col) const
  {
    return !intContains(row,col);
  }

  //! test if this cell Loc is a MV
  virtual bool isMV(const geo::CellLoc& l) const=0;
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
