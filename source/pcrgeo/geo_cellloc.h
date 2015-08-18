#ifndef INCLUDED_GEO_CELLLOC
#define INCLUDED_GEO_CELLLOC



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.



namespace geo {
  // CellLoc declarations.
}



namespace geo {

//! linear index into raster
typedef size_t LinearLoc;

//! identifies a cell location by a row and column number
/*! Typically used in a raster context.
 */
class CellLoc
{

private:

  // Assignment operator. DEFAULT
  // CellLoc&           operator=           (const CellLoc&);

  // Copy constructor. DEFAULT
  //                 CellLoc               (const CellLoc&);

  //! the y/row value
  size_t d_row;
  //! the x/column value
  size_t d_col;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! default ctor, not initialized
  CellLoc(){};

  //! ctor
  CellLoc(size_t row, size_t col) :
    d_row(row), d_col(col) {
  };


  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
   //! increment row and return new row id
   size_t nextRow() { return ++d_row; };
   //! increment col and return new col id
   size_t nextCol() { return ++d_col; };

   //! set row id and return new row id
   size_t setRow(size_t v) { return d_row=v; };
   //! set col id and return new col id
   size_t setCol(size_t v) { return d_col=v; };

   //! Set location to \a row, \a col.
   void  setIndices(size_t row, size_t col) { d_row = row; d_col = col; };

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------


   bool operator==(const CellLoc& c) const {
    return c.d_row == d_row && c.d_col == d_col;
   }
   bool operator!=(const CellLoc& c) const {
    return c.d_row != d_row || c.d_col != d_col;
   }


   size_t          x                   () const
   { return d_col; }

   size_t          y                   () const
   { return d_row; }

   //! return row id
   size_t row() const { return d_row; };
   //! return col id
   size_t col() const { return d_col; };

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//! Output operator for writing to an output stream.
extern std::ostream& operator<< (std::ostream &s,
                          const CellLoc &c);




//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo


#endif
