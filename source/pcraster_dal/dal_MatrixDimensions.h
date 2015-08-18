#ifndef INCLUDED_DAL_MATRIXDIMENSIONS
#define INCLUDED_DAL_MATRIXDIMENSIONS



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // MatrixDimensions declarations.
}



namespace dal {

//! Class for object representing the dimensions of a matrix.
/*!
  The dimensionality of a matrix is defined by its number of rows and columns.
*/
class PCR_DAL_DECL MatrixDimensions
{

  friend class MatrixDimensionsTest;
  friend bool operator==(MatrixDimensions const&, MatrixDimensions const&);
  friend bool operator!=(MatrixDimensions const&, MatrixDimensions const&);

private:

  //! Number of rows in the matrix.
  size_t           _nrRows;

  //! Number of columns in the matrix.
  size_t           _nrCols;

  bool             equals              (MatrixDimensions const& rhs) const;

protected:

  void             setNrRows           (size_t nrRows);

  void             setNrCols           (size_t nrCols);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MatrixDimensions    (size_t nrRows=1,
                                        size_t nrCols=1);

                   MatrixDimensions    (MatrixDimensions const& rhs);

  virtual          ~MatrixDimensions   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  MatrixDimensions& operator=          (MatrixDimensions const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (MatrixDimensions const& lhs,
                                        MatrixDimensions const& rhs);

bool               operator!=          (MatrixDimensions const& lhs,
                                        MatrixDimensions const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
