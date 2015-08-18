#ifndef INCLUDED_DAL_MATRIXDRIVER
#define INCLUDED_DAL_MATRIXDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

#ifndef INCLUDED_DAL_MATRIX
#include "dal_Matrix.h"
#define INCLUDED_DAL_MATRIX
#endif



namespace dal {
  // MatrixDriver declarations.
}



namespace dal {



//! This class is a base class for i/o drivers for Matrix datasets.
/*!
*/
class MatrixDriver: public Driver
{

  friend class MatrixDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MatrixDriver&    operator=           (MatrixDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MatrixDriver        (MatrixDriver const& rhs);

protected:

                   MatrixDriver        (Format const& format);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~MatrixDriver       ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Matrix*          read                (std::string const& name) const;

  //! Opens the matrix with name \a name and reads the data into \a matrix.
  /*!
    \exception Exception In case the matrix cannot be opened.

    This function will fail if the values read from \a name are not in sync
    with the properties of \a matrix. For example, if the matrix is configured
    to contain UINT2 values and a negative value is encountered an exception
    will be thrown.
  */
  virtual void     read                (std::string const& name,
                                        Matrix& matrix) const=0;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

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



} // namespace dal

#endif
