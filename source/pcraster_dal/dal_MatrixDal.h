#ifndef INCLUDED_DAL_MATRIXDAL
#define INCLUDED_DAL_MATRIXDAL



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif



namespace dal {
  // MatrixDal declarations.
  class Matrix;
  class MatrixDriver;
}



namespace dal {



//! This class represents the Data Abstraction Layer for Matrix datasets.
/*!
  Use an object of this class to read and write Matrix datasets.
*/
class MatrixDal: public Dal
{

  friend class MatrixDalTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  MatrixDal&       operator=           (MatrixDal const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   MatrixDal           (MatrixDal const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MatrixDal           (bool addAllDrivers=false);

  /* virtual */    ~MatrixDal          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  boost::tuple<boost::shared_ptr<dal::Matrix>, dal::MatrixDriver*> open(
                                        std::string const& name);

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
