#ifndef INCLUDED_DAL_MATRIXDAL
#include "dal_MatrixDal.h"
#define INCLUDED_DAL_MATRIXDAL
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_TEXTMATRIXDRIVER
#include "dal_TextMatrixDriver.h"
#define INCLUDED_DAL_TEXTMATRIXDRIVER
#endif



/*!
  \file
  This file contains the implementation of the MatrixDal class.
*/



//------------------------------------------------------------------------------

/*
namespace dal {

class MatrixDalPrivate
{
public:

  MatrixDalPrivate()
  {
  }

  ~MatrixDalPrivate()
  {
  }

};

} // namespace dal
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIXDAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MATRIXDAL MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     addAllDrivers Whether or not to add all available drivers automatically.
*/
dal::MatrixDal::MatrixDal(bool addAllDrivers)

  : Dal(false)

{
  if(addAllDrivers) {
    #include "autoAddMatrixDrivers.cc"
  }
}



//! Destructor.
/*!
*/
dal::MatrixDal::~MatrixDal()
{
}



//! Opens the Matrix dataset pointed to by \a name.
/*!
  \param     name Name of Matrix dataset.
  \return    Pointer to a newly created Matrix object or 0 if no driver could open \a name.

  The caller is responsible of deleting the Matrix object again.
*/
boost::tuple<boost::shared_ptr<dal::Matrix>, dal::MatrixDriver*>
dal::MatrixDal::open(
    std::string const& name)
{
  assert(nrDrivers() > 0);
  boost::shared_ptr<Dataset> dataset;
  dal::Driver* driver;
  boost::tie(dataset, driver) = Dal::open(name, MATRIX);
  return boost::make_tuple(boost::dynamic_pointer_cast<Matrix>(dataset),
      dynamic_cast<MatrixDriver*>(driver));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


