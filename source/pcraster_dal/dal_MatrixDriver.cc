#ifndef INCLUDED_DAL_MATRIXDRIVER
#include "dal_MatrixDriver.h"
#define INCLUDED_DAL_MATRIXDRIVER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the MatrixDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MATRIXDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     name Name of the driver.
  \param     description Description of the driver.
*/
MatrixDriver::MatrixDriver(
         Format const& format)

  : Driver(format)

{
}



//! Destructor.
/*!
*/
MatrixDriver::~MatrixDriver()
{
}



//! Opens the matrix with name \a name and reads the data.
/*!
  \return    Pointer to Matrix object.
  \exception Exception In case the matrix cannot be opened.
  \sa        read(std::string&, Matrix&)

  This function will return a Matrix object or throw an exception.

  The matrix is read using the matrix properties deduced by
  open(std::string const&). If the matrix properties are known by the client
  than it should call open(std::string const& name), configure the matrix
  and call read(std::string const&, Matrix*).
*/
Matrix* MatrixDriver::read(std::string const& name) const
{
  Matrix* matrix = dynamic_cast<Matrix*>(open(name));

  if(!matrix) {
    throwCannotBeOpened(name, MATRIX);
  }

  read(name, *matrix);

  return matrix;
}



void MatrixDriver::read(
         void* cell,
         TypeId
#ifdef DEBUG_BUILD
           typeId
#endif
         ,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(typeId == TI_REAL4);
  pcr::setMV(*static_cast<REAL4*>(cell));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

