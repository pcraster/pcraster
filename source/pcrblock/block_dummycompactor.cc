#include "stddefx.h"
#include "block_dummycompactor.h"
#include "dal_MathUtils.h"


/*!
  \file
  This file contains the implementation of the DummyCompactor class.
*/



namespace block {

//------------------------------------------------------------------------------

/*
class DummyCompactorPrivate
{
public:

  DummyCompactorPrivate()
  {
  }

  ~DummyCompactorPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DUMMYCOMPACTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DUMMYCOMPACTOR MEMBERS
//------------------------------------------------------------------------------

DummyCompactor::DummyCompactor()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
DummyCompactor::DummyCompactor(
         DummyCompactor const& rhs)

  : Base(rhs)

{
}
*/



DummyCompactor::~DummyCompactor()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
DummyCompactor& DummyCompactor::operator=(
         DummyCompactor const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



REAL4 DummyCompactor::operator()(
         REAL4 originalThickness,
         REAL4 /* depth */)
{
  DEVELOP_PRECOND(dal::greaterOrComparable(originalThickness, REAL4(0.0)));
  return originalThickness;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace block

