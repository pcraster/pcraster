#include "stddefx.h"
#include "calc_pointimpl.h"

/*!
  \file
  This file contains the implementation of the PointImpl class.
*/


//------------------------------------------------------------------------------

/*
namespace calc {

class PointImplPrivate
{
public:

  PointImplPrivate()
  {
  }

  ~PointImplPrivate()
  {
  }

};

} // namespace calc
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC POINTIMPL MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF POINTIMPL MEMBERS
//------------------------------------------------------------------------------

calc::PointImpl::PointImpl()

{
}

/* NOT IMPLEMENTED
//! Copy constructor.
calc::PointImpl::PointImpl(PointImpl const& rhs)

  : Base(rhs)

{
}
*/


calc::PointImpl::~PointImpl()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::PointImpl& calc::PointImpl::operator=(PointImpl const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_pointFunction
void calc::PointImpl::setPointFunction(const char *pointFunction)
{
  PRECOND(!d_pointFunction);  // set once
  d_pointFunction = pointFunction;
}

//! get value of d_pointFunction
const char *calc::PointImpl::pointFunction() const
{
  return d_pointFunction;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
