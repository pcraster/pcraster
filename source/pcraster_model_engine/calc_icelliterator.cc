#include "stddefx.h"
#include "calc_icelliterator.h"



/*!
  \file
  This file contains the implementation of the ICellIterator class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ICellIteratorPrivate
{
public:

  ICellIteratorPrivate()
  {
  }

  ~ICellIteratorPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ICELLITERATOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ICELLITERATOR MEMBERS
//------------------------------------------------------------------------------

calc::ICellIterator::ICellIterator()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::ICellIterator::ICellIterator(ICellIterator const& rhs)

  : Base(rhs)

{
}
*/



calc::ICellIterator::~ICellIterator()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::ICellIterator& calc::ICellIterator::operator=(ICellIterator const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! return downstream cell of "ldd-iterator"-current
/*!
 * can only be called if traversing an ldd.
 */
size_t calc::ICellIterator::lddDownstream() const
{
  PRECOND(false);
  return 0;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



