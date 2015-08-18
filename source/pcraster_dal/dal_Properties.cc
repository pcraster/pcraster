#ifndef INCLUDED_DAL_PROPERTIES
#include "dal_Properties.h"
#define INCLUDED_DAL_PROPERTIES
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Properties class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROPERTIES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROPERTIES MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Creates an empty properties object.
*/
Properties::Properties()
{
}



//! Copy constructor.
Properties::Properties(
         Properties const& rhs)

  : d_values(rhs.d_values)

{
}



//! Destructor.
/*!
*/
Properties::~Properties()
{
}



//! Assignment operator.
Properties& Properties::operator=(
         Properties const& rhs)
{
  if(this != &rhs) {
    d_values = rhs.d_values;
  }

  return *this;
}



void Properties::clear()
{
  d_values.clear();
}



//! Returns the number of property values which have been set.
/*!
  \return    amount
*/
size_t Properties::size() const
{
  return d_values.size();
}



//! Returns whether not properties have been set.
/*!
  \return    true or false
*/
bool Properties::isEmpty() const
{
  return d_values.empty();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

