#ifndef INCLUDED_CLASSIFICATION
#include "Classification.h"
#define INCLUDED_CLASSIFICATION
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Classification class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASSIFICATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASSIFICATION MEMBERS
//------------------------------------------------------------------------------

template<typename T>
Classification<T>::Classification()
{
}



template<typename T>
Classification<T>::~Classification()
{
}



template<typename T>
size_t Classification<T>::nrClasses() const
{
  assert(this->empty() || this->size() > 1);

  return this->empty() ? 0 : this->size() - 1;
}



template class Classification<float>;
template class Classification<double>;

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

