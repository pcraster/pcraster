#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_COUNTEDOBJECT
#include "com_countedobject.h"
#define INCLUDED_COM_COUNTEDOBJECT
#endif



/*!
  \file
  This file contains the implementation of the CountedObject class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

template<class T>
size_t com::CountedObject<T>::d_nrCreated = 0;



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

//! Constructs a CountedObject object for type T.
/*!
*/
template<class T>
com::CountedObject<T>::CountedObject()
{
  ++d_nrCreated;
}



//! Copy constructor.
/*!
  \param     co Object to copy values from.

  Since we have no object specific data \a co is not used here.
*/
template<class T>
com::CountedObject<T>::CountedObject(const CountedObject& /* co */)
{
  // A new object is created so we increment here too.
  ++d_nrCreated;
}



//! Destructs the CountedObject object for type T.
/*!
*/
template<class T>
com::CountedObject<T>::~CountedObject()
{
#ifdef DEBUG_DEVELOP
  PRECOND(d_nrCreated != 0);
#endif

  --d_nrCreated;
}



//! Returns true if this is the first object created.
/*!
  \return    true or false.
  \sa        nrCreated()
*/
template<class T>
bool com::CountedObject<T>::firstObjectConstructed() const
{
  return d_nrCreated == 1;
}



//! Returns the number of objects created.
/*!
  \return    Number of objects created.
  \sa        firstObjectConstructed()
*/
template<class T>
size_t com::CountedObject<T>::nrObjectsCreated() const
{
  return d_nrCreated;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


