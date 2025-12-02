#include "stddefx.h"
#include "com_observer.h"

/*!
  \file
  brief

  more elaborated
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::Observer::Observer()
{
}

com::Observer::~Observer()
{
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

/*!
  \fn        void com::Observer::update(Subject *s)
  \brief     This function gets called by subject \a s if its state changed.
  \param     s Subject which changed state.

  Any implementation should make sure that the state of the concrete observer
  is made consistent with the state of the concrete subject. 
*/
