#include "ag_VisObserver.h"



/*!
  \file
   This file contains the implementation of the VisObserver class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a VisObserver object.
/*!
*/
ag::VisObserver::VisObserver()
{
}



//! Destructs a VisObserver object.
/*!
*/
ag::VisObserver::~VisObserver()
{
}



//! Rescans the subject and determines whether changes relevant to this observer where made.
/*!
  \sa        process(), visualise()

  No need to visualise or process anything yet, just determine whether
  something relevant changed.

  The default does nothing.
*/
void ag::VisObserver::rescan()
{
}



//! Processes the changes detected in rescan().
/*!
  \sa        rescan(), visualise()

  No need to visualise anything yet. Based on the changes detected in rescan(),
  perform all actions needed to be able to visualise the current state of the
  observer as fast as possible. This might be drawing in the buffer of a
  BufferedWidget, for example, without bitblitting it to the widget itself.

  The default does nothing.
*/
void ag::VisObserver::process()
{
}



//! Visualises the current state of the subject.
/*!
  \sa        rescan(), process()

  The default does nothing.
*/
void ag::VisObserver::visualise()
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

