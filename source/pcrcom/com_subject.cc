#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_SUBJECT
#include "com_subject.h"
#define INCLUDED_COM_SUBJECT
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_COM_OBSERVER
#include "com_observer.h"
#define INCLUDED_COM_OBSERVER
#endif



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

com::Subject::Subject()
{
}



com::Subject::~Subject()
{
}



/*!
  \param     o Observer to attach.
  \warning   Make sure that the observer \link detach() detached \endlink if it
             is deleted.
  \sa        detach()
*/
void com::Subject::attach(Observer *o)
{
#ifdef DEBUG_DEVELOP
  PRECOND(o);
#endif

  d_observers.push_back(o);
}



/*!
  \param     o Observer to detach.
  \warning   It is assumed that \a o points to a previously \link attach()
             attached \endlink observer.
  \sa        attach()
*/
void com::Subject::detach(Observer *o)
{
#ifdef DEBUG_DEVELOP
  PRECOND(o);
  PRECOND(std::find(d_observers.begin(), d_observers.end(), o) !=
          d_observers.end());
#endif

  d_observers.erase(std::find(d_observers.begin(), d_observers.end(), o));
}



void com::Subject::notify()
{
  std::vector<Observer *>::iterator it;

  for(it = d_observers.begin(); it != d_observers.end(); ++it)
    (*it)->update(this);
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

