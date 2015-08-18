#include "ag_VisSubject.h"
#include "ag_VisChangeManager.h"



/*!
  \file
   This file contains the implementation of the VisSubject class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructs a VisSubject object.
/*!
*/
ag::VisSubject::VisSubject()

  : d_cm(0)

{
  d_cm = new VisChangeManager(this);
}



//! Copy constructor.
/*!
  \param     rhs Subject to copy.
  \warning   The VisChangeManager is *not* copied. This new object gets a
             fresh one. The change manager is not considered part of the data
             of the subject.
*/
ag::VisSubject::VisSubject(const VisSubject& /* rhs */)

  : d_cm(0)

{
  d_cm = new VisChangeManager(this);
}



//! Destructor.
/*!
*/
ag::VisSubject::~VisSubject()
{
  delete d_cm;
}



/*!
  \sa        ag::VisChangeManager::attach(VisObserver*),
             ag::VisChangeManager::detach(VisObserver*)
*/
void ag::VisSubject::attach(VisObserver* o)
{
  d_cm->attach(o);
}



/*!
  \sa        ag::VisChangeManager::detach(VisObserver*),
             ag::VisChangeManager::attach(VisObserver*)
*/
void ag::VisSubject::detach(VisObserver* o)
{
  d_cm->detach(o);
}



/*!
  \sa        ag::VisChangeManager::observedBy(VisObserver*)
*/
bool ag::VisSubject::observedBy(VisObserver* o) const
{
  return d_cm->observedBy(o);
}



/*!
  \sa        ag::VisChangeManager::nrObservers()
*/
size_t ag::VisSubject::nrObservers() const
{
  return d_cm->nrObservers();
}



bool ag::VisSubject::isObserved() const
{
  return nrObservers() > 0;
}



/*!
  \sa        ag::VisChangeManager::notify()

  When you override this function, don't forget to call it. Otherwise nothing
  will happen.
*/
void ag::VisSubject::notify()
{
  d_cm->notify();
}



ag::VisSubject::iterator ag::VisSubject::begin()
{
  return d_cm->begin();
}



ag::VisSubject::iterator ag::VisSubject::end()
{
  return d_cm->end();
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


