#include <functional>
#include <cassert>
#include "dev_Algorithm.h"
#include "ag_VisChangeManager.h"
#include "ag_VisObserver.h"
#include "ag_VisSubject.h"



/*!
  \file
   This file contains the implementation of the VisChangeManager class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     s Subject.
*/
ag::VisChangeManager::VisChangeManager(VisSubject* s)

  : d_subject(s)

{
  // Maybe the subject isn't needed at all and can be deleted.
  assert(d_subject);
}



//! Destructor.
/*!
*/
ag::VisChangeManager::~VisChangeManager()
{
}



//! Adds \a o to the list of observers for this.
/*!
  \param     o Observer.
  \sa        detach(VisObserver*)
*/
void ag::VisChangeManager::attach(VisObserver *o)
{
  d_observers.push_back(o);
}



//! Removes \a o from the list of observers for this.
/*!
  \param     o Observer.
  \sa        attach(VisObserver*)
*/
void ag::VisChangeManager::detach(VisObserver *o)
{
  assert(o);

  iterator it = std::find(begin(), end(), o);

#ifdef DEBUG_DEVELOP
  assert(it != end());
#endif

  d_observers.erase(it);
}



//! Returns true if \a o observes the subject.
/*!
  \param     o The observer to search.
  \return    True if \a o observes the subject.
*/
bool ag::VisChangeManager::observedBy(VisObserver *o) const
{
  const_iterator it = std::find(begin(), end(), o);
  return it != end() ? true : false;
}



//! Returns the number of observers.
/*!
  \return    Number of observers.
*/
size_t ag::VisChangeManager::nrObservers() const
{
  return d_observers.size();
}



//! Notifies all observers that the subject has changed its state.
/*!
  \warning   Be careful when you call this function. It's possible that the
             observers need a lot of time to update their state according to
             the changed state of the subject. Call this function only if the
             subject really changed its state.
*/
void ag::VisChangeManager::notify()
{
  dev::forWhole(d_observers, std::mem_fun(&VisObserver::rescan));
  dev::forWhole(d_observers, std::mem_fun(&VisObserver::process));
  dev::forWhole(d_observers, std::mem_fun(&VisObserver::visualise));
}



ag::VisChangeManager::iterator ag::VisChangeManager::begin()
{
  return d_observers.begin();
}



ag::VisChangeManager::iterator ag::VisChangeManager::end()
{
  return d_observers.end();
}



ag::VisChangeManager::const_iterator ag::VisChangeManager::begin() const
{
  return d_observers.begin();
}



ag::VisChangeManager::const_iterator ag::VisChangeManager::end() const
{
  return d_observers.end();
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


