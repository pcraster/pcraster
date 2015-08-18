#ifndef INCLUDED_AG_VISSUBJECT
#define INCLUDED_AG_VISSUBJECT



#include "ag_Configure.h"

#ifndef INCLUDED_AG_VISCHANGEMANAGER
#include "ag_VisChangeManager.h"
#endif



namespace ag {
  class VisObserver;
}



namespace ag {



//! The VisSubject class is for objects who are observed by VisObserver 's.
/*!
  The VisSubject knows which VisObserver objects are observing it. It the
  VisSubject 's state changes, than all it's VisObservers are notify() ed.
*/
class PCR_AG_DECL VisSubject
{

private:

  //! Change manager for managing the subject - object relationships.
  VisChangeManager *d_cm;

  //! Assignment operator. NOT IMPLEMENTED.
  VisSubject &     operator=           (const VisSubject &);

protected:

public:

  typedef VisChangeManager::iterator iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisSubject          ();

                   VisSubject          (const VisSubject & /* rhs */);

  virtual          ~VisSubject         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             attach              (VisObserver *o);

  void             detach              (VisObserver *o);

  virtual void     notify              ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             observedBy          (VisObserver *o) const;

  size_t           nrObservers         () const;

  bool             isObserved          () const;

  iterator         begin               ();

  iterator         end                 ();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
