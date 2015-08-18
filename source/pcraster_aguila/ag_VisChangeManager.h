#ifndef INCLUDED_AG_VISCHANGEMANAGER
#define INCLUDED_AG_VISCHANGEMANAGER



#include <vector>



namespace ag {
  class VisSubject;
  class VisObserver;
}



namespace ag {



//! The VisChangeManager manages VisSubject and VisObserver objects.
/*!
  This class encapsulates the update semantics of VisSubject and the
  VisObserver objects. If a VisSubject signals its change manager that its
  state has changed and that the VisObservers should be notified it calls
  notify() and the manager tells the VisObservers to rescan the subject.
*/
class VisChangeManager
{

private:

  //! The subject.
  VisSubject*      d_subject;

  //! The observers.
  std::vector<VisObserver*> d_observers; 

  //! Assignment operator. NOT IMPLEMENTED.
  VisChangeManager &operator=          (const VisChangeManager&);

  //! Copy constructor. NOT IMPLEMENTED.
                   VisChangeManager    (const VisChangeManager&);

public:

  typedef std::vector<VisObserver*>::const_iterator const_iterator;

  typedef std::vector<VisObserver*>::iterator iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   VisChangeManager    (VisSubject* s);

  /* virtual */    ~VisChangeManager   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             attach              (VisObserver* o);

  void             detach              (VisObserver* o);

  void             notify              ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             observedBy          (VisObserver* o) const;

  size_t           nrObservers         () const;

  const_iterator   begin               () const;

  const_iterator   end                 () const;

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
