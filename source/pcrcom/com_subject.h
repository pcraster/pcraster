#ifndef INCLUDED_COM_SUBJECT
#define INCLUDED_COM_SUBJECT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif



namespace com {
  class Observer;
}



namespace com {



/*!
  \class Subject
  \brief The Subject class is one part of an implementation of the observer
         pattern.

  A subject knows its observers. Any number of observer objects may observe a
  subject. It provides an interface for attaching and detaching observer
  objects.

  This is an abstract base class for concrete subjects. A concrete subject
  stores the state of interest to concrete observer objects. It sends a
  notification to its observers when its state changes.
*/
class Subject
{

private:

  //! Collection of observers.
  std::vector<Observer *> d_observers;

  //! Assignment operator. NOT IMPLEMENTED.
  Subject &        operator=           (const Subject &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Subject             (const Subject &);

protected:

  //! Constructor.
                   Subject             ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Destructor.
  virtual          ~Subject            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Attaches observer \a o to the subject.
  virtual void     attach              (Observer *o);

  //! Detaches observer \a o from the subject.
  virtual void     detach              (Observer *o);

  //! Notifies all attached observers that the subject's state has changed.
  virtual void     notify              ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace com

#endif
