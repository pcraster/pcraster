#ifndef INCLUDED_COM_OBSERVER
#define INCLUDED_COM_OBSERVER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace com {
  class Subject;
}



namespace com {



/*!
  \class Observer
  \brief The Observer class is one part of an implementation of the observer
         pattern.

  An observer defines an updating interface for objects that should be notified
  of changes in a subject.

  This is a base class for concrete observers. A concrete observer maintains
  a reference to a concrete subject object. It stores state which should be
  consistent with the subject's. And it implements the Observer updating
  interface to keep its state consistent with the subject's.
*/
//       1         2         3         4         5         6         7         8
class Observer
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Observer &       operator=           (const Observer &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Observer            (const Observer &);

protected:

  //! Constructor.
                   Observer            ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Destructor.
  virtual          ~Observer           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     update              (Subject *s) = 0;

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
