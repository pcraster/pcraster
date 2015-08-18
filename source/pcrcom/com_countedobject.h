#ifndef INCLUDED_COM_COUNTEDOBJECT
#define INCLUDED_COM_COUNTEDOBJECT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif



namespace com {



//! The CountedObject class is a base for objects which need to be counted.
/*!
  This class is meant as a base class for objects who, for example, have to
  know if:

  <ul>
    <li>
      <b>they are the first object created</b><br>
      This might be important for the initialisation of complex static
      variables.
    </li>
    <li>
      <b>they are the last object created</b><br>
      This might be important for the destruction of complex static variables.
    </li>
  </ul>

  You have to instantiate the template for each class you want to use this
  class as a base for:

  \code
  // myclass.h

  #include "countedobject.h"

  class MyClass: public CountedObject<MyClass>
  {
    // ...
  };

  // myclass.cc

  #include "countedobject.cc"

  template class CountedObject<MyClass>;
  \endcode

  Remember: Class objects are constructed from the bottom up: first the base,
  then the members, and then the derived class itself. They are destroyed in
  the opposite order: first the derived class itself, then the members, and then
  the base. Members and bases are constructed in order of declaration in the
  class and destroyed in the reverse order.

  Here is a sample:

  \code
  MyClass::~MyClass()
  {
    // CountedObject 's destructor has not been called yet, so
    // nrObjectsCreated() will return 1. In this case this means that this is
    // the last object of its type.
    if(nrObjectsCreated == 1) {
      // ...
    }
  }
  \endcode
*/
template<class T>
class CountedObject
{

private:

  //! Number of objects created.
  static size_t    d_nrCreated;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CountedObject       ();

                   CountedObject       (const CountedObject& co);

  virtual          ~CountedObject      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             firstObjectConstructed() const;

  size_t           nrObjectsCreated    () const;

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
