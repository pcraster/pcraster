#ifndef INCLUDED_COM_STACK
#define INCLUDED_COM_STACK

#include "stddefx.h"

#include <stack>



namespace com {
  // Stack declarations.
}


namespace com {

//! std::stack but with a value returning pop
/*!
 * as suggested in Josuttis
 */
template<typename T>
  class Stack: public std::stack<T>
{

  friend class StackTest;

private:

  //  Assignment operator. DEFAULT
  // Stack&           operator=           (Stack const& rhs);

  //  Copy constructor.    DEFAULT
  //               Stack               (Stack const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //               Stack               ();

  //               ~Stack              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  T                 pop                ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<typename T>
inline T Stack<T>::pop() {
  PRECOND(!this->empty());
  T v= this->top();
  std::stack<T>::pop();
  return v;
}


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
