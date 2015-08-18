#include "com_rcptr.h"



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

template<class T>
com::RCPtr<T>::RCPtr(T *realPtr)

  : d_pointee(realPtr)

{
  init();
}



template<class T>
com::RCPtr<T> &com::RCPtr<T>::operator=(const RCPtr &rhs)
{
  if(d_pointee != rhs.d_pointee)
  {
    if(d_pointee) d_pointee->removeReference();
    d_pointee = rhs.d_pointee;
    init();
  }

  return *this;
}



template<class T>
com::RCPtr<T>::RCPtr(const RCPtr &rhs)

  : d_pointee(rhs.d_pointee)

{
  init();
}



template<class T>
com::RCPtr<T>::~RCPtr()
{
  if(d_pointee) {
    d_pointee->removeReference();
  }
}



template<class T>
void com::RCPtr<T>::init()
{
  if(d_pointee == 0) return;

/*
  if(!d_pointee->isShareable())
    d_pointee = new T(*d_pointee);
*/

  d_pointee->addReference();
}



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


