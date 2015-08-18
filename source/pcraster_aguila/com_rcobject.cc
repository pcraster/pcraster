#include "com_rcobject.h"
#include <iostream>



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com::RCObject::RCObject()

  : d_refCount(0), d_shareable(true)

{
}



com::RCObject::RCObject(const RCObject &)

  : d_refCount(0), d_shareable(true)

{
}



com::RCObject &com::RCObject::operator=(const RCObject &)
{
  return *this;
}



com::RCObject::~RCObject()
{
}



void com::RCObject::addReference()
{
  ++d_refCount;
}



void com::RCObject::removeReference()
{
  if(--d_refCount == 0) {
    delete this;
  }
}



/*
void RCObject::markUnshareable()
{
  d_shareable = false;
}
*/



/*
bool RCObject::isShareable() const
{
  return d_shareable;
}
*/



bool com::RCObject::isShared() const
{
  return d_refCount > 1;
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


