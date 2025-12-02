#include "com_rcobject.h"
#include <iostream>

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

com::RCObject::RCObject()
{
}

com::RCObject::RCObject(const RCObject &)
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
  if (--d_refCount == 0) {
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
