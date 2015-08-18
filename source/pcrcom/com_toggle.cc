#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TOGGLE
#include "com_toggle.h"
#define INCLUDED_COM_TOGGLE
#endif



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

com_Toggle::com_Toggle(bool enabled)

  : d_enabled(enabled)

{
}



com_Toggle::~com_Toggle()
{
}



void com_Toggle::setEnabled(bool s)
{
  d_enabled = s;
}



bool com_Toggle::enabled() const
{
  return d_enabled;
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


