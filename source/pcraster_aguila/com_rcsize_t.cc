#include "com_rcsize_t.h"



/*!
  \file
  This file contains the implementation of the RCSize_t class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RCSIZE_T MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RCSIZE_T MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     value Value of layered object.
*/
com::RCSize_t::RCSize_t(size_t value)

  : RCObject(),
    d_value(value)

{
}



//! Destructor.
/*!
*/
com::RCSize_t::~RCSize_t()
{
}



//! Sets the value of the layered object to \a value.
/*!
  \param     value New value.
*/
void com::RCSize_t::setValue(size_t value)
{
  d_value = value;
}



//! Returns the layered value.
/*!
  \return    Value.
*/
size_t com::RCSize_t::value() const
{
  return d_value;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


