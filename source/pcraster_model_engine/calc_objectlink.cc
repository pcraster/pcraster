#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OBJECTLINK
#include "calc_objectlink.h"
#define INCLUDED_CALC_OBJECTLINK
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the ObjectLink class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ObjectLinkPrivate
{
public:

  ObjectLinkPrivate()
  {
  }

  ~ObjectLinkPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OBJECTLINK MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF OBJECTLINK MEMBERS
//------------------------------------------------------------------------------

//! ctor
/*!
 * \warning do not inline ctor/dtor. That may result
 * in dynamic_cast failures of derived object created across
 * different dll's
 */
calc::ObjectLink::ObjectLink()
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::ObjectLink::ObjectLink(ObjectLink const& rhs)

  : Base(rhs)

{
}
*/



calc::ObjectLink::~ObjectLink()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::ObjectLink& calc::ObjectLink::operator=(ObjectLink const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! default check dispatcher
/*!
 * zou zelfs in derived class als fallback kunnen dienen
void calc::ObjectLink::check(const std::string&    methodName,
                             const ObjectLinkMeta& olm)
{
  if (methodName.empty())
    ; // XXXXXXXXX
    // call ctorCheck ??
}
*/

calc::OVS calc::ObjectLink::ovs() const
{
  return VS_OBJECT;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



