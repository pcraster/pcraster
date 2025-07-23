#include "stddefx.h"
#include "calc_opimplredirect.h"



/*!
  \file
  This file contains the implementation of the OpImplRedirect class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class OpImplRedirectPrivate
{
public:

  OpImplRedirectPrivate()
  {
  }

  ~OpImplRedirectPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPIMPLREDIRECT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF OPIMPLREDIRECT MEMBERS
//------------------------------------------------------------------------------

calc::OpImplRedirect::OpImplRedirect()
   
{
}

calc::OpImplRedirect::OpImplRedirect(const IOpImpl *redirect):
    d_redirect(redirect)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::OpImplRedirect::OpImplRedirect(OpImplRedirect const& rhs)

  : Base(rhs)

{
}
*/



calc::OpImplRedirect::~OpImplRedirect()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::OpImplRedirect& calc::OpImplRedirect::operator=(OpImplRedirect const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/


//! get value of d_redirect
const calc::IOpImpl* calc::OpImplRedirect::redirect() const
{
  return d_redirect;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



