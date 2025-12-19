#ifndef INCLUDED_CALC_OPIMPLREDIRECT
#define INCLUDED_CALC_OPIMPLREDIRECT

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  // OpImplRedirect declarations.
}



namespace calc {



//! temporary stub for possible new interfaces
/*!
 *  this class is only for the transitition to the new interfaces
 *  want to dump current Global and MRF classes completely when all
 *  are implemented new. We wish to have them all moved to Exec::Direct
 *  as defined in operation.dtd
 */
class OpImplRedirect : public IOpImpl
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  OpImplRedirect&           operator=           (OpImplRedirect const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   OpImplRedirect               (OpImplRedirect const& rhs);

  const IOpImpl*   d_redirect{nullptr};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OpImplRedirect               (const IOpImpl *redirect);
                   OpImplRedirect               ();

  /* virtual */    ~OpImplRedirect              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const IOpImpl*    redirect                   () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
