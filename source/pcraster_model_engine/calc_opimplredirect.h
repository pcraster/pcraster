#ifndef INCLUDED_CALC_OPIMPLREDIRECT
#define INCLUDED_CALC_OPIMPLREDIRECT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_IOPIMPL
#include "calc_iopimpl.h"
#define INCLUDED_CALC_IOPIMPL
#endif



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

  const IOpImpl*   d_redirect;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OpImplRedirect               (const IOpImpl *redirect);
                   OpImplRedirect               ();

  /* virtual */    ~OpImplRedirect              ();

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
