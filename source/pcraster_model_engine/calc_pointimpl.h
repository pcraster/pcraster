#ifndef INCLUDED_CALC_POINTIMPL
#define INCLUDED_CALC_POINTIMPL

#include "stddefx.h"
#include "calc_iopimpl.h"



namespace calc {
  // PointImpl declarations.
}



namespace calc {



//! having a possible point implementation
class PointImpl : public IOpImpl
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  PointImpl&           operator=           (PointImpl const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   PointImpl               (PointImpl const& rhs);

  const char *     d_pointFunction{nullptr};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PointImpl               ();

  /* virtual */    ~PointImpl              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setPointFunction    (const char * pointFunction) override;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const char *     pointFunction       () const override;
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
