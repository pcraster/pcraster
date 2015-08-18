#ifndef INCLUDED_CALC_POINTIMPL
#define INCLUDED_CALC_POINTIMPL



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

  const char *     d_pointFunction;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PointImpl               ();

  /* virtual */    ~PointImpl              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setPointFunction    (const char * pointFunction);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const char *     pointFunction       () const;
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
