#ifndef INCLUDED_CALC_TIMEINPUTTSSOP
#define INCLUDED_CALC_TIMEINPUTTSSOP



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
  // TimeinputTssOp declarations.
}



namespace calc {


//! Operation Implementation
class TimeinputTssOp: public IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TimeinputTssOp&           operator=           (const TimeinputTssOp& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TimeinputTssOp               (const TimeinputTssOp& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TimeinputTssOp               ();

  virtual         ~TimeinputTssOp               ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const;
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
