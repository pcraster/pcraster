#ifndef INCLUDED_CALC_TIMEINPUTTSSOP
#define INCLUDED_CALC_TIMEINPUTTSSOP

#include "stddefx.h"
#include "calc_iopimpl.h"



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

          ~TimeinputTssOp               () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void exec  (RunTimeEnv* rte,const Operator& op,size_t nrArgs) const override;
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
