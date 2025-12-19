#ifndef INCLUDED_CALC_IOPIMPL
#define INCLUDED_CALC_IOPIMPL

#include "stddefx.h"


namespace calc {
  // IOpImpl declarations.
}

namespace calc {
class Operator;
class RunTimeEnv;
class PointCodeGenerator;


//! Operation Implementation Interface, actual classes live in calc_opimpl.h
class IOpImpl {

private:

  //! Assignment operator. NOT IMPLEMENTED.
  IOpImpl&           operator=           (const IOpImpl& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   IOpImpl               (const IOpImpl& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   IOpImpl               () {}

  virtual         ~IOpImpl               () {}

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void setPointFunction(const char *)
  {
    PRECOND(false); // should never be called
  }


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  virtual void exec              (RunTimeEnv* rte,
                                  const Operator& op,
                                  size_t nrActualArgs) const=0;

  virtual void genPointCode      (PointCodeGenerator* ) const
  {}

  virtual const char* pointFunction() const
  { return nullptr;}

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
