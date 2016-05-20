#ifndef INCLUDED_CALC_OPERATION
#define INCLUDED_CALC_OPERATION



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_MAJOR_OP
#include "major_op.h"
#define INCLUDED_MAJOR_OP
#endif

// Module headers.



namespace calc {
  // Operation declarations.
  class Operator;
  class ObjectLink;
  class RunTimeEnv;
}



namespace calc {



//! Interface to Python and so on
class Operation
{

  const Operator *d_builtIn;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Operation&           operator=           (Operation const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Operation               (Operation const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Operation               (const Operator* builtIn);

  /* virtual */    ~Operation              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void             checkAndExec            (RunTimeEnv*         rte,
                                            size_t              nrActualArgs) const;

  // and exec'ed
  static ObjectLink* createObjectLink      (const std::string&  dllName,
                                            const std::string&  modelLinkName,
                                            const std::string&  stringArg,
                                            RunTimeEnv*         rte,
                                            size_t              nrFieldArgs);

  static Operation* create                 (MAJOR_CODE opCode);

  static Operation* create                 (const ObjectLink*   obj,
                                            const std::string&  methodName);

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
