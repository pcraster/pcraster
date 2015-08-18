#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OPERATION
#include "calc_operation.h"
#define INCLUDED_CALC_OPERATION
#endif

// Library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_RTTYPECHECK
#include "calc_rttypecheck.h"
#define INCLUDED_CALC_RTTYPECHECK
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif


/*!
  \file
  This file contains the implementation of the Operation class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class OperationPrivate
{
public:

  OperationPrivate()
  {
  }

  ~OperationPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OPERATION MEMBERS
//------------------------------------------------------------------------------

calc::Operation* calc::Operation::create(MAJOR_CODE opCode)
{
  return new Operation(major2op(opCode));
}
calc::ObjectLink*  calc::Operation::createObjectLink(
         const std::string&  dllName,
         const std::string&  modelLinkName,
         const std::string&  stringArg,
         RunTimeEnv*         rte,
         size_t              nrFieldArgs)
{
  if (!stringArg.empty())
    throw com::Exception("string argument to objectlink not yet supported");
  return 0;
}

calc::Operation* calc::Operation::create(
         const ObjectLink*   obj,
         const std::string&  methodName)
{
  PRECOND(obj);
}

//------------------------------------------------------------------------------
// DEFINITION OF OPERATION MEMBERS
//------------------------------------------------------------------------------

calc::Operation::Operation(const Operator *builtIn):
  d_builtIn(builtIn)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::Operation::Operation(Operation const& rhs)

  : Base(rhs)

{
}
*/



calc::Operation::~Operation()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::Operation& calc::Operation::operator=(Operation const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



