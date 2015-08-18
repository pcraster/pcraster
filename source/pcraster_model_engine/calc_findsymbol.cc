#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.
#ifndef INCLUDED_APP
#include "app.h"
#define INCLUDED_APP
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif


#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_CALC_OPERATIONS
#include "calc_operations.h"
#define INCLUDED_CALC_OPERATIONS
#endif

//------------------------------------------------------------------------------

namespace calc {
 Operations globalOperations;
}

//! find an operator by its name
/*! \returns 0 if opName if not a valid name
 *  It can find loaded ObjectLink ctor's and methods.
 *  For example, considering an ObjectLink loaded named ObjectLinkTest
 *  with a method named methodTest:
 *  \param opName  name of operator/function (e.g. +,sin,Class::Method)
 *  \param nrArgs  default 2, only relevant if unary/binary +,- must be discerned
 *  \code
 *    const Operator* ctor=opName2op("ObjectLinkTest");
 *    const Operator* method=opName2op("ObjectLinkTest::methodTest");
 *  \endcode
 * \returns 0 if not found
 */
PCR_DLL_C const calc::Operator* calc::opName2op(const std::string& opName, size_t nrArgs)
{
  const Operator* op(globalOperations[opName]);
  if (!op)
     return 0;

  // discern for binary or unary + and -
  switch(op->opCode()) {
    case OP_UADD:
    case OP_BADD: if (nrArgs==2)
                      return globalOperations[OP_BADD];
                  return globalOperations[OP_UADD];
    case OP_UMIN:
    case OP_BMIN: if (nrArgs==2)
                      return globalOperations[OP_BMIN];
                  return globalOperations[OP_UMIN];
    default: return op;
  }
}

size_t calc::nrInternalOpCodes() {
  return globalOperations.size();
}


MAJOR_CODE calc::otherOneOfMRF(MAJOR_CODE op)
{
  return globalOperations.otherOneOfMRF(op);
}

const calc::Operator& calc::oneOf2Mrf(MAJOR_CODE op)
{
  return *major2op(globalOperations.oneOf2Mrf(op));
}

//! declared here, defined in calc_findsymbol.cc
bool calc::oneOfMrfIsStackTop(MAJOR_CODE oneOfMRF)
{
  return globalOperations.oneOfMrfIsStackTop(oneOfMRF);
}

PCR_DLL_C const calc::Operator* calc::major2op(MAJOR_CODE major)
{
  return globalOperations[major];
}


//! add;s ObjectLink's present in dll to globally known Operator's
PCR_DLL_C void calc::loadCalcLib(const std::string& dllName)
{
  globalOperations.loadLib(dllName);
}
