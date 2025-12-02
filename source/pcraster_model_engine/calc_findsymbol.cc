#include "stddefx.h"
#include "calc_findsymbol.h"
#include "app.h"
#include "misc.h"
#include "com_exception.h"
#include "calc_operator.h"
#include "calc_quote.h"
#include "calc_operations.h"

#include <sstream>
#include <vector>
#include <map>

//------------------------------------------------------------------------------

namespace calc
{
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
PCR_ME_EXPORT const calc::Operator *calc::opName2op(const std::string &opName, size_t nrArgs)
{
  const Operator *op(globalOperations[opName]);
  if (!op)
    return nullptr;

  // discern for binary or unary + and -
  switch (op->opCode()) {
    case OP_UADD:
    case OP_BADD:
      if (nrArgs == 2)
        return globalOperations[OP_BADD];
      return globalOperations[OP_UADD];
    case OP_UMIN:
    case OP_BMIN:
      if (nrArgs == 2)
        return globalOperations[OP_BMIN];
      return globalOperations[OP_UMIN];
    default:
      return op;
  }
}

size_t calc::nrInternalOpCodes()
{
  return globalOperations.size();
}

MAJOR_CODE calc::otherOneOfMRF(MAJOR_CODE op)
{
  return globalOperations.otherOneOfMRF(op);
}

const calc::Operator &calc::oneOf2Mrf(MAJOR_CODE op)
{
  return *major2op(globalOperations.oneOf2Mrf(op));
}

//! declared here, defined in calc_findsymbol.cc
bool calc::oneOfMrfIsStackTop(MAJOR_CODE oneOfMRF)
{
  return globalOperations.oneOfMrfIsStackTop(oneOfMRF);
}

PCR_ME_EXPORT const calc::Operator *calc::major2op(MAJOR_CODE major)
{
  return globalOperations[major];
}

//! add;s ObjectLink's present in dll to globally known Operator's
PCR_ME_EXPORT void calc::loadCalcLib(const std::string &dllName)
{
  globalOperations.loadLib(dllName);
}
