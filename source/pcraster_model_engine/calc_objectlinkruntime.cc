#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_OBJECTLINKRUNTIME
#include "calc_objectlinkruntime.h"
#define INCLUDED_CALC_OBJECTLINKRUNTIME
#endif
// Library headers.
#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif
#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_RUNTIMEENV
#include "calc_runtimeenv.h"
#define INCLUDED_CALC_RUNTIMEENV
#endif
#ifndef INCLUDED_CALC_OBJECTLINK
#include "calc_objectlink.h"
#define INCLUDED_CALC_OBJECTLINK
#endif
#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif
//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*!
 * \todo
 *   lijkt eigenlijk heel veel op een method invocation
 */
void calc::createObjectLink(
      const Operator&      /* op */,
      ObjectLinkFactoryPtr olf,
      const std::string&   /* stringArg */,
      RunTimeEnv*          rte,
      size_t               /* nrFieldArgs */)
{
  // PRECOND(stringArg.empty()); // not yet implemented
  // PRECOND(!nrFieldArgs); // not yet implemented
  ObjectLink *o(0);
  o = olf("",rte->rasterSpace(),0);
  rte->pushDataValue(o);
}

/*!
 * type1 execution
 * \throws
 *   com::Exception if there is no ObjectLink on the rte.stack
 *   or not such methodName
 */
void calc::execObjectLinkMethod(
        const Operator& op,
        RunTimeEnv*     rte,
        size_t          nrFieldArgs)
{
  try {
    std::vector<Field *> data;
    try {
    // both results and input

    // results
    for (size_t i=0; i < op.nrResults(); ++i)
      data.push_back(rte->createResultField(op.resultType(i)));

    // input
    for (size_t i=0; i < nrFieldArgs; ++i)
      data.push_back(rte->popField());
    // reverse input part
    std::reverse(data.begin()+op.nrResults(),data.end());

    ObjectLink* o(0);
    if (rte->stackSize())
      o=dynamic_cast<ObjectLink *>(rte->popDataValue());
    if (!o)
      throw com::Exception((
          boost::format("Method '%1%' called while no ObjectLink present")
           % op.implName()).str());

    o->exec1(op.implName(), data);

    // put result on stack
    size_t in=0;
    for (;in < op.nrResults(); ++in) {
      rte->pushField(data[in]);
      data[in]=0;
    }
    // delete inputs
    for (;in < op.nrResults()+nrFieldArgs; ++in)
      deleteFromPcrme(data[in]);

    } catch (...) {
      for (size_t i=0;i < data.size(); ++i)
        deleteFromPcrme(data[i]);
      throw;
    }
  } catch (const std::out_of_range& ) {
    // ObjectLinkProxy has checked vector access
    throw com::Exception(
        (boost::format(" '%1%' called with too few arguments")
         % op.name()).str());
  } catch (const ObjectLink::UnknownMethod& ) {
    throw com::Exception("Unknown method/function name");
  }
}
