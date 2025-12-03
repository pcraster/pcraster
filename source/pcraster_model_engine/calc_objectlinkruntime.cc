#include "stddefx.h"
#include "calc_objectlinkruntime.h"
#include "com_exception.h"
#include "calc_operator.h"
#include "calc_runtimeenv.h"
#include "calc_objectlink.h"
#include "calc_field.h"

#include <format>
#include <stdexcept>

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

/*!
 * \todo
 *   lijkt eigenlijk heel veel op een method invocation
 */
void calc::createObjectLink(const Operator & /* op */, ObjectLinkFactoryPtr olf,
                            const std::string & /* stringArg */, RunTimeEnv *rte,
                            size_t /* nrFieldArgs */)
{
  // PRECOND(stringArg.empty()); // not yet implemented
  // PRECOND(!nrFieldArgs); // not yet implemented
  ObjectLink *o(nullptr);
  o = olf("", rte->rasterSpace(), 0);
  rte->pushDataValue(o);
}

/*!
 * type1 execution
 * \throws
 *   com::Exception if there is no ObjectLink on the rte.stack
 *   or not such methodName
 */
void calc::execObjectLinkMethod(const Operator &op, RunTimeEnv *rte, size_t nrFieldArgs)
{
  try {
    std::vector<Field *> data;
    try {
      // both results and input

      // results
      for (size_t i = 0; i < op.nrResults(); ++i) {
        data.push_back(rte->createResultField(op.resultType(i)));
      }

      // input
      for (size_t i = 0; i < nrFieldArgs; ++i) {
        data.push_back(rte->popField());
      }
      // reverse input part
      std::reverse(data.begin() + op.nrResults(), data.end());

      ObjectLink *o(nullptr);
      if (rte->stackSize()) {
        o = dynamic_cast<ObjectLink *>(rte->popDataValue());
      }
      if (!o) {
        throw com::Exception(
            std::format("Method '{0}' called while no ObjectLink present", op.implName()));
      }

      o->exec1(op.implName(), data);

      // put result on stack
      size_t in = 0;
      for (; in < op.nrResults(); ++in) {
        rte->pushField(data[in]);
        data[in] = nullptr;
      }
      // delete inputs
      for (; in < op.nrResults() + nrFieldArgs; ++in) {
        deleteFromPcrme(data[in]);
      }

    } catch (...) {
      for (auto &i : data) {
        deleteFromPcrme(i);
      }
      throw;
    }
  } catch (const std::out_of_range &) {
    // ObjectLinkProxy has checked vector access
    throw com::Exception(std::format(" '{0}' called with too few arguments", op.name()));
  } catch (const ObjectLink::UnknownMethod &) {
    throw com::Exception("Unknown method/function name");
  }
}
