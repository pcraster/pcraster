#ifndef INCLUDED_CALC_OBJECTLINKRUNTIME
#define INCLUDED_CALC_OBJECTLINKRUNTIME

/*!
 * \file
 *   types and interfaces needed to hook up ObjectLink's in the
 *   runtime engine
 */

#include "stddefx.h"

#include <string>


namespace geo {
  class RasterSpace;
}

namespace calc {

class RunTimeEnv;
class ObjectLink;
class Operator;
using ObjectLinkFactoryPtr = ObjectLink *(*)(const std::string &, const geo::RasterSpace &, size_t);
using ObjectLinkFactory = ObjectLink *(const std::string &, const geo::RasterSpace &, size_t);

void createObjectLink(
      const Operator&      op,
      ObjectLinkFactoryPtr olf,
      const std::string&   stringArg,
      RunTimeEnv*          rte,
      size_t               nrFieldArgs);
void execObjectLinkMethod(const Operator& op,
                          RunTimeEnv*     rte,
                          size_t          nrFieldArgs);
} // namespace calc

#endif
