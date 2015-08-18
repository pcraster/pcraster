#ifndef INCLUDED_CALC_OBJECTLINKRUNTIME
#define INCLUDED_CALC_OBJECTLINKRUNTIME

/*!
 * \file
 *   types and interfaces needed to hook up ObjectLink's in the
 *   runtime engine
 */

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace geo {
  class RasterSpace;
}

namespace calc {

class RunTimeEnv;
class ObjectLink;
class Operator;
typedef ObjectLink* (*ObjectLinkFactoryPtr)
                   (const std::string&  stringArg,
                    const geo::RasterSpace& rs,
                    size_t              nrFieldArgs);
typedef ObjectLink* (ObjectLinkFactory)
                   (const std::string&  stringArg,
                    const geo::RasterSpace& rs,
                    size_t              nrFieldArgs);

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
