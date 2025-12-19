#ifndef INCLUDED_CALC_XMLCONTEXT
#define INCLUDED_CALC_XMLCONTEXT

#include "stddefx.h"



namespace calc {
  // XMLContext declarations.
  class AreaMap;
  class Timer;
}
namespace pcrxml {
  class RunContext;
  class CheckContext;
}



namespace calc {

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------
pcrxml::RunContext*   createXMLRunContext  (AreaMap const& areaMap, Timer const& timer);
pcrxml::CheckContext* createXMLCheckContext(AreaMap const& areaMap, Timer const& timer);

} // namespace calc

#endif
