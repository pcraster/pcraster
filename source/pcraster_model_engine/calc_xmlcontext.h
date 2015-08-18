#ifndef INCLUDED_CALC_XMLCONTEXT
#define INCLUDED_CALC_XMLCONTEXT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



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
