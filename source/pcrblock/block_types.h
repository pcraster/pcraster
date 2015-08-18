#ifndef INCLUDED_BLOCK_TYPES
#define INCLUDED_BLOCK_TYPES



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FUNCTION_FUNCTION2
#include <boost/function/function2.hpp>
#define INCLUDED_BOOST_FUNCTION_FUNCTION2
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



namespace block {
  // Types declarations.

typedef boost::function<REAL4(REAL4, REAL4)> MackeyBridgeCompactor;

}



namespace block {


} // namespace block

#endif
