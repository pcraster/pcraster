#ifndef INCLUDED_PCRXML_INTEGER
#define INCLUDED_PCRXML_INTEGER

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXML_SIMPLEATTR
#include "pcrxml_simpleattr.h"
#define INCLUDED_PCRXML_SIMPLEATTR
#endif

namespace pcrxml {



//! XML Schema compliant integer
typedef SimpleAttr<int>   Integer;

} // namespace pcrxml

#endif
