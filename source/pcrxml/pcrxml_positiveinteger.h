#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#define INCLUDED_PCRXML_POSITIVEINTEGER

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



//! XML Schema compliant positive integer
typedef SimpleAttr<size_t>   PositiveInteger;

} // namespace pcrxml

#endif
