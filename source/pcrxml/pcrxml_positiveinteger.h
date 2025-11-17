#ifndef INCLUDED_PCRXML_POSITIVEINTEGER
#define INCLUDED_PCRXML_POSITIVEINTEGER

#include "stddefx.h"
#include "pcrxml_simpleattr.h"


namespace pcrxml {



//! XML Schema compliant positive integer
typedef SimpleAttr<size_t>   PositiveInteger;

} // namespace pcrxml

#endif
