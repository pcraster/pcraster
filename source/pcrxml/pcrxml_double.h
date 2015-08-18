#ifndef INCLUDED_PCRXML_DOUBLE
#define INCLUDED_PCRXML_DOUBLE

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



//! XML Schema compliant double
/*!
    \todo
      carefull: i18n decides how to use '.' and ',' !
 */
typedef SimpleAttr<double>   Double;

} // namespace pcrxml

#endif
