#ifndef INCLUDED_PCRXML_DOUBLE
#define INCLUDED_PCRXML_DOUBLE

#include "stddefx.h"
#include "pcrxml_simpleattr.h"


namespace pcrxml {



//! XML Schema compliant double
/*!
    \todo
      careful: i18n decides how to use '.' and ',' !
 */
using Double = SimpleAttr<double>;

} // namespace pcrxml

#endif
