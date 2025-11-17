#ifndef INCLUDED_PCRXML_DOUBLE
#define INCLUDED_PCRXML_DOUBLE

#include "stddefx.h"
#include "pcrxml_simpleattr.h"


namespace pcrxml {



//! XML Schema compliant double
/*!
    \todo
      carefull: i18n decides how to use '.' and ',' !
 */
typedef SimpleAttr<double>   Double;

} // namespace pcrxml

#endif
