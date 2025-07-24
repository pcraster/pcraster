/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_iostrategy.h"
#include "pcrxml_enumnmtoken.h"




namespace pcrxml {
template<> const char *EnumNmToken<IoStrategy>::d_strings[] = {
 "PCRaster","EsriGrid","Band"
};

//! generated from DTD
template<> const size_t EnumNmToken<IoStrategy>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<IoStrategy>::d_strings));
//! generated from DTD
static const EnumNmToken<IoStrategy> classIoStrategy;
}

pcrxml::IoStrategy::IoStrategy(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classIoStrategy.value(owningElement,nameOfAttr);
}

pcrxml::IoStrategy::IoStrategy():
    Attribute(false)
{
}

pcrxml::IoStrategy::IoStrategy(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::IoStrategy::attrValueStr() const
{
    return classIoStrategy.attrValueStr(value());
}

