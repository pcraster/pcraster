/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_IOSTRATEGY
#include "pcrgenxml_iostrategy.h"
#define INCLUDED_PCRGENXML_IOSTRATEGY
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




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

