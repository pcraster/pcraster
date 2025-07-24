/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_iostrategytype.h"
#include "pcrxml_enumnmtoken.h"




namespace pcrxml {
template<> const char *EnumNmToken<IOStrategyType>::d_strings[] = {
 "IOFiles","IOMemory"
};

//! generated from DTD
template<> const size_t EnumNmToken<IOStrategyType>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<IOStrategyType>::d_strings));
//! generated from DTD
static const EnumNmToken<IOStrategyType> classIOStrategyType;
}

pcrxml::IOStrategyType::IOStrategyType(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classIOStrategyType.value(owningElement,nameOfAttr);
}

pcrxml::IOStrategyType::IOStrategyType():
    Attribute(false)
{
}

pcrxml::IOStrategyType::IOStrategyType(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::IOStrategyType::attrValueStr() const
{
    return classIOStrategyType.attrValueStr(value());
}

