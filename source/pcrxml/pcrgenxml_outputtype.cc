/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_OUTPUTTYPE
#include "pcrgenxml_outputtype.h"
#define INCLUDED_PCRGENXML_OUTPUTTYPE
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<OutputType>::d_strings[] = {
 "Fixed","Initial","Dynamic"
};

//! generated from DTD
template<> const size_t EnumNmToken<OutputType>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<OutputType>::d_strings));
//! generated from DTD
static const EnumNmToken<OutputType> classOutputType;
}

pcrxml::OutputType::OutputType(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classOutputType.value(owningElement,nameOfAttr);
}

pcrxml::OutputType::OutputType():
    Attribute(false)
{
}

pcrxml::OutputType::OutputType(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::OutputType::attrValueStr() const
{
    return classOutputType.attrValueStr(value());
}

