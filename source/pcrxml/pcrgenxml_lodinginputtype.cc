/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_LODINGINPUTTYPE
#include "pcrgenxml_lodinginputtype.h"
#define INCLUDED_PCRGENXML_LODINGINPUTTYPE
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<LodingInputType>::d_strings[] = {
 "ASCII","RWSLOD"
};

//! generated from DTD
template<> const size_t EnumNmToken<LodingInputType>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<LodingInputType>::d_strings));
//! generated from DTD
static const EnumNmToken<LodingInputType> classLodingInputType;
}

pcrxml::LodingInputType::LodingInputType(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classLodingInputType.value(owningElement,nameOfAttr);
}

pcrxml::LodingInputType::LodingInputType():
    Attribute(false)
{
}

pcrxml::LodingInputType::LodingInputType(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::LodingInputType::attrValueStr() const
{
    return classLodingInputType.attrValueStr(value());
}

