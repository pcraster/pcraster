/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATAENVELOPENCODING
#include "pcrgenxml_dataenvelopencoding.h"
#define INCLUDED_PCRGENXML_DATAENVELOPENCODING
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<DataEnvelopEncoding>::d_strings[] = {
 "text"
};

//! generated from DTD
template<> const size_t EnumNmToken<DataEnvelopEncoding>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<DataEnvelopEncoding>::d_strings));
//! generated from DTD
static const EnumNmToken<DataEnvelopEncoding> classDataEnvelopEncoding;
}

pcrxml::DataEnvelopEncoding::DataEnvelopEncoding(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classDataEnvelopEncoding.value(owningElement,nameOfAttr);
}

pcrxml::DataEnvelopEncoding::DataEnvelopEncoding():
    Attribute(false)
{
}

pcrxml::DataEnvelopEncoding::DataEnvelopEncoding(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::DataEnvelopEncoding::attrValueStr() const
{
    return classDataEnvelopEncoding.attrValueStr(value());
}

