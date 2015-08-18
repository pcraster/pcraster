/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATEFORMAT
#include "pcrgenxml_dateformat.h"
#define INCLUDED_PCRGENXML_DATEFORMAT
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<DateFormat>::d_strings[] = {
 "MONDDYYYY","ISO8601"
};

//! generated from DTD
template<> const size_t EnumNmToken<DateFormat>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<DateFormat>::d_strings));
//! generated from DTD
static const EnumNmToken<DateFormat> classDateFormat;
}

pcrxml::DateFormat::DateFormat(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classDateFormat.value(owningElement,nameOfAttr);
}

pcrxml::DateFormat::DateFormat():
    Attribute(false)
{
}

pcrxml::DateFormat::DateFormat(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::DateFormat::attrValueStr() const
{
    return classDateFormat.attrValueStr(value());
}

