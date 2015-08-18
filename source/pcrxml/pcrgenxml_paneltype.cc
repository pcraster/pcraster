/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_PANELTYPE
#include "pcrgenxml_paneltype.h"
#define INCLUDED_PCRGENXML_PANELTYPE
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<PanelType>::d_strings[] = {
 "Tab","Box"
};

//! generated from DTD
template<> const size_t EnumNmToken<PanelType>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<PanelType>::d_strings));
//! generated from DTD
static const EnumNmToken<PanelType> classPanelType;
}

pcrxml::PanelType::PanelType(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classPanelType.value(owningElement,nameOfAttr);
}

pcrxml::PanelType::PanelType():
    Attribute(false)
{
}

pcrxml::PanelType::PanelType(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::PanelType::attrValueStr() const
{
    return classPanelType.attrValueStr(value());
}

