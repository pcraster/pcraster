/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_SCRIPTTYPE
#include "pcrgenxml_scripttype.h"
#define INCLUDED_PCRGENXML_SCRIPTTYPE
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




namespace pcrxml {
template<> const char *EnumNmToken<ScriptType>::d_strings[] = {
 "Static","Dynamic"
};

//! generated from DTD
template<> const size_t EnumNmToken<ScriptType>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<ScriptType>::d_strings));
//! generated from DTD
static const EnumNmToken<ScriptType> classScriptType;
}

pcrxml::ScriptType::ScriptType(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classScriptType.value(owningElement,nameOfAttr);
}

pcrxml::ScriptType::ScriptType():
    Attribute(false)
{
}

pcrxml::ScriptType::ScriptType(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::ScriptType::attrValueStr() const
{
    return classScriptType.attrValueStr(value());
}

