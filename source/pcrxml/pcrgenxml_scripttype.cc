/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_scripttype.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<ScriptType>::d_strings[] = {"Static", "Dynamic"};

//! generated from DTD
template <>
const size_t EnumNmToken<ScriptType>::d_nrStrings(ARRAY_SIZE(EnumNmToken<ScriptType>::d_strings));
//! generated from DTD
static const EnumNmToken<ScriptType> classScriptType;
}  // namespace pcrxml

pcrxml::ScriptType::ScriptType(const QDomNode &owningElement, const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classScriptType.value(owningElement, nameOfAttr);
}

pcrxml::ScriptType::ScriptType() : Attribute(false)
{
}

pcrxml::ScriptType::ScriptType(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::ScriptType::attrValueStr() const
{
  return classScriptType.attrValueStr(value());
}
