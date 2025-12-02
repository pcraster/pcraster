/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_inputtype.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<InputType>::d_strings[] = {"Constant", "None", "Initial", "Dynamic"};

//! generated from DTD
template <>
const size_t EnumNmToken<InputType>::d_nrStrings(ARRAY_SIZE(EnumNmToken<InputType>::d_strings));
//! generated from DTD
static const EnumNmToken<InputType> classInputType;
}  // namespace pcrxml

pcrxml::InputType::InputType(const QDomNode &owningElement, const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classInputType.value(owningElement, nameOfAttr);
}

pcrxml::InputType::InputType() : Attribute(false)
{
}

pcrxml::InputType::InputType(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::InputType::attrValueStr() const
{
  return classInputType.attrValueStr(value());
}
