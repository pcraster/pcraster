/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_iotype.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <>
const char *EnumNmToken<IoType>::d_strings[] = {"Input", "Output", "Constant", "Both", "None"};

//! generated from DTD
template <> const size_t EnumNmToken<IoType>::d_nrStrings(ARRAY_SIZE(EnumNmToken<IoType>::d_strings));
//! generated from DTD
static const EnumNmToken<IoType> classIoType;
}  // namespace pcrxml

pcrxml::IoType::IoType(const QDomNode &owningElement, const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present()) {
    d_value = classIoType.value(owningElement, nameOfAttr);
  }
}

pcrxml::IoType::IoType() : Attribute(false)
{
}

pcrxml::IoType::IoType(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::IoType::attrValueStr() const
{
  return classIoType.attrValueStr(value());
}
