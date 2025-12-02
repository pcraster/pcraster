/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_exchangedirection.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<ExchangeDirection>::d_strings[] = {"Input", "Output"};

//! generated from DTD
template <>
const size_t
    EnumNmToken<ExchangeDirection>::d_nrStrings(ARRAY_SIZE(EnumNmToken<ExchangeDirection>::d_strings));
//! generated from DTD
static const EnumNmToken<ExchangeDirection> classExchangeDirection;
}  // namespace pcrxml

pcrxml::ExchangeDirection::ExchangeDirection(const QDomNode &owningElement,
                                             const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classExchangeDirection.value(owningElement, nameOfAttr);
}

pcrxml::ExchangeDirection::ExchangeDirection() : Attribute(false)
{
}

pcrxml::ExchangeDirection::ExchangeDirection(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::ExchangeDirection::attrValueStr() const
{
  return classExchangeDirection.attrValueStr(value());
}
