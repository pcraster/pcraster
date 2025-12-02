/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_dateformat.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<DateFormat>::d_strings[] = {"MONDDYYYY", "ISO8601"};

//! generated from DTD
template <>
const size_t EnumNmToken<DateFormat>::d_nrStrings(ARRAY_SIZE(EnumNmToken<DateFormat>::d_strings));
//! generated from DTD
static const EnumNmToken<DateFormat> classDateFormat;
}  // namespace pcrxml

pcrxml::DateFormat::DateFormat(const QDomNode &owningElement, const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classDateFormat.value(owningElement, nameOfAttr);
}

pcrxml::DateFormat::DateFormat() : Attribute(false)
{
}

pcrxml::DateFormat::DateFormat(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::DateFormat::attrValueStr() const
{
  return classDateFormat.attrValueStr(value());
}
