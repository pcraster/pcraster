/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_datatypeenum.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <>
const char *EnumNmToken<DataTypeEnum>::d_strings[] = {
    "Tss", "Table", "Field", "Unknown", "Boolean", "Nominal", "Ordinal", "Scalar", "Directional", "Ldd"};

//! generated from DTD
template <>
const size_t EnumNmToken<DataTypeEnum>::d_nrStrings(ARRAY_SIZE(EnumNmToken<DataTypeEnum>::d_strings));
//! generated from DTD
static const EnumNmToken<DataTypeEnum> classDataTypeEnum;
}  // namespace pcrxml

pcrxml::DataTypeEnum::DataTypeEnum(const QDomNode &owningElement, const std::string &nameOfAttr,
                                   bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present()) {
    d_value = classDataTypeEnum.value(owningElement, nameOfAttr);
  }
}

pcrxml::DataTypeEnum::DataTypeEnum() : Attribute(false)
{
}

pcrxml::DataTypeEnum::DataTypeEnum(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::DataTypeEnum::attrValueStr() const
{
  return classDataTypeEnum.attrValueStr(value());
}
