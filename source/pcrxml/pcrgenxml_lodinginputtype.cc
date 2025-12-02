/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_lodinginputtype.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<LodingInputType>::d_strings[] = {"ASCII", "RWSLOD"};

//! generated from DTD
template <>
const size_t
    EnumNmToken<LodingInputType>::d_nrStrings(ARRAY_SIZE(EnumNmToken<LodingInputType>::d_strings));
//! generated from DTD
static const EnumNmToken<LodingInputType> classLodingInputType;
}  // namespace pcrxml

pcrxml::LodingInputType::LodingInputType(const QDomNode &owningElement, const std::string &nameOfAttr,
                                         bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present()) {
    d_value = classLodingInputType.value(owningElement, nameOfAttr);
  }
}

pcrxml::LodingInputType::LodingInputType() : Attribute(false)
{
}

pcrxml::LodingInputType::LodingInputType(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::LodingInputType::attrValueStr() const
{
  return classLodingInputType.attrValueStr(value());
}
