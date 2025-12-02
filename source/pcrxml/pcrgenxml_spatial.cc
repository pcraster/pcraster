/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_spatial.h"
#include "pcrxml_enumnmtoken.h"

namespace pcrxml
{
template <> const char *EnumNmToken<Spatial>::d_strings[] = {"Yes", "Non", "Either", "NA"};

//! generated from DTD
template <> const size_t EnumNmToken<Spatial>::d_nrStrings(ARRAY_SIZE(EnumNmToken<Spatial>::d_strings));
//! generated from DTD
static const EnumNmToken<Spatial> classSpatial;
}  // namespace pcrxml

pcrxml::Spatial::Spatial(const QDomNode &owningElement, const std::string &nameOfAttr, bool req)
    : Attribute(owningElement, nameOfAttr, req)
{
  if (present())
    d_value = classSpatial.value(owningElement, nameOfAttr);
}

pcrxml::Spatial::Spatial() : Attribute(false)
{
}

pcrxml::Spatial::Spatial(EnumType value) : Attribute(true), d_value(value)
{
}

std::string pcrxml::Spatial::attrValueStr() const
{
  return classSpatial.attrValueStr(value());
}
