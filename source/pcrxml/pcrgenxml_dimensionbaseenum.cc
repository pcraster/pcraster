/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_dimensionbaseenum.h"
#include "pcrxml_enumnmtoken.h"




namespace pcrxml {
template<> const char *EnumNmToken<DimensionBaseEnum>::d_strings[] = {
 "Length","Mass","Time","ElectricCurrent","Temperature","AmountOfSubstance","LuminousIntensity","Currency"
};

//! generated from DTD
template<> const size_t EnumNmToken<DimensionBaseEnum>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<DimensionBaseEnum>::d_strings));
//! generated from DTD
static const EnumNmToken<DimensionBaseEnum> classDimensionBaseEnum;
}

pcrxml::DimensionBaseEnum::DimensionBaseEnum(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classDimensionBaseEnum.value(owningElement,nameOfAttr);
}

pcrxml::DimensionBaseEnum::DimensionBaseEnum():
    Attribute(false)
{
}

pcrxml::DimensionBaseEnum::DimensionBaseEnum(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::DimensionBaseEnum::attrValueStr() const
{
    return classDimensionBaseEnum.attrValueStr(value());
}

