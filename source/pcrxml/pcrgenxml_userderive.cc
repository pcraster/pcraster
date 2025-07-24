/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_userderive.h"
#include "pcrxml_enumnmtoken.h"




namespace pcrxml {
template<> const char *EnumNmToken<UserDerive>::d_strings[] = {
 "Tree","Child","None"
};

//! generated from DTD
template<> const size_t EnumNmToken<UserDerive>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<UserDerive>::d_strings));
//! generated from DTD
static const EnumNmToken<UserDerive> classUserDerive;
}

pcrxml::UserDerive::UserDerive(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classUserDerive.value(owningElement,nameOfAttr);
}

pcrxml::UserDerive::UserDerive():
    Attribute(false)
{
}

pcrxml::UserDerive::UserDerive(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::UserDerive::attrValueStr() const
{
    return classUserDerive.attrValueStr(value());
}

