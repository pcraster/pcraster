/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_resultlastrun.h"
#include "pcrxml_enumnmtoken.h"




namespace pcrxml {
template<> const char *EnumNmToken<ResultLastRun>::d_strings[] = {
 "None","Incomplete","New","Error","Canceled","Done","Changed"
};

//! generated from DTD
template<> const size_t EnumNmToken<ResultLastRun>::d_nrStrings(
 ARRAY_SIZE(EnumNmToken<ResultLastRun>::d_strings));
//! generated from DTD
static const EnumNmToken<ResultLastRun> classResultLastRun;
}

pcrxml::ResultLastRun::ResultLastRun(
   const QDomNode& owningElement, const std::string& nameOfAttr, bool req):
   Attribute(owningElement,nameOfAttr,req)
{
  if (present())
    d_value= classResultLastRun.value(owningElement,nameOfAttr);
}

pcrxml::ResultLastRun::ResultLastRun():
    Attribute(false)
{
}

pcrxml::ResultLastRun::ResultLastRun(EnumType value):
    Attribute(true),
    d_value(value)
{
}

std::string pcrxml::ResultLastRun::attrValueStr() const
{
    return classResultLastRun.attrValueStr(value());
}

