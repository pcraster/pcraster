/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_RESULTLASTRUN
#include "pcrgenxml_resultlastrun.h"
#define INCLUDED_PCRGENXML_RESULTLASTRUN
#endif

#ifndef INCLUDED_PCRXML_ENUMNMTOKEN
#include "pcrxml_enumnmtoken.h"
#define INCLUDED_PCRXML_ENUMNMTOKEN
#endif




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

