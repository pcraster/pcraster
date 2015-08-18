/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_NUMERICSETTING
#include "pcrgenxml_numericsetting.h"
#define INCLUDED_PCRGENXML_NUMERICSETTING
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::NumericSetting::d_elementName("NumericSetting");
//! ctor
pcrxml::NumericSetting::NumericSetting(const QDomElement& element):Element(element,d_elementName)
 ,name(element,"name",true)
 ,value(element,"value",true)
 {
 }
pcrxml::NumericSetting::NumericSetting():Element()
 {
 }
const std::string& pcrxml::NumericSetting::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::NumericSetting::~NumericSetting()
{
 clean();
}
//! clean
void pcrxml::NumericSetting::clean()
{
}
//! copy ctor
pcrxml::NumericSetting::NumericSetting(const NumericSetting& src):
pcrxml::Element(src)
,name(src.name)
,value(src.value)
{
}
//! assignment operator
pcrxml::NumericSetting& pcrxml::NumericSetting::operator=(const NumericSetting& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::NumericSetting::fill(QDomElement el) const
{
 name.addToElement(el,"name");
 value.addToElement(el,"value");
}
