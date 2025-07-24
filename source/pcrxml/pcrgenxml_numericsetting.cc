/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_numericsetting.h"
#include <qdom.h>



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
