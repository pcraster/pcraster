/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_coefficient.h"
#include <qdom.h>



const std::string pcrxml::Coefficient::d_elementName("Coefficient");
//! ctor
pcrxml::Coefficient::Coefficient(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 ,binValue(element,"binValue",true)
 {
 }
pcrxml::Coefficient::Coefficient():Element()
 {
 }
const std::string& pcrxml::Coefficient::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Coefficient::~Coefficient()
{
 clean();
}
//! clean
void pcrxml::Coefficient::clean()
{
}
//! copy ctor
pcrxml::Coefficient::Coefficient(const Coefficient& src):
pcrxml::Element(src)
,value(src.value)
,binValue(src.binValue)
{
}
//! assignment operator
pcrxml::Coefficient& pcrxml::Coefficient::operator=(const Coefficient& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Coefficient::fill(QDomElement el) const
{
 value.addToElement(el,"value");
 binValue.addToElement(el,"binValue");
}
