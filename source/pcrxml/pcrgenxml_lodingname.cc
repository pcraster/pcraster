/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_lodingname.h"
#include <qdom.h>



const std::string pcrxml::LodingName::d_elementName("LodingName");
//! ctor
pcrxml::LodingName::LodingName(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 {
 }
pcrxml::LodingName::LodingName():Element()
 {
 }
const std::string& pcrxml::LodingName::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::LodingName::~LodingName()
{
 clean();
}
//! clean
void pcrxml::LodingName::clean()
{
}
//! copy ctor
pcrxml::LodingName::LodingName(const LodingName& src):
pcrxml::Element(src)
,value(src.value)
{
}
//! assignment operator
pcrxml::LodingName& pcrxml::LodingName::operator=(const LodingName& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::LodingName::fill(QDomElement el) const
{
 value.addToElement(el,"value");
}
