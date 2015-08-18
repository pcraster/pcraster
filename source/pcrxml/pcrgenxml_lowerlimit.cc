/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_LOWERLIMIT
#include "pcrgenxml_lowerlimit.h"
#define INCLUDED_PCRGENXML_LOWERLIMIT
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::LowerLimit::d_elementName("LowerLimit");
//! ctor
pcrxml::LowerLimit::LowerLimit(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 ,inclusive(element,"inclusive",true)
 {
 }
pcrxml::LowerLimit::LowerLimit():Element()
 {
 }
const std::string& pcrxml::LowerLimit::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::LowerLimit::~LowerLimit()
{
 clean();
}
//! clean
void pcrxml::LowerLimit::clean()
{
}
//! copy ctor
pcrxml::LowerLimit::LowerLimit(const LowerLimit& src):
pcrxml::Element(src)
,value(src.value)
,inclusive(src.inclusive)
{
}
//! assignment operator
pcrxml::LowerLimit& pcrxml::LowerLimit::operator=(const LowerLimit& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::LowerLimit::fill(QDomElement el) const
{
 value.addToElement(el,"value");
 inclusive.addToElement(el,"inclusive");
}
