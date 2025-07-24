/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_timesteprange.h"
#include <qdom.h>



const std::string pcrxml::TimestepRange::d_elementName("TimestepRange");
//! ctor
pcrxml::TimestepRange::TimestepRange(const QDomElement& element):Element(element,d_elementName)
 ,first(element,"first",true)
 ,last(element,"last",true)
 {
 }
pcrxml::TimestepRange::TimestepRange():Element()
 {
 }
const std::string& pcrxml::TimestepRange::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::TimestepRange::~TimestepRange()
{
 clean();
}
//! clean
void pcrxml::TimestepRange::clean()
{
}
//! copy ctor
pcrxml::TimestepRange::TimestepRange(const TimestepRange& src):
pcrxml::Element(src)
,first(src.first)
,last(src.last)
{
}
//! assignment operator
pcrxml::TimestepRange& pcrxml::TimestepRange::operator=(const TimestepRange& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::TimestepRange::fill(QDomElement el) const
{
 first.addToElement(el,"first");
 last.addToElement(el,"last");
}
