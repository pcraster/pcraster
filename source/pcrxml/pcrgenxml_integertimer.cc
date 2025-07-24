/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_integertimer.h"
#include <qdom.h>



const std::string pcrxml::IntegerTimer::d_elementName("integerTimer");
//! ctor
pcrxml::IntegerTimer::IntegerTimer(const QDomElement& element):Element(element,d_elementName)
 ,start(element,"start",true)
 ,end(element,"end",true)
 ,step(element,"step",true)
 {
 }
pcrxml::IntegerTimer::IntegerTimer():Element()
 {
 }
const std::string& pcrxml::IntegerTimer::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::IntegerTimer::~IntegerTimer()
{
 clean();
}
//! clean
void pcrxml::IntegerTimer::clean()
{
}
//! copy ctor
pcrxml::IntegerTimer::IntegerTimer(const IntegerTimer& src):
pcrxml::Element(src)
,start(src.start)
,end(src.end)
,step(src.step)
{
}
//! assignment operator
pcrxml::IntegerTimer& pcrxml::IntegerTimer::operator=(const IntegerTimer& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::IntegerTimer::fill(QDomElement el) const
{
 start.addToElement(el,"start");
 end.addToElement(el,"end");
 step.addToElement(el,"step");
}
