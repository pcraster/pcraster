/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_TIMER
#include "pcrgenxml_timer.h"
#define INCLUDED_PCRGENXML_TIMER
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::Timer::d_elementName("Timer");
//! ctor
pcrxml::Timer::Timer(const QDomElement& element):Element(element,d_elementName)
 {
 }
pcrxml::Timer::Timer():Element()
 {
 }
const std::string& pcrxml::Timer::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Timer::~Timer()
{
 clean();
}
//! clean
void pcrxml::Timer::clean()
{
}
//! copy ctor
pcrxml::Timer::Timer(const Timer& src):
pcrxml::Element(src)
{
}
//! assignment operator
pcrxml::Timer& pcrxml::Timer::operator=(const Timer& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Timer::fill(QDomElement /* el */) const
{
}
