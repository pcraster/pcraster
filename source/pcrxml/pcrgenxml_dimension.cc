/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DIMENSION
#include "pcrgenxml_dimension.h"
#define INCLUDED_PCRGENXML_DIMENSION
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::Dimension::d_elementName("dimension");
//! ctor
pcrxml::Dimension::Dimension(const QDomElement& element):Element(element,d_elementName)
 ,base(element,"base",true)
 ,power(element,"power",true)
 {
 }
pcrxml::Dimension::Dimension():Element()
 {
 }
const std::string& pcrxml::Dimension::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Dimension::~Dimension()
{
 clean();
}
//! clean
void pcrxml::Dimension::clean()
{
}
//! copy ctor
pcrxml::Dimension::Dimension(const Dimension& src):
pcrxml::Element(src)
,base(src.base)
,power(src.power)
{
}
//! assignment operator
pcrxml::Dimension& pcrxml::Dimension::operator=(const Dimension& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Dimension::fill(QDomElement el) const
{
 base.addToElement(el,"base");
 power.addToElement(el,"power");
}
