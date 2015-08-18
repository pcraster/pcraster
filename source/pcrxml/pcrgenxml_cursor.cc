/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_CURSOR
#include "pcrgenxml_cursor.h"
#define INCLUDED_PCRGENXML_CURSOR
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::Cursor::d_elementName("Cursor");
//! ctor
pcrxml::Cursor::Cursor(const QDomElement& element):Element(element,d_elementName)
 ,x(element,"x",true)
 ,y(element,"y",true)
 ,t(element,"t",true)
 {
 }
pcrxml::Cursor::Cursor():Element()
 {
 }
const std::string& pcrxml::Cursor::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Cursor::~Cursor()
{
 clean();
}
//! clean
void pcrxml::Cursor::clean()
{
}
//! copy ctor
pcrxml::Cursor::Cursor(const Cursor& src):
pcrxml::Element(src)
,x(src.x)
,y(src.y)
,t(src.t)
{
}
//! assignment operator
pcrxml::Cursor& pcrxml::Cursor::operator=(const Cursor& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Cursor::fill(QDomElement el) const
{
 x.addToElement(el,"x");
 y.addToElement(el,"y");
 t.addToElement(el,"t");
}
