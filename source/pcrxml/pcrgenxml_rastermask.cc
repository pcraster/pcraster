/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_RASTERMASK
#include "pcrgenxml_rastermask.h"
#define INCLUDED_PCRGENXML_RASTERMASK
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::RasterMask::d_elementName("rasterMask");
//! ctor
pcrxml::RasterMask::RasterMask(const QDomElement& element):Element(element,d_elementName)
 ,mask(element,"mask",true)
 {
 }
pcrxml::RasterMask::RasterMask():Element()
 {
 }
const std::string& pcrxml::RasterMask::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::RasterMask::~RasterMask()
{
 clean();
}
//! clean
void pcrxml::RasterMask::clean()
{
}
//! copy ctor
pcrxml::RasterMask::RasterMask(const RasterMask& src):
pcrxml::Element(src)
,mask(src.mask)
{
}
//! assignment operator
pcrxml::RasterMask& pcrxml::RasterMask::operator=(const RasterMask& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::RasterMask::fill(QDomElement el) const
{
 mask.addToElement(el,"mask");
}
