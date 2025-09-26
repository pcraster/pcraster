/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_areamapdtd.h"
#include "pcrxml_childelementvisitor.h"
#include <QtXml>

const std::string pcrxml::AreaMapDTD::d_elementName("areaMapDTD");
//! ctor
pcrxml::AreaMapDTD::AreaMapDTD(const QDomElement& element):Element(element,d_elementName)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("rasterSpace"))
     rasterSpace = new RasterSpace(v.processChild());
   // optional element
   if(v.currentChildEq("rasterMask"))
     rasterMask = new RasterMask(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::AreaMapDTD::AreaMapDTD():Element()
 
 {
 }
const std::string& pcrxml::AreaMapDTD::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::AreaMapDTD::~AreaMapDTD()
{
 clean();
}
//! clean
void pcrxml::AreaMapDTD::clean()
{
 delete rasterSpace;rasterSpace=nullptr;
 delete rasterMask;rasterMask=nullptr;
}
//! copy ctor
pcrxml::AreaMapDTD::AreaMapDTD(const AreaMapDTD& src):
pcrxml::Element(src)
{
 rasterSpace= (src.rasterSpace) ? new RasterSpace(*(src.rasterSpace)): nullptr;
 rasterMask= (src.rasterMask) ? new RasterMask(*(src.rasterMask)): nullptr;
}
//! assignment operator
pcrxml::AreaMapDTD& pcrxml::AreaMapDTD::operator=(const AreaMapDTD& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  rasterSpace= (src.rasterSpace) ? new RasterSpace(*(src.rasterSpace)): nullptr;
  rasterMask= (src.rasterMask) ? new RasterMask(*(src.rasterMask)): nullptr;
 }
return *this;
}
void pcrxml::AreaMapDTD::fill(QDomElement el) const
{
 if (rasterSpace) rasterSpace->appendTo(el);
 if (rasterMask) rasterMask->appendTo(el);
}
