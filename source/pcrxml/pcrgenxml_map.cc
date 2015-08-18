/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_MAP
#include "pcrgenxml_map.h"
#define INCLUDED_PCRGENXML_MAP
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Map::d_elementName("Map");
//! ctor
pcrxml::Map::Map(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Map::Map():Element()
 ,dataTypeDTD(0)
 {
 }
const std::string& pcrxml::Map::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Map::~Map()
{
 clean();
}
//! clean
void pcrxml::Map::clean()
{
 delete dataTypeDTD;dataTypeDTD=0;
}
//! copy ctor
pcrxml::Map::Map(const Map& src):
pcrxml::Element(src)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
}
//! assignment operator
pcrxml::Map& pcrxml::Map::operator=(const Map& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 }
return *this;
}
void pcrxml::Map::fill(QDomElement el) const
{
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
}
