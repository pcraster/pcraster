/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_map.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Map::d_elementName("Map");
//! ctor
pcrxml::Map::Map(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(nullptr)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Map::Map():Element()
 
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
 delete dataTypeDTD;dataTypeDTD=nullptr;
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
