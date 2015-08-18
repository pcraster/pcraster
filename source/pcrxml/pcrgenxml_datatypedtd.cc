/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATATYPEDTD
#include "pcrgenxml_datatypedtd.h"
#define INCLUDED_PCRGENXML_DATATYPEDTD
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::DataTypeDTD::d_elementName("dataTypeDTD");
//! ctor
pcrxml::DataTypeDTD::DataTypeDTD(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("dimension"))
     dimension.push_back(new Dimension(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::DataTypeDTD::DataTypeDTD():Element()
 {
 }
const std::string& pcrxml::DataTypeDTD::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::DataTypeDTD::~DataTypeDTD()
{
 clean();
}
//! clean
void pcrxml::DataTypeDTD::clean()
{
 for(size_t i=0; i<dimension.size(); i++) delete dimension[i];
dimension.clear();
}
//! copy ctor
pcrxml::DataTypeDTD::DataTypeDTD(const DataTypeDTD& src):
pcrxml::Element(src)
,value(src.value)
{
 for(size_t i=0; i<src.dimension.size(); i++) dimension.push_back(new Dimension(*(src.dimension[i])));
}
//! assignment operator
pcrxml::DataTypeDTD& pcrxml::DataTypeDTD::operator=(const DataTypeDTD& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.dimension.size(); i++) dimension.push_back(new Dimension(*(src.dimension[i])));
 }
return *this;
}
void pcrxml::DataTypeDTD::fill(QDomElement el) const
{
 value.addToElement(el,"value");
 for(size_t i=0; i<dimension.size(); i++) dimension[i]->appendTo(el);
}
