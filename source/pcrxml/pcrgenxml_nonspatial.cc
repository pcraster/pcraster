/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_NONSPATIAL
#include "pcrgenxml_nonspatial.h"
#define INCLUDED_PCRGENXML_NONSPATIAL
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::NonSpatial::d_elementName("NonSpatial");
//! ctor
pcrxml::NonSpatial::NonSpatial(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",false)
 ,dataTypeDTD(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::NonSpatial::NonSpatial():Element()
 ,dataTypeDTD(0)
 {
 }
const std::string& pcrxml::NonSpatial::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::NonSpatial::~NonSpatial()
{
 clean();
}
//! clean
void pcrxml::NonSpatial::clean()
{
 delete dataTypeDTD;dataTypeDTD=0;
}
//! copy ctor
pcrxml::NonSpatial::NonSpatial(const NonSpatial& src):
pcrxml::Element(src)
,value(src.value)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
}
//! assignment operator
pcrxml::NonSpatial& pcrxml::NonSpatial::operator=(const NonSpatial& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 }
return *this;
}
void pcrxml::NonSpatial::fill(QDomElement el) const
{
 value.addToElement(el,"value");
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
}
