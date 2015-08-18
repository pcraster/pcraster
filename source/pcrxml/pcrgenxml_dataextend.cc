/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATAEXTEND
#include "pcrgenxml_dataextend.h"
#define INCLUDED_PCRGENXML_DATAEXTEND
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::DataExtend::d_elementName("DataExtend");
//! ctor
pcrxml::DataExtend::DataExtend(const QDomElement& element):Element(element,d_elementName)
 ,migrationDirection(element,"migrationDirection",true)
 {
  try {
   ChildElementVisitor v(element);

   // + repeated element
   v.checkRequiredChild("Coefficient");
   while(v.currentChildEq("Coefficient"))
     coefficient.push_back(new Coefficient(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::DataExtend::DataExtend():Element()
 {
 }
const std::string& pcrxml::DataExtend::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::DataExtend::~DataExtend()
{
 clean();
}
//! clean
void pcrxml::DataExtend::clean()
{
 for(size_t i=0; i<coefficient.size(); i++) delete coefficient[i];
coefficient.clear();
}
//! copy ctor
pcrxml::DataExtend::DataExtend(const DataExtend& src):
pcrxml::Element(src)
,migrationDirection(src.migrationDirection)
{
 for(size_t i=0; i<src.coefficient.size(); i++) coefficient.push_back(new Coefficient(*(src.coefficient[i])));
}
//! assignment operator
pcrxml::DataExtend& pcrxml::DataExtend::operator=(const DataExtend& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.coefficient.size(); i++) coefficient.push_back(new Coefficient(*(src.coefficient[i])));
 }
return *this;
}
void pcrxml::DataExtend::fill(QDomElement el) const
{
 migrationDirection.addToElement(el,"migrationDirection");
 for(size_t i=0; i<coefficient.size(); i++) coefficient[i]->appendTo(el);
}
