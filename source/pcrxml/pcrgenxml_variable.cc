/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_VARIABLE
#include "pcrgenxml_variable.h"
#define INCLUDED_PCRGENXML_VARIABLE
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Variable::d_elementName("variable");
//! ctor
pcrxml::Variable::Variable(const QDomElement& element):Element(element,d_elementName)
 ,id(element,"id",true)
 ,description(element,"description",false)
 ,spatial(element,"spatial",false)
 ,input(element,"input",false)
 ,output(element,"output",false)
 {
  try {
   ChildElementVisitor v(element);

   // + repeated element
   v.checkRequiredChild("dataTypeDTD");
   while(v.currentChildEq("dataTypeDTD"))
     dataTypeDTD.push_back(new DataTypeDTD(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::Variable::Variable():Element()
 {
 }
const std::string& pcrxml::Variable::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Variable::~Variable()
{
 clean();
}
//! clean
void pcrxml::Variable::clean()
{
 for(size_t i=0; i<dataTypeDTD.size(); i++) delete dataTypeDTD[i];
dataTypeDTD.clear();
}
//! copy ctor
pcrxml::Variable::Variable(const Variable& src):
pcrxml::Element(src)
,id(src.id)
,description(src.description)
,spatial(src.spatial)
,input(src.input)
,output(src.output)
{
 for(size_t i=0; i<src.dataTypeDTD.size(); i++) dataTypeDTD.push_back(new DataTypeDTD(*(src.dataTypeDTD[i])));
}
//! assignment operator
pcrxml::Variable& pcrxml::Variable::operator=(const Variable& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.dataTypeDTD.size(); i++) dataTypeDTD.push_back(new DataTypeDTD(*(src.dataTypeDTD[i])));
 }
return *this;
}
void pcrxml::Variable::fill(QDomElement el) const
{
 id.addToElement(el,"id");
 description.addToElement(el,"description");
 spatial.addToElement(el,"spatial");
 input.addToElement(el,"input");
 output.addToElement(el,"output");
 for(size_t i=0; i<dataTypeDTD.size(); i++) dataTypeDTD[i]->appendTo(el);
}
