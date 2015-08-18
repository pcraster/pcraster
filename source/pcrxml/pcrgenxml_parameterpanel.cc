/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_PARAMETERPANEL
#include "pcrgenxml_parameterpanel.h"
#define INCLUDED_PCRGENXML_PARAMETERPANEL
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::ParameterPanel::d_elementName("ParameterPanel");
//! ctor
pcrxml::ParameterPanel::ParameterPanel(const QDomElement& element):Element(element,d_elementName)
 ,name(element,"name",true)
 ,show(element,"show",false)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("ParameterItem"))
     parameterItem.push_back(new ParameterItem(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::ParameterPanel::ParameterPanel():Element()
 {
 }
const std::string& pcrxml::ParameterPanel::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ParameterPanel::~ParameterPanel()
{
 clean();
}
//! clean
void pcrxml::ParameterPanel::clean()
{
 for(size_t i=0; i<parameterItem.size(); i++) delete parameterItem[i];
parameterItem.clear();
}
//! copy ctor
pcrxml::ParameterPanel::ParameterPanel(const ParameterPanel& src):
pcrxml::Element(src)
,name(src.name)
,show(src.show)
{
 for(size_t i=0; i<src.parameterItem.size(); i++) parameterItem.push_back(new ParameterItem(*(src.parameterItem[i])));
}
//! assignment operator
pcrxml::ParameterPanel& pcrxml::ParameterPanel::operator=(const ParameterPanel& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.parameterItem.size(); i++) parameterItem.push_back(new ParameterItem(*(src.parameterItem[i])));
 }
return *this;
}
void pcrxml::ParameterPanel::fill(QDomElement el) const
{
 name.addToElement(el,"name");
 show.addToElement(el,"show");
 for(size_t i=0; i<parameterItem.size(); i++) parameterItem[i]->appendTo(el);
}
