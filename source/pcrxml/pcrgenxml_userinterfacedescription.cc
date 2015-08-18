/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION
#include "pcrgenxml_userinterfacedescription.h"
#define INCLUDED_PCRGENXML_USERINTERFACEDESCRIPTION
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::UserInterfaceDescription::d_elementName("UserInterfaceDescription");
//! ctor
pcrxml::UserInterfaceDescription::UserInterfaceDescription(const QDomElement& element):Element(element,d_elementName)
 ,scriptFile(element,"scriptFile",true)
 ,panelType(element,"panelType",true)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("ParameterPanel"))
     parameterPanel.push_back(new ParameterPanel(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::UserInterfaceDescription::UserInterfaceDescription():Element()
 {
 }
const std::string& pcrxml::UserInterfaceDescription::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::UserInterfaceDescription::~UserInterfaceDescription()
{
 clean();
}
//! clean
void pcrxml::UserInterfaceDescription::clean()
{
 for(size_t i=0; i<parameterPanel.size(); i++) delete parameterPanel[i];
parameterPanel.clear();
}
//! copy ctor
pcrxml::UserInterfaceDescription::UserInterfaceDescription(const UserInterfaceDescription& src):
pcrxml::Element(src)
,scriptFile(src.scriptFile)
,panelType(src.panelType)
{
 for(size_t i=0; i<src.parameterPanel.size(); i++) parameterPanel.push_back(new ParameterPanel(*(src.parameterPanel[i])));
}
//! assignment operator
pcrxml::UserInterfaceDescription& pcrxml::UserInterfaceDescription::operator=(const UserInterfaceDescription& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.parameterPanel.size(); i++) parameterPanel.push_back(new ParameterPanel(*(src.parameterPanel[i])));
 }
return *this;
}
void pcrxml::UserInterfaceDescription::fill(QDomElement el) const
{
 scriptFile.addToElement(el,"scriptFile");
 panelType.addToElement(el,"panelType");
 for(size_t i=0; i<parameterPanel.size(); i++) parameterPanel[i]->appendTo(el);
}
