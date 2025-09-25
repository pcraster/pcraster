/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_parameteritem.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::ParameterItem::d_elementName("ParameterItem");
//! ctor
pcrxml::ParameterItem::ParameterItem(const QDomElement& element):Element(element,d_elementName)
 ,label(element,"label",true)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("ScriptLink");
   scriptLink = new ScriptLink(v.processChild());
   // optional element
   if(v.currentChildEq("NumericInput"))
     numericInput = new NumericInput(v.processChild());
   // optional element
   if(v.currentChildEq("FileInput"))
     fileInput = new FileInput(v.processChild());
   // optional element
   if(v.currentChildEq("ShowData"))
     showData = new ShowData(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::ParameterItem::ParameterItem():Element()
 
 {
 }
const std::string& pcrxml::ParameterItem::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ParameterItem::~ParameterItem()
{
 clean();
}
//! clean
void pcrxml::ParameterItem::clean()
{
 delete scriptLink;scriptLink=nullptr;
 delete numericInput;numericInput=nullptr;
 delete fileInput;fileInput=nullptr;
 delete showData;showData=nullptr;
}
//! copy ctor
pcrxml::ParameterItem::ParameterItem(const ParameterItem& src):
pcrxml::Element(src)
,label(src.label)
{
 scriptLink=new ScriptLink(*(src.scriptLink));
 numericInput= (src.numericInput) ? new NumericInput(*(src.numericInput)): nullptr;
 fileInput= (src.fileInput) ? new FileInput(*(src.fileInput)): nullptr;
 showData= (src.showData) ? new ShowData(*(src.showData)): nullptr;
}
//! assignment operator
pcrxml::ParameterItem& pcrxml::ParameterItem::operator=(const ParameterItem& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  scriptLink=new ScriptLink(*(src.scriptLink));
  numericInput= (src.numericInput) ? new NumericInput(*(src.numericInput)): nullptr;
  fileInput= (src.fileInput) ? new FileInput(*(src.fileInput)): nullptr;
  showData= (src.showData) ? new ShowData(*(src.showData)): nullptr;
 }
return *this;
}
void pcrxml::ParameterItem::fill(QDomElement el) const
{
 label.addToElement(el,"label");
 if (scriptLink) scriptLink->appendTo(el);
 if (numericInput) numericInput->appendTo(el);
 if (fileInput) fileInput->appendTo(el);
 if (showData) showData->appendTo(el);
}
