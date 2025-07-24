/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_visualisationgroup.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::VisualisationGroup::d_elementName("VisualisationGroup");
//! ctor
pcrxml::VisualisationGroup::VisualisationGroup(const QDomElement& element):Element(element,d_elementName)
 ,dataObject(nullptr)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("DataObject");
   dataObject = new DataObject(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::VisualisationGroup::VisualisationGroup():Element()
 
 {
 }
const std::string& pcrxml::VisualisationGroup::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::VisualisationGroup::~VisualisationGroup()
{
 clean();
}
//! clean
void pcrxml::VisualisationGroup::clean()
{
 delete dataObject;dataObject=nullptr;
}
//! copy ctor
pcrxml::VisualisationGroup::VisualisationGroup(const VisualisationGroup& src):
pcrxml::Element(src)
{
 dataObject=new DataObject(*(src.dataObject));
}
//! assignment operator
pcrxml::VisualisationGroup& pcrxml::VisualisationGroup::operator=(const VisualisationGroup& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataObject=new DataObject(*(src.dataObject));
 }
return *this;
}
void pcrxml::VisualisationGroup::fill(QDomElement el) const
{
 if (dataObject) dataObject->appendTo(el);
}
