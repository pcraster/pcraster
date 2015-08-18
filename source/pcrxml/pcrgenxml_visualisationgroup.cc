/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_VISUALISATIONGROUP
#include "pcrgenxml_visualisationgroup.h"
#define INCLUDED_PCRGENXML_VISUALISATIONGROUP
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::VisualisationGroup::d_elementName("VisualisationGroup");
//! ctor
pcrxml::VisualisationGroup::VisualisationGroup(const QDomElement& element):Element(element,d_elementName)
 ,dataObject(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("DataObject");
   dataObject = new DataObject(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::VisualisationGroup::VisualisationGroup():Element()
 ,dataObject(0)
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
 delete dataObject;dataObject=0;
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
