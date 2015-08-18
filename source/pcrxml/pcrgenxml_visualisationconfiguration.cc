/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#include "pcrgenxml_visualisationconfiguration.h"
#define INCLUDED_PCRGENXML_VISUALISATIONCONFIGURATION
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::VisualisationConfiguration::d_elementName("VisualisationConfiguration");
//! ctor
pcrxml::VisualisationConfiguration::VisualisationConfiguration(const QDomElement& element):Element(element,d_elementName)
 ,date(element,"date",true)
 ,version(element,"version",true)
 ,cwd(element,"cwd",true)
 ,os(element,"os",true)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("VisualisationGroup"))
     visualisationGroup.push_back(new VisualisationGroup(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::VisualisationConfiguration::VisualisationConfiguration():Element()
 {
 }
const std::string& pcrxml::VisualisationConfiguration::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::VisualisationConfiguration::~VisualisationConfiguration()
{
 clean();
}
//! clean
void pcrxml::VisualisationConfiguration::clean()
{
 for(size_t i=0; i<visualisationGroup.size(); i++) delete visualisationGroup[i];
visualisationGroup.clear();
}
//! copy ctor
pcrxml::VisualisationConfiguration::VisualisationConfiguration(const VisualisationConfiguration& src):
pcrxml::Element(src)
,date(src.date)
,version(src.version)
,cwd(src.cwd)
,os(src.os)
{
 for(size_t i=0; i<src.visualisationGroup.size(); i++) visualisationGroup.push_back(new VisualisationGroup(*(src.visualisationGroup[i])));
}
//! assignment operator
pcrxml::VisualisationConfiguration& pcrxml::VisualisationConfiguration::operator=(const VisualisationConfiguration& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.visualisationGroup.size(); i++) visualisationGroup.push_back(new VisualisationGroup(*(src.visualisationGroup[i])));
 }
return *this;
}
void pcrxml::VisualisationConfiguration::fill(QDomElement el) const
{
 date.addToElement(el,"date");
 version.addToElement(el,"version");
 cwd.addToElement(el,"cwd");
 os.addToElement(el,"os");
 for(size_t i=0; i<visualisationGroup.size(); i++) visualisationGroup[i]->appendTo(el);
}
