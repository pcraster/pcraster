/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_visualisationconfiguration.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

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
 for(auto & i : visualisationGroup) delete i;
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
 for(auto i : src.visualisationGroup) visualisationGroup.push_back(new VisualisationGroup(*i));
}
//! assignment operator
pcrxml::VisualisationConfiguration& pcrxml::VisualisationConfiguration::operator=(const VisualisationConfiguration& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(auto i : src.visualisationGroup) visualisationGroup.push_back(new VisualisationGroup(*i));
 }
return *this;
}
void pcrxml::VisualisationConfiguration::fill(QDomElement el) const
{
 date.addToElement(el,"date");
 version.addToElement(el,"version");
 cwd.addToElement(el,"cwd");
 os.addToElement(el,"os");
 for(auto i : visualisationGroup) i->appendTo(el);
}
