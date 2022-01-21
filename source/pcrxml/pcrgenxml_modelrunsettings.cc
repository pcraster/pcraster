/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_MODELRUNSETTINGS
#include "pcrgenxml_modelrunsettings.h"
#define INCLUDED_PCRGENXML_MODELRUNSETTINGS
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::ModelRunSettings::d_elementName("ModelRunSettings");
//! ctor
pcrxml::ModelRunSettings::ModelRunSettings(const QDomElement& element):Element(element,d_elementName)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("NumericSetting"))
     numericSetting.push_back(new NumericSetting(v.processChild()));
   // * repeated element
   while(v.currentChildEq("FileSetting"))
     fileSetting.push_back(new FileSetting(v.processChild()));
   // * repeated element
   while(v.currentChildEq("Binding"))
     binding.push_back(new Binding(v.processChild()));
   // * repeated element
   while(v.currentChildEq("ModelRunChild"))
     modelRunChild.push_back(new ModelRunChild(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::ModelRunSettings::ModelRunSettings():Element()
 {
 }
const std::string& pcrxml::ModelRunSettings::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ModelRunSettings::~ModelRunSettings()
{
 clean();
}
//! clean
void pcrxml::ModelRunSettings::clean()
{
 for(auto & i : numericSetting) delete i;
numericSetting.clear();
 for(auto & i : fileSetting) delete i;
fileSetting.clear();
 for(auto & i : binding) delete i;
binding.clear();
 for(auto & i : modelRunChild) delete i;
modelRunChild.clear();
}
//! copy ctor
pcrxml::ModelRunSettings::ModelRunSettings(const ModelRunSettings& src):
pcrxml::Element(src)
{
 for(auto i : src.numericSetting) numericSetting.push_back(new NumericSetting(*i));
 for(auto i : src.fileSetting) fileSetting.push_back(new FileSetting(*i));
 for(auto i : src.binding) binding.push_back(new Binding(*i));
 for(auto i : src.modelRunChild) modelRunChild.push_back(new ModelRunChild(*i));
}
//! assignment operator
pcrxml::ModelRunSettings& pcrxml::ModelRunSettings::operator=(const ModelRunSettings& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(auto i : src.numericSetting) numericSetting.push_back(new NumericSetting(*i));
  for(auto i : src.fileSetting) fileSetting.push_back(new FileSetting(*i));
  for(auto i : src.binding) binding.push_back(new Binding(*i));
  for(auto i : src.modelRunChild) modelRunChild.push_back(new ModelRunChild(*i));
 }
return *this;
}
void pcrxml::ModelRunSettings::fill(QDomElement el) const
{
 for(auto i : numericSetting) i->appendTo(el);
 for(auto i : fileSetting) i->appendTo(el);
 for(auto i : binding) i->appendTo(el);
 for(auto i : modelRunChild) i->appendTo(el);
}
