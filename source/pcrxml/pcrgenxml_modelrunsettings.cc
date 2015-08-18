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
 for(size_t i=0; i<numericSetting.size(); i++) delete numericSetting[i];
numericSetting.clear();
 for(size_t i=0; i<fileSetting.size(); i++) delete fileSetting[i];
fileSetting.clear();
 for(size_t i=0; i<binding.size(); i++) delete binding[i];
binding.clear();
 for(size_t i=0; i<modelRunChild.size(); i++) delete modelRunChild[i];
modelRunChild.clear();
}
//! copy ctor
pcrxml::ModelRunSettings::ModelRunSettings(const ModelRunSettings& src):
pcrxml::Element(src)
{
 for(size_t i=0; i<src.numericSetting.size(); i++) numericSetting.push_back(new NumericSetting(*(src.numericSetting[i])));
 for(size_t i=0; i<src.fileSetting.size(); i++) fileSetting.push_back(new FileSetting(*(src.fileSetting[i])));
 for(size_t i=0; i<src.binding.size(); i++) binding.push_back(new Binding(*(src.binding[i])));
 for(size_t i=0; i<src.modelRunChild.size(); i++) modelRunChild.push_back(new ModelRunChild(*(src.modelRunChild[i])));
}
//! assignment operator
pcrxml::ModelRunSettings& pcrxml::ModelRunSettings::operator=(const ModelRunSettings& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.numericSetting.size(); i++) numericSetting.push_back(new NumericSetting(*(src.numericSetting[i])));
  for(size_t i=0; i<src.fileSetting.size(); i++) fileSetting.push_back(new FileSetting(*(src.fileSetting[i])));
  for(size_t i=0; i<src.binding.size(); i++) binding.push_back(new Binding(*(src.binding[i])));
  for(size_t i=0; i<src.modelRunChild.size(); i++) modelRunChild.push_back(new ModelRunChild(*(src.modelRunChild[i])));
 }
return *this;
}
void pcrxml::ModelRunSettings::fill(QDomElement el) const
{
 for(size_t i=0; i<numericSetting.size(); i++) numericSetting[i]->appendTo(el);
 for(size_t i=0; i<fileSetting.size(); i++) fileSetting[i]->appendTo(el);
 for(size_t i=0; i<binding.size(); i++) binding[i]->appendTo(el);
 for(size_t i=0; i<modelRunChild.size(); i++) modelRunChild[i]->appendTo(el);
}
