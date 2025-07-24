/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_scriptdata.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::ScriptData::d_elementName("ScriptData");
//! ctor
pcrxml::ScriptData::ScriptData(const QDomElement& element):Element(element,d_elementName)
 {
  try {
   ChildElementVisitor v(element);

   // * repeated element
   while(v.currentChildEq("Data"))
     data.push_back(new Data(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::ScriptData::ScriptData():Element()
 {
 }
const std::string& pcrxml::ScriptData::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ScriptData::~ScriptData()
{
 clean();
}
//! clean
void pcrxml::ScriptData::clean()
{
 for(auto & i : data) delete i;
data.clear();
}
//! copy ctor
pcrxml::ScriptData::ScriptData(const ScriptData& src):
pcrxml::Element(src)
{
 for(auto i : src.data) data.push_back(new Data(*i));
}
//! assignment operator
pcrxml::ScriptData& pcrxml::ScriptData::operator=(const ScriptData& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(auto i : src.data) data.push_back(new Data(*i));
 }
return *this;
}
void pcrxml::ScriptData::fill(QDomElement el) const
{
 for(auto i : data) i->appendTo(el);
}
