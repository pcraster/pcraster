/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_SCRIPTDATA
#include "pcrgenxml_scriptdata.h"
#define INCLUDED_PCRGENXML_SCRIPTDATA
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

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
