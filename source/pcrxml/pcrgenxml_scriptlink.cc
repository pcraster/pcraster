/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_SCRIPTLINK
#include "pcrgenxml_scriptlink.h"
#define INCLUDED_PCRGENXML_SCRIPTLINK
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::ScriptLink::d_elementName("ScriptLink");
//! ctor
pcrxml::ScriptLink::ScriptLink(const QDomElement& element):Element(element,d_elementName)
 {
  try {
   ChildElementVisitor v(element);

   // + repeated element
   v.checkRequiredChild("Data");
   while(v.currentChildEq("Data"))
     data.push_back(new Data(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::ScriptLink::ScriptLink():Element()
 {
 }
const std::string& pcrxml::ScriptLink::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ScriptLink::~ScriptLink()
{
 clean();
}
//! clean
void pcrxml::ScriptLink::clean()
{
 for(size_t i=0; i<data.size(); i++) delete data[i];
data.clear();
}
//! copy ctor
pcrxml::ScriptLink::ScriptLink(const ScriptLink& src):
pcrxml::Element(src)
{
 for(size_t i=0; i<src.data.size(); i++) data.push_back(new Data(*(src.data[i])));
}
//! assignment operator
pcrxml::ScriptLink& pcrxml::ScriptLink::operator=(const ScriptLink& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  for(size_t i=0; i<src.data.size(); i++) data.push_back(new Data(*(src.data[i])));
 }
return *this;
}
void pcrxml::ScriptLink::fill(QDomElement el) const
{
 for(size_t i=0; i<data.size(); i++) data[i]->appendTo(el);
}
