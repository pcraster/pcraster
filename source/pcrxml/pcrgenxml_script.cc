/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_SCRIPT
#include "pcrgenxml_script.h"
#define INCLUDED_PCRGENXML_SCRIPT
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Script::d_elementName("Script");
//! ctor
pcrxml::Script::Script(const QDomElement& element):Element(element,d_elementName)
 ,scriptFileName(element,"scriptFileName",false)
 ,scriptType(element,"scriptType",true)
 ,ioStrategy(element,"ioStrategy",false)
 ,integerTimer(0)
 ,scriptData(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("integerTimer"))
     integerTimer = new IntegerTimer(v.processChild());
   // required element
   v.checkRequiredChild("ScriptData");
   scriptData = new ScriptData(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Script::Script():Element()
 ,integerTimer(0)
 ,scriptData(0)
 {
 }
const std::string& pcrxml::Script::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Script::~Script()
{
 clean();
}
//! clean
void pcrxml::Script::clean()
{
 delete integerTimer;integerTimer=0;
 delete scriptData;scriptData=0;
}
//! copy ctor
pcrxml::Script::Script(const Script& src):
pcrxml::Element(src)
,scriptFileName(src.scriptFileName)
,scriptType(src.scriptType)
,ioStrategy(src.ioStrategy)
{
 integerTimer= (src.integerTimer) ? new IntegerTimer(*(src.integerTimer)): 0;
 scriptData=new ScriptData(*(src.scriptData));
}
//! assignment operator
pcrxml::Script& pcrxml::Script::operator=(const Script& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  integerTimer= (src.integerTimer) ? new IntegerTimer(*(src.integerTimer)): 0;
  scriptData=new ScriptData(*(src.scriptData));
 }
return *this;
}
void pcrxml::Script::fill(QDomElement el) const
{
 scriptFileName.addToElement(el,"scriptFileName");
 scriptType.addToElement(el,"scriptType");
 ioStrategy.addToElement(el,"ioStrategy");
 if (integerTimer) integerTimer->appendTo(el);
 if (scriptData) scriptData->appendTo(el);
}
