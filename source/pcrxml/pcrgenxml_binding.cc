/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_BINDING
#include "pcrgenxml_binding.h"
#define INCLUDED_PCRGENXML_BINDING
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Binding::d_elementName("Binding");
//! ctor
pcrxml::Binding::Binding(const QDomElement& element):Element(element,d_elementName)
 ,parameter(element,"parameter",true)
 ,value(element,"value",true)
 ,map(0)
 ,nonSpatial(0)
 ,stack(0)
 ,timeSeries(0)
 ,table(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("Map"))
     map = new Map(v.processChild());
   // optional element
   if(v.currentChildEq("NonSpatial"))
     nonSpatial = new NonSpatial(v.processChild());
   // optional element
   if(v.currentChildEq("Stack"))
     stack = new Stack(v.processChild());
   // optional element
   if(v.currentChildEq("TimeSeries"))
     timeSeries = new TimeSeries(v.processChild());
   // optional element
   if(v.currentChildEq("Table"))
     table = new Table(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Binding::Binding():Element()
 ,map(0)
 ,nonSpatial(0)
 ,stack(0)
 ,timeSeries(0)
 ,table(0)
 {
 }
const std::string& pcrxml::Binding::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Binding::~Binding()
{
 clean();
}
//! clean
void pcrxml::Binding::clean()
{
 delete map;map=0;
 delete nonSpatial;nonSpatial=0;
 delete stack;stack=0;
 delete timeSeries;timeSeries=0;
 delete table;table=0;
}
//! copy ctor
pcrxml::Binding::Binding(const Binding& src):
pcrxml::Element(src)
,parameter(src.parameter)
,value(src.value)
{
 map= (src.map) ? new Map(*(src.map)): 0;
 nonSpatial= (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)): 0;
 stack= (src.stack) ? new Stack(*(src.stack)): 0;
 timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): 0;
 table= (src.table) ? new Table(*(src.table)): 0;
}
//! assignment operator
pcrxml::Binding& pcrxml::Binding::operator=(const Binding& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  map= (src.map) ? new Map(*(src.map)): 0;
  nonSpatial= (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)): 0;
  stack= (src.stack) ? new Stack(*(src.stack)): 0;
  timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): 0;
  table= (src.table) ? new Table(*(src.table)): 0;
 }
return *this;
}
void pcrxml::Binding::fill(QDomElement el) const
{
 parameter.addToElement(el,"parameter");
 value.addToElement(el,"value");
 if (map) map->appendTo(el);
 if (nonSpatial) nonSpatial->appendTo(el);
 if (stack) stack->appendTo(el);
 if (timeSeries) timeSeries->appendTo(el);
 if (table) table->appendTo(el);
}
