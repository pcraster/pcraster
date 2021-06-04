/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATA
#include "pcrgenxml_data.h"
#define INCLUDED_PCRGENXML_DATA
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Data::d_elementName("Data");
//! ctor
pcrxml::Data::Data(const QDomElement& element):Element(element,d_elementName)
 ,name(element,"name",true)
 ,description(element,"description",false)
 ,externalFileName(element,"externalFileName",false)
 ,ioType(element,"ioType",true)
 ,map(nullptr)
 ,nonSpatial(nullptr)
 ,stack(nullptr)
 ,timeSeries(nullptr)
 ,table(nullptr)
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
pcrxml::Data::Data():Element()
 ,map(nullptr)
 ,nonSpatial(nullptr)
 ,stack(nullptr)
 ,timeSeries(nullptr)
 ,table(nullptr)
 {
 }
const std::string& pcrxml::Data::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Data::~Data()
{
 clean();
}
//! clean
void pcrxml::Data::clean()
{
 delete map;map=nullptr;
 delete nonSpatial;nonSpatial=nullptr;
 delete stack;stack=nullptr;
 delete timeSeries;timeSeries=nullptr;
 delete table;table=nullptr;
}
//! copy ctor
pcrxml::Data::Data(const Data& src):
pcrxml::Element(src)
,name(src.name)
,description(src.description)
,externalFileName(src.externalFileName)
,ioType(src.ioType)
{
 map= (src.map) ? new Map(*(src.map)): nullptr;
 nonSpatial= (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)): nullptr;
 stack= (src.stack) ? new Stack(*(src.stack)): nullptr;
 timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): nullptr;
 table= (src.table) ? new Table(*(src.table)): nullptr;
}
//! assignment operator
pcrxml::Data& pcrxml::Data::operator=(const Data& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  map= (src.map) ? new Map(*(src.map)): nullptr;
  nonSpatial= (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)): nullptr;
  stack= (src.stack) ? new Stack(*(src.stack)): nullptr;
  timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): nullptr;
  table= (src.table) ? new Table(*(src.table)): nullptr;
 }
return *this;
}
void pcrxml::Data::fill(QDomElement el) const
{
 name.addToElement(el,"name");
 description.addToElement(el,"description");
 externalFileName.addToElement(el,"externalFileName");
 ioType.addToElement(el,"ioType");
 if (map) map->appendTo(el);
 if (nonSpatial) nonSpatial->appendTo(el);
 if (stack) stack->appendTo(el);
 if (timeSeries) timeSeries->appendTo(el);
 if (table) table->appendTo(el);
}
