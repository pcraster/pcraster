/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_FILEINPUT
#include "pcrgenxml_fileinput.h"
#define INCLUDED_PCRGENXML_FILEINPUT
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::FileInput::d_elementName("FileInput");
//! ctor
pcrxml::FileInput::FileInput(const QDomElement& element):Element(element,d_elementName)
 ,canChooseOtherFiles(element,"canChooseOtherFiles",true)
 ,map(0)
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
   if(v.currentChildEq("Stack"))
     stack = new Stack(v.processChild());
   // optional element
   if(v.currentChildEq("TimeSeries"))
     timeSeries = new TimeSeries(v.processChild());
   // optional element
   if(v.currentChildEq("Table"))
     table = new Table(v.processChild());
   // * repeated element
   while(v.currentChildEq("Data"))
     data.push_back(new Data(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::FileInput::FileInput():Element()
 ,map(0)
 ,stack(0)
 ,timeSeries(0)
 ,table(0)
 {
 }
const std::string& pcrxml::FileInput::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::FileInput::~FileInput()
{
 clean();
}
//! clean
void pcrxml::FileInput::clean()
{
 delete map;map=0;
 delete stack;stack=0;
 delete timeSeries;timeSeries=0;
 delete table;table=0;
 for(size_t i=0; i<data.size(); i++) delete data[i];
data.clear();
}
//! copy ctor
pcrxml::FileInput::FileInput(const FileInput& src):
pcrxml::Element(src)
,canChooseOtherFiles(src.canChooseOtherFiles)
{
 map= (src.map) ? new Map(*(src.map)): 0;
 stack= (src.stack) ? new Stack(*(src.stack)): 0;
 timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): 0;
 table= (src.table) ? new Table(*(src.table)): 0;
 for(size_t i=0; i<src.data.size(); i++) data.push_back(new Data(*(src.data[i])));
}
//! assignment operator
pcrxml::FileInput& pcrxml::FileInput::operator=(const FileInput& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  map= (src.map) ? new Map(*(src.map)): 0;
  stack= (src.stack) ? new Stack(*(src.stack)): 0;
  timeSeries= (src.timeSeries) ? new TimeSeries(*(src.timeSeries)): 0;
  table= (src.table) ? new Table(*(src.table)): 0;
  for(size_t i=0; i<src.data.size(); i++) data.push_back(new Data(*(src.data[i])));
 }
return *this;
}
void pcrxml::FileInput::fill(QDomElement el) const
{
 canChooseOtherFiles.addToElement(el,"canChooseOtherFiles");
 if (map) map->appendTo(el);
 if (stack) stack->appendTo(el);
 if (timeSeries) timeSeries->appendTo(el);
 if (table) table->appendTo(el);
 for(size_t i=0; i<data.size(); i++) data[i]->appendTo(el);
}
