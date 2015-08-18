/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_STACK
#include "pcrgenxml_stack.h"
#define INCLUDED_PCRGENXML_STACK
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Stack::d_elementName("Stack");
//! ctor
pcrxml::Stack::Stack(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(0)
 ,timestepRange(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
   // optional element
   if(v.currentChildEq("TimestepRange"))
     timestepRange = new TimestepRange(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Stack::Stack():Element()
 ,dataTypeDTD(0)
 ,timestepRange(0)
 {
 }
const std::string& pcrxml::Stack::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Stack::~Stack()
{
 clean();
}
//! clean
void pcrxml::Stack::clean()
{
 delete dataTypeDTD;dataTypeDTD=0;
 delete timestepRange;timestepRange=0;
}
//! copy ctor
pcrxml::Stack::Stack(const Stack& src):
pcrxml::Element(src)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 timestepRange= (src.timestepRange) ? new TimestepRange(*(src.timestepRange)): 0;
}
//! assignment operator
pcrxml::Stack& pcrxml::Stack::operator=(const Stack& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
  timestepRange= (src.timestepRange) ? new TimestepRange(*(src.timestepRange)): 0;
 }
return *this;
}
void pcrxml::Stack::fill(QDomElement el) const
{
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
 if (timestepRange) timestepRange->appendTo(el);
}
