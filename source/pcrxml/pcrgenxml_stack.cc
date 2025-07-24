/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_stack.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Stack::d_elementName("Stack");
//! ctor
pcrxml::Stack::Stack(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(nullptr)
 ,timestepRange(nullptr)
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
 delete dataTypeDTD;dataTypeDTD=nullptr;
 delete timestepRange;timestepRange=nullptr;
}
//! copy ctor
pcrxml::Stack::Stack(const Stack& src):
pcrxml::Element(src)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 timestepRange= (src.timestepRange) ? new TimestepRange(*(src.timestepRange)): nullptr;
}
//! assignment operator
pcrxml::Stack& pcrxml::Stack::operator=(const Stack& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
  timestepRange= (src.timestepRange) ? new TimestepRange(*(src.timestepRange)): nullptr;
 }
return *this;
}
void pcrxml::Stack::fill(QDomElement el) const
{
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
 if (timestepRange) timestepRange->appendTo(el);
}
