/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_NUMERICINPUT
#include "pcrgenxml_numericinput.h"
#define INCLUDED_PCRGENXML_NUMERICINPUT
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::NumericInput::d_elementName("NumericInput");
//! ctor
pcrxml::NumericInput::NumericInput(const QDomElement& element):Element(element,d_elementName)
 ,lowerLimit(0)
 ,upperLimit(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("LowerLimit"))
     lowerLimit = new LowerLimit(v.processChild());
   // optional element
   if(v.currentChildEq("UpperLimit"))
     upperLimit = new UpperLimit(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::NumericInput::NumericInput():Element()
 ,lowerLimit(0)
 ,upperLimit(0)
 {
 }
const std::string& pcrxml::NumericInput::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::NumericInput::~NumericInput()
{
 clean();
}
//! clean
void pcrxml::NumericInput::clean()
{
 delete lowerLimit;lowerLimit=0;
 delete upperLimit;upperLimit=0;
}
//! copy ctor
pcrxml::NumericInput::NumericInput(const NumericInput& src):
pcrxml::Element(src)
{
 lowerLimit= (src.lowerLimit) ? new LowerLimit(*(src.lowerLimit)): 0;
 upperLimit= (src.upperLimit) ? new UpperLimit(*(src.upperLimit)): 0;
}
//! assignment operator
pcrxml::NumericInput& pcrxml::NumericInput::operator=(const NumericInput& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  lowerLimit= (src.lowerLimit) ? new LowerLimit(*(src.lowerLimit)): 0;
  upperLimit= (src.upperLimit) ? new UpperLimit(*(src.upperLimit)): 0;
 }
return *this;
}
void pcrxml::NumericInput::fill(QDomElement el) const
{
 if (lowerLimit) lowerLimit->appendTo(el);
 if (upperLimit) upperLimit->appendTo(el);
}
