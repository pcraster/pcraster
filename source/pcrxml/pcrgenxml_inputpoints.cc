/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_INPUTPOINTS
#include "pcrgenxml_inputpoints.h"
#define INCLUDED_PCRGENXML_INPUTPOINTS
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::InputPoints::d_elementName("InputPoints");
//! ctor
pcrxml::InputPoints::InputPoints(const QDomElement& element):Element(element,d_elementName)
 ,lodingDistance(element,"lodingDistance",true)
 ,maxPointDeviation(element,"maxPointDeviation",true)
 ,computedRiverAxis(0)
 ,riverAxisFile(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("ComputedRiverAxis"))
     computedRiverAxis = new ComputedRiverAxis(v.processChild());
   // optional element
   if(v.currentChildEq("RiverAxisFile"))
     riverAxisFile = new RiverAxisFile(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::InputPoints::InputPoints():Element()
 ,computedRiverAxis(0)
 ,riverAxisFile(0)
 {
 }
const std::string& pcrxml::InputPoints::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::InputPoints::~InputPoints()
{
 clean();
}
//! clean
void pcrxml::InputPoints::clean()
{
 delete computedRiverAxis;computedRiverAxis=0;
 delete riverAxisFile;riverAxisFile=0;
}
//! copy ctor
pcrxml::InputPoints::InputPoints(const InputPoints& src):
pcrxml::Element(src)
,lodingDistance(src.lodingDistance)
,maxPointDeviation(src.maxPointDeviation)
{
 computedRiverAxis= (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)): 0;
 riverAxisFile= (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)): 0;
}
//! assignment operator
pcrxml::InputPoints& pcrxml::InputPoints::operator=(const InputPoints& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  computedRiverAxis= (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)): 0;
  riverAxisFile= (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)): 0;
 }
return *this;
}
void pcrxml::InputPoints::fill(QDomElement el) const
{
 lodingDistance.addToElement(el,"lodingDistance");
 maxPointDeviation.addToElement(el,"maxPointDeviation");
 if (computedRiverAxis) computedRiverAxis->appendTo(el);
 if (riverAxisFile) riverAxisFile->appendTo(el);
}
