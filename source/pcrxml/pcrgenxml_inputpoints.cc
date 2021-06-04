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
 ,computedRiverAxis(nullptr)
 ,riverAxisFile(nullptr)
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
 ,computedRiverAxis(nullptr)
 ,riverAxisFile(nullptr)
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
 delete computedRiverAxis;computedRiverAxis=nullptr;
 delete riverAxisFile;riverAxisFile=nullptr;
}
//! copy ctor
pcrxml::InputPoints::InputPoints(const InputPoints& src):
pcrxml::Element(src)
,lodingDistance(src.lodingDistance)
,maxPointDeviation(src.maxPointDeviation)
{
 computedRiverAxis= (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)): nullptr;
 riverAxisFile= (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)): nullptr;
}
//! assignment operator
pcrxml::InputPoints& pcrxml::InputPoints::operator=(const InputPoints& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  computedRiverAxis= (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)): nullptr;
  riverAxisFile= (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)): nullptr;
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
