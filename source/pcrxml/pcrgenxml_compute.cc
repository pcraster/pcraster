/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_compute.h"
#include <qdom.h>



const std::string pcrxml::Compute::d_elementName("Compute");
//! ctor
pcrxml::Compute::Compute(const QDomElement& element):Element(element,d_elementName)
 ,maximalMigrationDistance(element,"maximalMigrationDistance",true)
 ,maximumZeroDownCrossingThreshold(element,"maximumZeroDownCrossingThreshold",true)
 ,minimalSlopeBrinkPoint(element,"minimalSlopeBrinkPoint",true)
 ,minimalBrinkHeight(element,"minimalBrinkHeight",true)
 {
 }
pcrxml::Compute::Compute():Element()
 {
 }
const std::string& pcrxml::Compute::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Compute::~Compute()
{
 clean();
}
//! clean
void pcrxml::Compute::clean()
{
}
//! copy ctor
pcrxml::Compute::Compute(const Compute& src):
pcrxml::Element(src)
,maximalMigrationDistance(src.maximalMigrationDistance)
,maximumZeroDownCrossingThreshold(src.maximumZeroDownCrossingThreshold)
,minimalSlopeBrinkPoint(src.minimalSlopeBrinkPoint)
,minimalBrinkHeight(src.minimalBrinkHeight)
{
}
//! assignment operator
pcrxml::Compute& pcrxml::Compute::operator=(const Compute& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Compute::fill(QDomElement el) const
{
 maximalMigrationDistance.addToElement(el,"maximalMigrationDistance");
 maximumZeroDownCrossingThreshold.addToElement(el,"maximumZeroDownCrossingThreshold");
 minimalSlopeBrinkPoint.addToElement(el,"minimalSlopeBrinkPoint");
 minimalBrinkHeight.addToElement(el,"minimalBrinkHeight");
}
