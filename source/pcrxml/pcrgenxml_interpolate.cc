/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_INTERPOLATE
#include "pcrgenxml_interpolate.h"
#define INCLUDED_PCRGENXML_INTERPOLATE
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::Interpolate::d_elementName("Interpolate");
//! ctor
pcrxml::Interpolate::Interpolate(const QDomElement& element):Element(element,d_elementName)
 ,searchRadius(element,"searchRadius",true)
 ,mapCellSize(element,"mapCellSize",true)
 ,minimumPointsEachQuadrant(element,"minimumPointsEachQuadrant",true)
 {
 }
pcrxml::Interpolate::Interpolate():Element()
 {
 }
const std::string& pcrxml::Interpolate::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Interpolate::~Interpolate()
{
 clean();
}
//! clean
void pcrxml::Interpolate::clean()
{
}
//! copy ctor
pcrxml::Interpolate::Interpolate(const Interpolate& src):
pcrxml::Element(src)
,searchRadius(src.searchRadius)
,mapCellSize(src.mapCellSize)
,minimumPointsEachQuadrant(src.minimumPointsEachQuadrant)
{
}
//! assignment operator
pcrxml::Interpolate& pcrxml::Interpolate::operator=(const Interpolate& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::Interpolate::fill(QDomElement el) const
{
 searchRadius.addToElement(el,"searchRadius");
 mapCellSize.addToElement(el,"mapCellSize");
 minimumPointsEachQuadrant.addToElement(el,"minimumPointsEachQuadrant");
}
