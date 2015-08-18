/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_COMPUTEDRIVERAXIS
#include "pcrgenxml_computedriveraxis.h"
#define INCLUDED_PCRGENXML_COMPUTEDRIVERAXIS
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::ComputedRiverAxis::d_elementName("ComputedRiverAxis");
//! ctor
pcrxml::ComputedRiverAxis::ComputedRiverAxis(const QDomElement& element):Element(element,d_elementName)
 {
 }
pcrxml::ComputedRiverAxis::ComputedRiverAxis():Element()
 {
 }
const std::string& pcrxml::ComputedRiverAxis::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ComputedRiverAxis::~ComputedRiverAxis()
{
 clean();
}
//! clean
void pcrxml::ComputedRiverAxis::clean()
{
}
//! copy ctor
pcrxml::ComputedRiverAxis::ComputedRiverAxis(const ComputedRiverAxis& src):
pcrxml::Element(src)
{
}
//! assignment operator
pcrxml::ComputedRiverAxis& pcrxml::ComputedRiverAxis::operator=(const ComputedRiverAxis& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::ComputedRiverAxis::fill(QDomElement /* el */) const
{
}
