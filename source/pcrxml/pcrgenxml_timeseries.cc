/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_TIMESERIES
#include "pcrgenxml_timeseries.h"
#define INCLUDED_PCRGENXML_TIMESERIES
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::TimeSeries::d_elementName("TimeSeries");
//! ctor
pcrxml::TimeSeries::TimeSeries(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::TimeSeries::TimeSeries():Element()
 ,dataTypeDTD(0)
 {
 }
const std::string& pcrxml::TimeSeries::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::TimeSeries::~TimeSeries()
{
 clean();
}
//! clean
void pcrxml::TimeSeries::clean()
{
 delete dataTypeDTD;dataTypeDTD=0;
}
//! copy ctor
pcrxml::TimeSeries::TimeSeries(const TimeSeries& src):
pcrxml::Element(src)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
}
//! assignment operator
pcrxml::TimeSeries& pcrxml::TimeSeries::operator=(const TimeSeries& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 }
return *this;
}
void pcrxml::TimeSeries::fill(QDomElement el) const
{
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
}
