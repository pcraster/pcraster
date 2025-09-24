/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_timeseries.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::TimeSeries::d_elementName("TimeSeries");
//! ctor
pcrxml::TimeSeries::TimeSeries(const QDomElement& element):Element(element,d_elementName)
 
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::TimeSeries::TimeSeries():Element()
 
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
 delete dataTypeDTD;dataTypeDTD=nullptr;
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
