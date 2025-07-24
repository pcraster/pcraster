/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_showdata.h"
#include <qdom.h>



const std::string pcrxml::ShowData::d_elementName("ShowData");
//! ctor
pcrxml::ShowData::ShowData(const QDomElement& element):Element(element,d_elementName)
 {
 }
pcrxml::ShowData::ShowData():Element()
 {
 }
const std::string& pcrxml::ShowData::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ShowData::~ShowData()
{
 clean();
}
//! clean
void pcrxml::ShowData::clean()
{
}
//! copy ctor
pcrxml::ShowData::ShowData(const ShowData& src):
pcrxml::Element(src)
{
}
//! assignment operator
pcrxml::ShowData& pcrxml::ShowData::operator=(const ShowData& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::ShowData::fill(QDomElement /* el */) const
{
}
