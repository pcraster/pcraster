/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_riveraxisfile.h"
#include <qdom.h>



const std::string pcrxml::RiverAxisFile::d_elementName("RiverAxisFile");
//! ctor
pcrxml::RiverAxisFile::RiverAxisFile(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 {
 }
pcrxml::RiverAxisFile::RiverAxisFile():Element()
 {
 }
const std::string& pcrxml::RiverAxisFile::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::RiverAxisFile::~RiverAxisFile()
{
 clean();
}
//! clean
void pcrxml::RiverAxisFile::clean()
{
}
//! copy ctor
pcrxml::RiverAxisFile::RiverAxisFile(const RiverAxisFile& src):
pcrxml::Element(src)
,value(src.value)
{
}
//! assignment operator
pcrxml::RiverAxisFile& pcrxml::RiverAxisFile::operator=(const RiverAxisFile& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::RiverAxisFile::fill(QDomElement el) const
{
 value.addToElement(el,"value");
}
