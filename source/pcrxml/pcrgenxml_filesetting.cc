/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_filesetting.h"
#include <qdom.h>



const std::string pcrxml::FileSetting::d_elementName("FileSetting");
//! ctor
pcrxml::FileSetting::FileSetting(const QDomElement& element):Element(element,d_elementName)
 ,name(element,"name",true)
 ,externalFileName(element,"externalFileName",true)
 {
 }
pcrxml::FileSetting::FileSetting():Element()
 {
 }
const std::string& pcrxml::FileSetting::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::FileSetting::~FileSetting()
{
 clean();
}
//! clean
void pcrxml::FileSetting::clean()
{
}
//! copy ctor
pcrxml::FileSetting::FileSetting(const FileSetting& src):
pcrxml::Element(src)
,name(src.name)
,externalFileName(src.externalFileName)
{
}
//! assignment operator
pcrxml::FileSetting& pcrxml::FileSetting::operator=(const FileSetting& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::FileSetting::fill(QDomElement el) const
{
 name.addToElement(el,"name");
 externalFileName.addToElement(el,"externalFileName");
}
