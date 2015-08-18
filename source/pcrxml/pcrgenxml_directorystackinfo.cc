/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#include "pcrgenxml_directorystackinfo.h"
#define INCLUDED_PCRGENXML_DIRECTORYSTACKINFO
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::DirectoryStackInfo::d_elementName("DirectoryStackInfo");
//! ctor
pcrxml::DirectoryStackInfo::DirectoryStackInfo(const QDomElement& element):Element(element,d_elementName)
 ,allMissingValue(element,"allMissingValue",false)
 ,minimumValue(element,"minimumValue",true)
 ,maximumValue(element,"maximumValue",true)
 ,stackEnd(element,"stackEnd",false)
 ,dataTypeDTD(element,"dataTypeDTD",true)
 {
 }
pcrxml::DirectoryStackInfo::DirectoryStackInfo():Element()
 {
 }
const std::string& pcrxml::DirectoryStackInfo::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::DirectoryStackInfo::~DirectoryStackInfo()
{
 clean();
}
//! clean
void pcrxml::DirectoryStackInfo::clean()
{
}
//! copy ctor
pcrxml::DirectoryStackInfo::DirectoryStackInfo(const DirectoryStackInfo& src):
pcrxml::Element(src)
,allMissingValue(src.allMissingValue)
,minimumValue(src.minimumValue)
,maximumValue(src.maximumValue)
,stackEnd(src.stackEnd)
,dataTypeDTD(src.dataTypeDTD)
{
}
//! assignment operator
pcrxml::DirectoryStackInfo& pcrxml::DirectoryStackInfo::operator=(const DirectoryStackInfo& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::DirectoryStackInfo::fill(QDomElement el) const
{
 allMissingValue.addToElement(el,"allMissingValue");
 minimumValue.addToElement(el,"minimumValue");
 maximumValue.addToElement(el,"maximumValue");
 stackEnd.addToElement(el,"stackEnd");
 dataTypeDTD.addToElement(el,"dataTypeDTD");
}
