/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATAENVELOP
#include "pcrgenxml_dataenvelop.h"
#define INCLUDED_PCRGENXML_DATAENVELOP
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::DataEnvelop::d_elementName("dataEnvelop");
//! ctor
pcrxml::DataEnvelop::DataEnvelop(const QDomElement& element):PCDATAElement(element,d_elementName)
 ,encoding(element,"encoding",true)
 {
 }
pcrxml::DataEnvelop::DataEnvelop():PCDATAElement()
 {
 }
const std::string& pcrxml::DataEnvelop::elementName() const
{
 return d_elementName;
}
//! string ctor
pcrxml::DataEnvelop::DataEnvelop(const std::string& contents):
 PCDATAElement(contents) {}
//! dtor
pcrxml::DataEnvelop::~DataEnvelop()
{
 clean();
}
//! clean
void pcrxml::DataEnvelop::clean()
{
}
//! copy ctor
pcrxml::DataEnvelop::DataEnvelop(const DataEnvelop& src):
pcrxml::PCDATAElement(src)
,encoding(src.encoding)
{
}
//! assignment operator
pcrxml::DataEnvelop& pcrxml::DataEnvelop::operator=(const DataEnvelop& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::DataEnvelop::fill(QDomElement el) const
{
 el.appendChild(contentsNode(el));
 encoding.addToElement(el,"encoding");
}
