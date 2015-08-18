/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_MODELRUNCHILD
#include "pcrgenxml_modelrunchild.h"
#define INCLUDED_PCRGENXML_MODELRUNCHILD
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::ModelRunChild::d_elementName("ModelRunChild");
//! ctor
pcrxml::ModelRunChild::ModelRunChild(const QDomElement& element):Element(element,d_elementName)
 ,directoryName(element,"directoryName",true)
 {
 }
pcrxml::ModelRunChild::ModelRunChild():Element()
 {
 }
const std::string& pcrxml::ModelRunChild::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ModelRunChild::~ModelRunChild()
{
 clean();
}
//! clean
void pcrxml::ModelRunChild::clean()
{
}
//! copy ctor
pcrxml::ModelRunChild::ModelRunChild(const ModelRunChild& src):
pcrxml::Element(src)
,directoryName(src.directoryName)
{
}
//! assignment operator
pcrxml::ModelRunChild& pcrxml::ModelRunChild::operator=(const ModelRunChild& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::ModelRunChild::fill(QDomElement el) const
{
 directoryName.addToElement(el,"directoryName");
}
