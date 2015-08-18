/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_INPUTFILE
#include "pcrgenxml_inputfile.h"
#define INCLUDED_PCRGENXML_INPUTFILE
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::InputFile::d_elementName("InputFile");
//! ctor
pcrxml::InputFile::InputFile(const QDomElement& element):Element(element,d_elementName)
 ,value(element,"value",true)
 {
 }
pcrxml::InputFile::InputFile():Element()
 {
 }
const std::string& pcrxml::InputFile::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::InputFile::~InputFile()
{
 clean();
}
//! clean
void pcrxml::InputFile::clean()
{
}
//! copy ctor
pcrxml::InputFile::InputFile(const InputFile& src):
pcrxml::Element(src)
,value(src.value)
{
}
//! assignment operator
pcrxml::InputFile& pcrxml::InputFile::operator=(const InputFile& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::InputFile::fill(QDomElement el) const
{
 value.addToElement(el,"value");
}
