/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_inputlodings.h"
#include <qdom.h>



const std::string pcrxml::InputLodings::d_elementName("InputLodings");
//! ctor
pcrxml::InputLodings::InputLodings(const QDomElement& element):Element(element,d_elementName)
 ,inputType(element,"inputType",true)
 {
 }
pcrxml::InputLodings::InputLodings():Element()
 {
 }
const std::string& pcrxml::InputLodings::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::InputLodings::~InputLodings()
{
 clean();
}
//! clean
void pcrxml::InputLodings::clean()
{
}
//! copy ctor
pcrxml::InputLodings::InputLodings(const InputLodings& src):
pcrxml::Element(src)
,inputType(src.inputType)
{
}
//! assignment operator
pcrxml::InputLodings& pcrxml::InputLodings::operator=(const InputLodings& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::InputLodings::fill(QDomElement el) const
{
 inputType.addToElement(el,"inputType");
}
