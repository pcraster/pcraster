/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_INPUT
#include "pcrgenxml_input.h"
#define INCLUDED_PCRGENXML_INPUT
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Input::d_elementName("Input");
//! ctor
pcrxml::Input::Input(const QDomElement& element):Element(element,d_elementName)
 ,flipZ(element,"flipZ",true)
 ,samplingInterval(element,"samplingInterval",true)
 ,migrDirection(element,"migrDirection",false)
 ,inputLodings(0)
 ,inputPoints(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("InputLodings"))
     inputLodings = new InputLodings(v.processChild());
   // optional element
   if(v.currentChildEq("InputPoints"))
     inputPoints = new InputPoints(v.processChild());
   // + repeated element
   v.checkRequiredChild("InputFile");
   while(v.currentChildEq("InputFile"))
     inputFile.push_back(new InputFile(v.processChild()));
  } catch (...) { clean(); throw; }
 }
pcrxml::Input::Input():Element()
 ,inputLodings(0)
 ,inputPoints(0)
 {
 }
const std::string& pcrxml::Input::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Input::~Input()
{
 clean();
}
//! clean
void pcrxml::Input::clean()
{
 delete inputLodings;inputLodings=0;
 delete inputPoints;inputPoints=0;
 for(size_t i=0; i<inputFile.size(); i++) delete inputFile[i];
inputFile.clear();
}
//! copy ctor
pcrxml::Input::Input(const Input& src):
pcrxml::Element(src)
,flipZ(src.flipZ)
,samplingInterval(src.samplingInterval)
,migrDirection(src.migrDirection)
{
 inputLodings= (src.inputLodings) ? new InputLodings(*(src.inputLodings)): 0;
 inputPoints= (src.inputPoints) ? new InputPoints(*(src.inputPoints)): 0;
 for(size_t i=0; i<src.inputFile.size(); i++) inputFile.push_back(new InputFile(*(src.inputFile[i])));
}
//! assignment operator
pcrxml::Input& pcrxml::Input::operator=(const Input& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  inputLodings= (src.inputLodings) ? new InputLodings(*(src.inputLodings)): 0;
  inputPoints= (src.inputPoints) ? new InputPoints(*(src.inputPoints)): 0;
  for(size_t i=0; i<src.inputFile.size(); i++) inputFile.push_back(new InputFile(*(src.inputFile[i])));
 }
return *this;
}
void pcrxml::Input::fill(QDomElement el) const
{
 flipZ.addToElement(el,"flipZ");
 samplingInterval.addToElement(el,"samplingInterval");
 migrDirection.addToElement(el,"migrDirection");
 if (inputLodings) inputLodings->appendTo(el);
 if (inputPoints) inputPoints->appendTo(el);
 for(size_t i=0; i<inputFile.size(); i++) inputFile[i]->appendTo(el);
}
