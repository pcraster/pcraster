/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_DATASETCONFIGURATION
#include "pcrgenxml_datasetconfiguration.h"
#define INCLUDED_PCRGENXML_DATASETCONFIGURATION
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::DataSetConfiguration::d_elementName("DataSetConfiguration");
//! ctor
pcrxml::DataSetConfiguration::DataSetConfiguration(const QDomElement& element):Element(element,d_elementName)
 ,version(element,"version",true)
 ,input(0)
 ,compute(0)
 ,interpolate(0)
 ,lodings(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("Input");
   input = new Input(v.processChild());
   // required element
   v.checkRequiredChild("Compute");
   compute = new Compute(v.processChild());
   // optional element
   if(v.currentChildEq("Interpolate"))
     interpolate = new Interpolate(v.processChild());
   // optional element
   if(v.currentChildEq("Lodings"))
     lodings = new Lodings(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::DataSetConfiguration::DataSetConfiguration():Element()
 ,input(0)
 ,compute(0)
 ,interpolate(0)
 ,lodings(0)
 {
 }
const std::string& pcrxml::DataSetConfiguration::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::DataSetConfiguration::~DataSetConfiguration()
{
 clean();
}
//! clean
void pcrxml::DataSetConfiguration::clean()
{
 delete input;input=0;
 delete compute;compute=0;
 delete interpolate;interpolate=0;
 delete lodings;lodings=0;
}
//! copy ctor
pcrxml::DataSetConfiguration::DataSetConfiguration(const DataSetConfiguration& src):
pcrxml::Element(src)
,version(src.version)
{
 input=new Input(*(src.input));
 compute=new Compute(*(src.compute));
 interpolate= (src.interpolate) ? new Interpolate(*(src.interpolate)): 0;
 lodings= (src.lodings) ? new Lodings(*(src.lodings)): 0;
}
//! assignment operator
pcrxml::DataSetConfiguration& pcrxml::DataSetConfiguration::operator=(const DataSetConfiguration& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  input=new Input(*(src.input));
  compute=new Compute(*(src.compute));
  interpolate= (src.interpolate) ? new Interpolate(*(src.interpolate)): 0;
  lodings= (src.lodings) ? new Lodings(*(src.lodings)): 0;
 }
return *this;
}
void pcrxml::DataSetConfiguration::fill(QDomElement el) const
{
 version.addToElement(el,"version");
 if (input) input->appendTo(el);
 if (compute) compute->appendTo(el);
 if (interpolate) interpolate->appendTo(el);
 if (lodings) lodings->appendTo(el);
}
