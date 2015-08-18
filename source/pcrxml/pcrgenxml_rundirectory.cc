/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_RUNDIRECTORY
#include "pcrgenxml_rundirectory.h"
#define INCLUDED_PCRGENXML_RUNDIRECTORY
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::RunDirectory::d_elementName("RunDirectory");
//! ctor
pcrxml::RunDirectory::RunDirectory(const QDomElement& element):Element(element,d_elementName)
 ,modelRunSettings(0)
 ,modelRunStatus(0)
 {
  try {
   ChildElementVisitor v(element);

   // optional element
   if(v.currentChildEq("ModelRunSettings"))
     modelRunSettings = new ModelRunSettings(v.processChild());
   // optional element
   if(v.currentChildEq("ModelRunStatus"))
     modelRunStatus = new ModelRunStatus(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::RunDirectory::RunDirectory():Element()
 ,modelRunSettings(0)
 ,modelRunStatus(0)
 {
 }
const std::string& pcrxml::RunDirectory::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::RunDirectory::~RunDirectory()
{
 clean();
}
//! clean
void pcrxml::RunDirectory::clean()
{
 delete modelRunSettings;modelRunSettings=0;
 delete modelRunStatus;modelRunStatus=0;
}
//! copy ctor
pcrxml::RunDirectory::RunDirectory(const RunDirectory& src):
pcrxml::Element(src)
{
 modelRunSettings= (src.modelRunSettings) ? new ModelRunSettings(*(src.modelRunSettings)): 0;
 modelRunStatus= (src.modelRunStatus) ? new ModelRunStatus(*(src.modelRunStatus)): 0;
}
//! assignment operator
pcrxml::RunDirectory& pcrxml::RunDirectory::operator=(const RunDirectory& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  modelRunSettings= (src.modelRunSettings) ? new ModelRunSettings(*(src.modelRunSettings)): 0;
  modelRunStatus= (src.modelRunStatus) ? new ModelRunStatus(*(src.modelRunStatus)): 0;
 }
return *this;
}
void pcrxml::RunDirectory::fill(QDomElement el) const
{
 if (modelRunSettings) modelRunSettings->appendTo(el);
 if (modelRunStatus) modelRunStatus->appendTo(el);
}
