/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_MODELRUNSTATUS
#include "pcrgenxml_modelrunstatus.h"
#define INCLUDED_PCRGENXML_MODELRUNSTATUS
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



const std::string pcrxml::ModelRunStatus::d_elementName("ModelRunStatus");
//! ctor
pcrxml::ModelRunStatus::ModelRunStatus(const QDomElement& element):Element(element,d_elementName)
 ,userInterfaceName(element,"userInterfaceName",false)
 ,userCanChange(element,"userCanChange",true)
 ,userCanRun(element,"userCanRun",true)
 ,userCanDelete(element,"userCanDelete",true)
 ,userDerive(element,"userDerive",true)
 ,resultLastRun(element,"resultLastRun",true)
 ,lastTimeStep(element,"lastTimeStep",false)
 {
 }
pcrxml::ModelRunStatus::ModelRunStatus():Element()
 {
 }
const std::string& pcrxml::ModelRunStatus::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ModelRunStatus::~ModelRunStatus()
{
 clean();
}
//! clean
void pcrxml::ModelRunStatus::clean()
{
}
//! copy ctor
pcrxml::ModelRunStatus::ModelRunStatus(const ModelRunStatus& src):
pcrxml::Element(src)
,userInterfaceName(src.userInterfaceName)
,userCanChange(src.userCanChange)
,userCanRun(src.userCanRun)
,userCanDelete(src.userCanDelete)
,userDerive(src.userDerive)
,resultLastRun(src.resultLastRun)
,lastTimeStep(src.lastTimeStep)
{
}
//! assignment operator
pcrxml::ModelRunStatus& pcrxml::ModelRunStatus::operator=(const ModelRunStatus& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
 }
return *this;
}
void pcrxml::ModelRunStatus::fill(QDomElement el) const
{
 userInterfaceName.addToElement(el,"userInterfaceName");
 userCanChange.addToElement(el,"userCanChange");
 userCanRun.addToElement(el,"userCanRun");
 userCanDelete.addToElement(el,"userCanDelete");
 userDerive.addToElement(el,"userDerive");
 resultLastRun.addToElement(el,"resultLastRun");
 lastTimeStep.addToElement(el,"lastTimeStep");
}
