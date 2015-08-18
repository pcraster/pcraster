/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_TABLE
#include "pcrgenxml_table.h"
#define INCLUDED_PCRGENXML_TABLE
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::Table::d_elementName("Table");
//! ctor
pcrxml::Table::Table(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Table::Table():Element()
 ,dataTypeDTD(0)
 {
 }
const std::string& pcrxml::Table::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::Table::~Table()
{
 clean();
}
//! clean
void pcrxml::Table::clean()
{
 delete dataTypeDTD;dataTypeDTD=0;
}
//! copy ctor
pcrxml::Table::Table(const Table& src):
pcrxml::Element(src)
{
 dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
}
//! assignment operator
pcrxml::Table& pcrxml::Table::operator=(const Table& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  dataTypeDTD=new DataTypeDTD(*(src.dataTypeDTD));
 }
return *this;
}
void pcrxml::Table::fill(QDomElement el) const
{
 if (dataTypeDTD) dataTypeDTD->appendTo(el);
}
