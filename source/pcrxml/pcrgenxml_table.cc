/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_table.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Table::d_elementName("Table");
//! ctor
pcrxml::Table::Table(const QDomElement& element):Element(element,d_elementName)
 ,dataTypeDTD(nullptr)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("dataTypeDTD");
   dataTypeDTD = new DataTypeDTD(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::Table::Table():Element()
 
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
 delete dataTypeDTD;dataTypeDTD=nullptr;
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
