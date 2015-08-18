/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif
#ifndef INCLUDED_PCRGENXML_EXCHANGEITEM
#include "pcrgenxml_exchangeitem.h"
#define INCLUDED_PCRGENXML_EXCHANGEITEM
#endif

#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif



#ifndef INCLUDED_PCRXML_CHILDELEMENTVISITOR
#include "pcrxml_childelementvisitor.h"
#define INCLUDED_PCRXML_CHILDELEMENTVISITOR
#endif

const std::string pcrxml::ExchangeItem::d_elementName("exchangeItem");
//! ctor
pcrxml::ExchangeItem::ExchangeItem(const QDomElement& element):Element(element,d_elementName)
 ,exchangeDirection(element,"exchangeDirection",false)
 ,index(element,"index",false)
 ,variable(0)
 {
  try {
   ChildElementVisitor v(element);

   // required element
   v.checkRequiredChild("variable");
   variable = new Variable(v.processChild());
  } catch (...) { clean(); throw; }
 }
pcrxml::ExchangeItem::ExchangeItem():Element()
 ,variable(0)
 {
 }
const std::string& pcrxml::ExchangeItem::elementName() const
{
 return d_elementName;
}
//! dtor
pcrxml::ExchangeItem::~ExchangeItem()
{
 clean();
}
//! clean
void pcrxml::ExchangeItem::clean()
{
 delete variable;variable=0;
}
//! copy ctor
pcrxml::ExchangeItem::ExchangeItem(const ExchangeItem& src):
pcrxml::Element(src)
,exchangeDirection(src.exchangeDirection)
,index(src.index)
{
 variable=new Variable(*(src.variable));
}
//! assignment operator
pcrxml::ExchangeItem& pcrxml::ExchangeItem::operator=(const ExchangeItem& src)
{
 if(this != &src)
 {
   clean(); PRECOND(false);
  variable=new Variable(*(src.variable));
 }
return *this;
}
void pcrxml::ExchangeItem::fill(QDomElement el) const
{
 exchangeDirection.addToElement(el,"exchangeDirection");
 index.addToElement(el,"index");
 if (variable) variable->appendTo(el);
}
