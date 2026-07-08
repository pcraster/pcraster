/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "pcrxml_element.h"
#include "pcrgenxml_variable.h"
#include "debug.h"
#include "pcrgenxml_exchangeitem.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>
#include <string>

const std::string pcrxml::ExchangeItem::d_elementName("exchangeItem");

//! ctor
pcrxml::ExchangeItem::ExchangeItem(const QDomElement &element)
    : Element(element, d_elementName), exchangeDirection(element, "exchangeDirection", false),
      index(element, "index", false)
{
  try {
    ChildElementVisitor v(element);

    // required element
    v.checkRequiredChild("variable");
    variable = new Variable(v.processChild());
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::ExchangeItem::ExchangeItem() : Element()

{
}

const std::string &pcrxml::ExchangeItem::elementName() const
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
  delete variable;
  variable = nullptr;
}

//! copy ctor
pcrxml::ExchangeItem::ExchangeItem(const ExchangeItem &src)
    : pcrxml::Element(src), exchangeDirection(src.exchangeDirection), index(src.index), variable(new Variable(*(src.variable)))
{
}

//! assignment operator
pcrxml::ExchangeItem &pcrxml::ExchangeItem::operator=(const ExchangeItem &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    variable = new Variable(*(src.variable));
  }
  return *this;
}

void pcrxml::ExchangeItem::fill(QDomElement el) const
{
  exchangeDirection.addToElement(el, "exchangeDirection");
  index.addToElement(el, "index");
  if (variable != nullptr) {
    variable->appendTo(el);
  }
}
