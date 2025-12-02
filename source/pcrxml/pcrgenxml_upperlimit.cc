/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_upperlimit.h"
#include <qdom.h>


const std::string pcrxml::UpperLimit::d_elementName("UpperLimit");

//! ctor
pcrxml::UpperLimit::UpperLimit(const QDomElement &element)
    : Element(element, d_elementName), value(element, "value", true),
      inclusive(element, "inclusive", true)
{
}

pcrxml::UpperLimit::UpperLimit() : Element()
{
}

const std::string &pcrxml::UpperLimit::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::UpperLimit::~UpperLimit()
{
  clean();
}

//! clean
void pcrxml::UpperLimit::clean()
{
}

//! copy ctor
pcrxml::UpperLimit::UpperLimit(const UpperLimit &src)
    : pcrxml::Element(src), value(src.value), inclusive(src.inclusive)
{
}

//! assignment operator
pcrxml::UpperLimit &pcrxml::UpperLimit::operator=(const UpperLimit &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
  }
  return *this;
}

void pcrxml::UpperLimit::fill(QDomElement el) const
{
  value.addToElement(el, "value");
  inclusive.addToElement(el, "inclusive");
}
