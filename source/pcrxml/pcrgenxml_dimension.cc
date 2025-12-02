/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_dimension.h"
#include <qdom.h>


const std::string pcrxml::Dimension::d_elementName("dimension");

//! ctor
pcrxml::Dimension::Dimension(const QDomElement &element)
    : Element(element, d_elementName), base(element, "base", true), power(element, "power", true)
{
}

pcrxml::Dimension::Dimension() : Element()
{
}

const std::string &pcrxml::Dimension::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::Dimension::~Dimension()
{
  clean();
}

//! clean
void pcrxml::Dimension::clean()
{
}

//! copy ctor
pcrxml::Dimension::Dimension(const Dimension &src)
    : pcrxml::Element(src), base(src.base), power(src.power)
{
}

//! assignment operator
pcrxml::Dimension &pcrxml::Dimension::operator=(const Dimension &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
  }
  return *this;
}

void pcrxml::Dimension::fill(QDomElement el) const
{
  base.addToElement(el, "base");
  power.addToElement(el, "power");
}
