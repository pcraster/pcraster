/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_expirationdate.h"
#include <qdom.h>


const std::string pcrxml::ExpirationDate::d_elementName("expirationDate");

//! ctor
pcrxml::ExpirationDate::ExpirationDate(const QDomElement &element)
    : Element(element, d_elementName), value(element, "value", true),
      dateFormat(element, "dateFormat", false)
{
}

pcrxml::ExpirationDate::ExpirationDate() : Element()
{
}

const std::string &pcrxml::ExpirationDate::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::ExpirationDate::~ExpirationDate()
{
  clean();
}

//! clean
void pcrxml::ExpirationDate::clean()
{
}

//! copy ctor
pcrxml::ExpirationDate::ExpirationDate(const ExpirationDate &src)
    : pcrxml::Element(src), value(src.value), dateFormat(src.dateFormat)
{
}

//! assignment operator
pcrxml::ExpirationDate &pcrxml::ExpirationDate::operator=(const ExpirationDate &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
  }
  return *this;
}

void pcrxml::ExpirationDate::fill(QDomElement el) const
{
  value.addToElement(el, "value");
  dateFormat.addToElement(el, "dateFormat");
}
