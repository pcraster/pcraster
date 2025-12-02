/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_testemptyelementnoattr.h"
#include <qdom.h>


const std::string pcrxml::TestEmptyElementNoAttr::d_elementName("TestEmptyElementNoAttr");

//! ctor
pcrxml::TestEmptyElementNoAttr::TestEmptyElementNoAttr(const QDomElement &element)
    : Element(element, d_elementName)
{
}

pcrxml::TestEmptyElementNoAttr::TestEmptyElementNoAttr() : Element()
{
}

const std::string &pcrxml::TestEmptyElementNoAttr::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::TestEmptyElementNoAttr::~TestEmptyElementNoAttr()
{
  clean();
}

//! clean
void pcrxml::TestEmptyElementNoAttr::clean()
{
}

//! copy ctor
pcrxml::TestEmptyElementNoAttr::TestEmptyElementNoAttr(const TestEmptyElementNoAttr &src)
    : pcrxml::Element(src)
{
}

//! assignment operator
pcrxml::TestEmptyElementNoAttr &
pcrxml::TestEmptyElementNoAttr::operator=(const TestEmptyElementNoAttr &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
  }
  return *this;
}

void pcrxml::TestEmptyElementNoAttr::fill(QDomElement /* el */) const
{
}
