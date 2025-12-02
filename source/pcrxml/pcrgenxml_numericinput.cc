/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_numericinput.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::NumericInput::d_elementName("NumericInput");

//! ctor
pcrxml::NumericInput::NumericInput(const QDomElement &element) : Element(element, d_elementName)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("LowerLimit"))
      lowerLimit = new LowerLimit(v.processChild());
    // optional element
    if (v.currentChildEq("UpperLimit"))
      upperLimit = new UpperLimit(v.processChild());
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::NumericInput::NumericInput() : Element()

{
}

const std::string &pcrxml::NumericInput::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::NumericInput::~NumericInput()
{
  clean();
}

//! clean
void pcrxml::NumericInput::clean()
{
  delete lowerLimit;
  lowerLimit = nullptr;
  delete upperLimit;
  upperLimit = nullptr;
}

//! copy ctor
pcrxml::NumericInput::NumericInput(const NumericInput &src) : pcrxml::Element(src)
{
  lowerLimit = (src.lowerLimit) ? new LowerLimit(*(src.lowerLimit)) : nullptr;
  upperLimit = (src.upperLimit) ? new UpperLimit(*(src.upperLimit)) : nullptr;
}

//! assignment operator
pcrxml::NumericInput &pcrxml::NumericInput::operator=(const NumericInput &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    lowerLimit = (src.lowerLimit) ? new LowerLimit(*(src.lowerLimit)) : nullptr;
    upperLimit = (src.upperLimit) ? new UpperLimit(*(src.upperLimit)) : nullptr;
  }
  return *this;
}

void pcrxml::NumericInput::fill(QDomElement el) const
{
  if (lowerLimit)
    lowerLimit->appendTo(el);
  if (upperLimit)
    upperLimit->appendTo(el);
}
