/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_inputpoints.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::InputPoints::d_elementName("InputPoints");

//! ctor
pcrxml::InputPoints::InputPoints(const QDomElement &element)
    : Element(element, d_elementName), lodingDistance(element, "lodingDistance", true),
      maxPointDeviation(element, "maxPointDeviation", true)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("ComputedRiverAxis")) {
      computedRiverAxis = new ComputedRiverAxis(v.processChild());
    }
    // optional element
    if (v.currentChildEq("RiverAxisFile")) {
      riverAxisFile = new RiverAxisFile(v.processChild());
    }
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::InputPoints::InputPoints() : Element()

{
}

const std::string &pcrxml::InputPoints::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::InputPoints::~InputPoints()
{
  clean();
}

//! clean
void pcrxml::InputPoints::clean()
{
  delete computedRiverAxis;
  computedRiverAxis = nullptr;
  delete riverAxisFile;
  riverAxisFile = nullptr;
}

//! copy ctor
pcrxml::InputPoints::InputPoints(const InputPoints &src)
    : pcrxml::Element(src), lodingDistance(src.lodingDistance), maxPointDeviation(src.maxPointDeviation)
{
  computedRiverAxis =
      (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)) : nullptr;
  riverAxisFile = (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)) : nullptr;
}

//! assignment operator
pcrxml::InputPoints &pcrxml::InputPoints::operator=(const InputPoints &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    computedRiverAxis =
        (src.computedRiverAxis) ? new ComputedRiverAxis(*(src.computedRiverAxis)) : nullptr;
    riverAxisFile = (src.riverAxisFile) ? new RiverAxisFile(*(src.riverAxisFile)) : nullptr;
  }
  return *this;
}

void pcrxml::InputPoints::fill(QDomElement el) const
{
  lodingDistance.addToElement(el, "lodingDistance");
  maxPointDeviation.addToElement(el, "maxPointDeviation");
  if (computedRiverAxis) {
    computedRiverAxis->appendTo(el);
  }
  if (riverAxisFile) {
    riverAxisFile->appendTo(el);
  }
}
