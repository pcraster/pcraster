/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_rasterspace.h"
#include <qdom.h>


const std::string pcrxml::RasterSpace::d_elementName("rasterSpace");

//! ctor
pcrxml::RasterSpace::RasterSpace(const QDomElement &element)
    : Element(element, d_elementName), nrRows(element, "nrRows", true), nrCols(element, "nrCols", true),
      cellSize(element, "cellSize", false), xLowerLeftCorner(element, "xLowerLeftCorner", false),
      yLowerLeftCorner(element, "yLowerLeftCorner", false)
{
}

pcrxml::RasterSpace::RasterSpace() : Element()
{
}

const std::string &pcrxml::RasterSpace::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::RasterSpace::~RasterSpace()
{
  clean();
}

//! clean
void pcrxml::RasterSpace::clean()
{
}

//! copy ctor
pcrxml::RasterSpace::RasterSpace(const RasterSpace &src)
    : pcrxml::Element(src), nrRows(src.nrRows), nrCols(src.nrCols), cellSize(src.cellSize),
      xLowerLeftCorner(src.xLowerLeftCorner), yLowerLeftCorner(src.yLowerLeftCorner)
{
}

//! assignment operator
pcrxml::RasterSpace &pcrxml::RasterSpace::operator=(const RasterSpace &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
  }
  return *this;
}

void pcrxml::RasterSpace::fill(QDomElement el) const
{
  nrRows.addToElement(el, "nrRows");
  nrCols.addToElement(el, "nrCols");
  cellSize.addToElement(el, "cellSize");
  xLowerLeftCorner.addToElement(el, "xLowerLeftCorner");
  yLowerLeftCorner.addToElement(el, "yLowerLeftCorner");
}
