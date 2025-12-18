/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_data.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Data::d_elementName("Data");

//! ctor
pcrxml::Data::Data(const QDomElement &element)
    : Element(element, d_elementName), name(element, "name", true),
      description(element, "description", false), externalFileName(element, "externalFileName", false),
      ioType(element, "ioType", true)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("Map")) {
      map = new Map(v.processChild());
    }
    // optional element
    if (v.currentChildEq("NonSpatial")) {
      nonSpatial = new NonSpatial(v.processChild());
    }
    // optional element
    if (v.currentChildEq("Stack")) {
      stack = new Stack(v.processChild());
    }
    // optional element
    if (v.currentChildEq("TimeSeries")) {
      timeSeries = new TimeSeries(v.processChild());
    }
    // optional element
    if (v.currentChildEq("Table")) {
      table = new Table(v.processChild());
    }
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::Data::Data() : Element()

{
}

const std::string &pcrxml::Data::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::Data::~Data()
{
  clean();
}

//! clean
void pcrxml::Data::clean()
{
  delete map;
  map = nullptr;
  delete nonSpatial;
  nonSpatial = nullptr;
  delete stack;
  stack = nullptr;
  delete timeSeries;
  timeSeries = nullptr;
  delete table;
  table = nullptr;
}

//! copy ctor
pcrxml::Data::Data(const Data &src)
    : pcrxml::Element(src), name(src.name), description(src.description),
      externalFileName(src.externalFileName), ioType(src.ioType)
{
  map = ((src.map) != nullptr) ? new Map(*(src.map)) : nullptr;
  nonSpatial = ((src.nonSpatial) != nullptr) ? new NonSpatial(*(src.nonSpatial)) : nullptr;
  stack = ((src.stack) != nullptr) ? new Stack(*(src.stack)) : nullptr;
  timeSeries = ((src.timeSeries) != nullptr) ? new TimeSeries(*(src.timeSeries)) : nullptr;
  table = ((src.table) != nullptr) ? new Table(*(src.table)) : nullptr;
}

//! assignment operator
pcrxml::Data &pcrxml::Data::operator=(const Data &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    map = ((src.map) != nullptr) ? new Map(*(src.map)) : nullptr;
    nonSpatial = ((src.nonSpatial) != nullptr) ? new NonSpatial(*(src.nonSpatial)) : nullptr;
    stack = ((src.stack) != nullptr) ? new Stack(*(src.stack)) : nullptr;
    timeSeries = ((src.timeSeries) != nullptr) ? new TimeSeries(*(src.timeSeries)) : nullptr;
    table = ((src.table) != nullptr) ? new Table(*(src.table)) : nullptr;
  }
  return *this;
}

void pcrxml::Data::fill(QDomElement el) const
{
  name.addToElement(el, "name");
  description.addToElement(el, "description");
  externalFileName.addToElement(el, "externalFileName");
  ioType.addToElement(el, "ioType");
  if (map != nullptr) {
    map->appendTo(el);
  }
  if (nonSpatial != nullptr) {
    nonSpatial->appendTo(el);
  }
  if (stack != nullptr) {
    stack->appendTo(el);
  }
  if (timeSeries != nullptr) {
    timeSeries->appendTo(el);
  }
  if (table != nullptr) {
    table->appendTo(el);
  }
}
