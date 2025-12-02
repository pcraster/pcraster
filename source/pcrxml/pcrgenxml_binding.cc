/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_binding.h"
#include <qdom.h>
#include "pcrxml_childelementvisitor.h"

const std::string pcrxml::Binding::d_elementName("Binding");

//! ctor
pcrxml::Binding::Binding(const QDomElement &element)
    : Element(element, d_elementName), parameter(element, "parameter", true),
      value(element, "value", true)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("Map"))
      map = new Map(v.processChild());
    // optional element
    if (v.currentChildEq("NonSpatial"))
      nonSpatial = new NonSpatial(v.processChild());
    // optional element
    if (v.currentChildEq("Stack"))
      stack = new Stack(v.processChild());
    // optional element
    if (v.currentChildEq("TimeSeries"))
      timeSeries = new TimeSeries(v.processChild());
    // optional element
    if (v.currentChildEq("Table"))
      table = new Table(v.processChild());
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::Binding::Binding() : Element()

{
}

const std::string &pcrxml::Binding::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::Binding::~Binding()
{
  clean();
}

//! clean
void pcrxml::Binding::clean()
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
pcrxml::Binding::Binding(const Binding &src)
    : pcrxml::Element(src), parameter(src.parameter), value(src.value)
{
  map = (src.map) ? new Map(*(src.map)) : nullptr;
  nonSpatial = (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)) : nullptr;
  stack = (src.stack) ? new Stack(*(src.stack)) : nullptr;
  timeSeries = (src.timeSeries) ? new TimeSeries(*(src.timeSeries)) : nullptr;
  table = (src.table) ? new Table(*(src.table)) : nullptr;
}

//! assignment operator
pcrxml::Binding &pcrxml::Binding::operator=(const Binding &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    map = (src.map) ? new Map(*(src.map)) : nullptr;
    nonSpatial = (src.nonSpatial) ? new NonSpatial(*(src.nonSpatial)) : nullptr;
    stack = (src.stack) ? new Stack(*(src.stack)) : nullptr;
    timeSeries = (src.timeSeries) ? new TimeSeries(*(src.timeSeries)) : nullptr;
    table = (src.table) ? new Table(*(src.table)) : nullptr;
  }
  return *this;
}

void pcrxml::Binding::fill(QDomElement el) const
{
  parameter.addToElement(el, "parameter");
  value.addToElement(el, "value");
  if (map)
    map->appendTo(el);
  if (nonSpatial)
    nonSpatial->appendTo(el);
  if (stack)
    stack->appendTo(el);
  if (timeSeries)
    timeSeries->appendTo(el);
  if (table)
    table->appendTo(el);
}
