/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_fileinput.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::FileInput::d_elementName("FileInput");

//! ctor
pcrxml::FileInput::FileInput(const QDomElement &element)
    : Element(element, d_elementName), canChooseOtherFiles(element, "canChooseOtherFiles", true)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("Map")) {
      map = new Map(v.processChild());
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
    // * repeated element
    while (v.currentChildEq("Data")) {
      data.push_back(new Data(v.processChild()));
    }
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::FileInput::FileInput() : Element()

{
}

const std::string &pcrxml::FileInput::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::FileInput::~FileInput()
{
  clean();
}

//! clean
void pcrxml::FileInput::clean()
{
  delete map;
  map = nullptr;
  delete stack;
  stack = nullptr;
  delete timeSeries;
  timeSeries = nullptr;
  delete table;
  table = nullptr;
  for (auto &i : data) {
    delete i;
  }
  data.clear();
}

//! copy ctor
pcrxml::FileInput::FileInput(const FileInput &src)
    : pcrxml::Element(src), canChooseOtherFiles(src.canChooseOtherFiles)
{
  map = (src.map) ? new Map(*(src.map)) : nullptr;
  stack = (src.stack) ? new Stack(*(src.stack)) : nullptr;
  timeSeries = (src.timeSeries) ? new TimeSeries(*(src.timeSeries)) : nullptr;
  table = (src.table) ? new Table(*(src.table)) : nullptr;
  for (auto i : src.data) {
    data.push_back(new Data(*i));
  }
}

//! assignment operator
pcrxml::FileInput &pcrxml::FileInput::operator=(const FileInput &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    map = (src.map) ? new Map(*(src.map)) : nullptr;
    stack = (src.stack) ? new Stack(*(src.stack)) : nullptr;
    timeSeries = (src.timeSeries) ? new TimeSeries(*(src.timeSeries)) : nullptr;
    table = (src.table) ? new Table(*(src.table)) : nullptr;
    for (auto i : src.data) {
      data.push_back(new Data(*i));
    }
  }
  return *this;
}

void pcrxml::FileInput::fill(QDomElement el) const
{
  canChooseOtherFiles.addToElement(el, "canChooseOtherFiles");
  if (map) {
    map->appendTo(el);
  }
  if (stack) {
    stack->appendTo(el);
  }
  if (timeSeries) {
    timeSeries->appendTo(el);
  }
  if (table) {
    table->appendTo(el);
  }
  for (auto i : data) {
    i->appendTo(el);
  }
}
