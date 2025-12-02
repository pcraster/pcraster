/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_dataobject.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::DataObject::d_elementName("DataObject");

//! ctor
pcrxml::DataObject::DataObject(const QDomElement &element) : Element(element, d_elementName)
{
  try {
    ChildElementVisitor v(element);

    // required element
    v.checkRequiredChild("Cursor");
    cursor = new Cursor(v.processChild());
    // * repeated element
    while (v.currentChildEq("Data"))
      data.push_back(new Data(v.processChild()));
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::DataObject::DataObject() : Element()

{
}

const std::string &pcrxml::DataObject::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::DataObject::~DataObject()
{
  clean();
}

//! clean
void pcrxml::DataObject::clean()
{
  delete cursor;
  cursor = nullptr;
  for (auto &i : data)
    delete i;
  data.clear();
}

//! copy ctor
pcrxml::DataObject::DataObject(const DataObject &src) : pcrxml::Element(src)
{
  cursor = new Cursor(*(src.cursor));
  for (auto i : src.data)
    data.push_back(new Data(*i));
}

//! assignment operator
pcrxml::DataObject &pcrxml::DataObject::operator=(const DataObject &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    cursor = new Cursor(*(src.cursor));
    for (auto i : src.data)
      data.push_back(new Data(*i));
  }
  return *this;
}

void pcrxml::DataObject::fill(QDomElement el) const
{
  if (cursor)
    cursor->appendTo(el);
  for (auto i : data)
    i->appendTo(el);
}
