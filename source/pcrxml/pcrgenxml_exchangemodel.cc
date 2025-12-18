/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_exchangemodel.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::ExchangeModel::d_elementName("exchangeModel");

//! ctor
pcrxml::ExchangeModel::ExchangeModel(const QDomElement &element)
    : Element(element, d_elementName), id(element, "id", false),
      ioStrategy(element, "ioStrategy", false), description(element, "description", false)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("integerTimer")) {
      integerTimer = new IntegerTimer(v.processChild());
    }
    // optional element
    if (v.currentChildEq("areaMapDTD")) {
      areaMapDTD = new AreaMapDTD(v.processChild());
    }
    // * repeated element
    while (v.currentChildEq("exchangeItem")) {
      exchangeItem.push_back(new ExchangeItem(v.processChild()));
    }
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::ExchangeModel::ExchangeModel() : Element()

{
}

const std::string &pcrxml::ExchangeModel::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::ExchangeModel::~ExchangeModel()
{
  clean();
}

//! clean
void pcrxml::ExchangeModel::clean()
{
  delete integerTimer;
  integerTimer = nullptr;
  delete areaMapDTD;
  areaMapDTD = nullptr;
  for (auto &i : exchangeItem) {
    delete i;
  }
  exchangeItem.clear();
}

//! copy ctor
pcrxml::ExchangeModel::ExchangeModel(const ExchangeModel &src)
    : pcrxml::Element(src), id(src.id), ioStrategy(src.ioStrategy), description(src.description)
{
  integerTimer = ((src.integerTimer) != nullptr) ? new IntegerTimer(*(src.integerTimer)) : nullptr;
  areaMapDTD = ((src.areaMapDTD) != nullptr) ? new AreaMapDTD(*(src.areaMapDTD)) : nullptr;
  for (auto i : src.exchangeItem) {
    exchangeItem.push_back(new ExchangeItem(*i));
  }
}

//! assignment operator
pcrxml::ExchangeModel &pcrxml::ExchangeModel::operator=(const ExchangeModel &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    integerTimer = ((src.integerTimer) != nullptr) ? new IntegerTimer(*(src.integerTimer)) : nullptr;
    areaMapDTD = ((src.areaMapDTD) != nullptr) ? new AreaMapDTD(*(src.areaMapDTD)) : nullptr;
    for (auto i : src.exchangeItem) {
      exchangeItem.push_back(new ExchangeItem(*i));
    }
  }
  return *this;
}

void pcrxml::ExchangeModel::fill(QDomElement el) const
{
  id.addToElement(el, "id");
  ioStrategy.addToElement(el, "ioStrategy");
  description.addToElement(el, "description");
  if (integerTimer != nullptr) {
    integerTimer->appendTo(el);
  }
  if (areaMapDTD != nullptr) {
    areaMapDTD->appendTo(el);
  }
  for (auto i : exchangeItem) {
    i->appendTo(el);
  }
}
