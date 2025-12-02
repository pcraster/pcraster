/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_parameterpanel.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::ParameterPanel::d_elementName("ParameterPanel");

//! ctor
pcrxml::ParameterPanel::ParameterPanel(const QDomElement &element)
    : Element(element, d_elementName), name(element, "name", true), show(element, "show", false)
{
  try {
    ChildElementVisitor v(element);

    // * repeated element
    while (v.currentChildEq("ParameterItem")) {
      parameterItem.push_back(new ParameterItem(v.processChild()));
    }
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::ParameterPanel::ParameterPanel() : Element()
{
}

const std::string &pcrxml::ParameterPanel::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::ParameterPanel::~ParameterPanel()
{
  clean();
}

//! clean
void pcrxml::ParameterPanel::clean()
{
  for (auto &i : parameterItem) {
    delete i;
  }
  parameterItem.clear();
}

//! copy ctor
pcrxml::ParameterPanel::ParameterPanel(const ParameterPanel &src)
    : pcrxml::Element(src), name(src.name), show(src.show)
{
  for (auto i : src.parameterItem) {
    parameterItem.push_back(new ParameterItem(*i));
  }
}

//! assignment operator
pcrxml::ParameterPanel &pcrxml::ParameterPanel::operator=(const ParameterPanel &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    for (auto i : src.parameterItem) {
      parameterItem.push_back(new ParameterItem(*i));
    }
  }
  return *this;
}

void pcrxml::ParameterPanel::fill(QDomElement el) const
{
  name.addToElement(el, "name");
  show.addToElement(el, "show");
  for (auto i : parameterItem) {
    i->appendTo(el);
  }
}
