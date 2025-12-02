/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_userinterfacedescription.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::UserInterfaceDescription::d_elementName("UserInterfaceDescription");

//! ctor
pcrxml::UserInterfaceDescription::UserInterfaceDescription(const QDomElement &element)
    : Element(element, d_elementName), scriptFile(element, "scriptFile", true),
      panelType(element, "panelType", true)
{
  try {
    ChildElementVisitor v(element);

    // * repeated element
    while (v.currentChildEq("ParameterPanel"))
      parameterPanel.push_back(new ParameterPanel(v.processChild()));
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::UserInterfaceDescription::UserInterfaceDescription() : Element()
{
}

const std::string &pcrxml::UserInterfaceDescription::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::UserInterfaceDescription::~UserInterfaceDescription()
{
  clean();
}

//! clean
void pcrxml::UserInterfaceDescription::clean()
{
  for (auto &i : parameterPanel)
    delete i;
  parameterPanel.clear();
}

//! copy ctor
pcrxml::UserInterfaceDescription::UserInterfaceDescription(const UserInterfaceDescription &src)
    : pcrxml::Element(src), scriptFile(src.scriptFile), panelType(src.panelType)
{
  for (auto i : src.parameterPanel)
    parameterPanel.push_back(new ParameterPanel(*i));
}

//! assignment operator
pcrxml::UserInterfaceDescription &
pcrxml::UserInterfaceDescription::operator=(const UserInterfaceDescription &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    for (auto i : src.parameterPanel)
      parameterPanel.push_back(new ParameterPanel(*i));
  }
  return *this;
}

void pcrxml::UserInterfaceDescription::fill(QDomElement el) const
{
  scriptFile.addToElement(el, "scriptFile");
  panelType.addToElement(el, "panelType");
  for (auto i : parameterPanel)
    i->appendTo(el);
}
