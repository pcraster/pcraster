/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_input.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::Input::d_elementName("Input");

//! ctor
pcrxml::Input::Input(const QDomElement &element)
    : Element(element, d_elementName), flipZ(element, "flipZ", true),
      samplingInterval(element, "samplingInterval", true), migrDirection(element, "migrDirection", false)
{
  try {
    ChildElementVisitor v(element);

    // optional element
    if (v.currentChildEq("InputLodings"))
      inputLodings = new InputLodings(v.processChild());
    // optional element
    if (v.currentChildEq("InputPoints"))
      inputPoints = new InputPoints(v.processChild());
    // + repeated element
    v.checkRequiredChild("InputFile");
    while (v.currentChildEq("InputFile"))
      inputFile.push_back(new InputFile(v.processChild()));
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::Input::Input() : Element()

{
}

const std::string &pcrxml::Input::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::Input::~Input()
{
  clean();
}

//! clean
void pcrxml::Input::clean()
{
  delete inputLodings;
  inputLodings = nullptr;
  delete inputPoints;
  inputPoints = nullptr;
  for (auto &i : inputFile)
    delete i;
  inputFile.clear();
}

//! copy ctor
pcrxml::Input::Input(const Input &src)
    : pcrxml::Element(src), flipZ(src.flipZ), samplingInterval(src.samplingInterval),
      migrDirection(src.migrDirection)
{
  inputLodings = (src.inputLodings) ? new InputLodings(*(src.inputLodings)) : nullptr;
  inputPoints = (src.inputPoints) ? new InputPoints(*(src.inputPoints)) : nullptr;
  for (auto i : src.inputFile)
    inputFile.push_back(new InputFile(*i));
}

//! assignment operator
pcrxml::Input &pcrxml::Input::operator=(const Input &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    inputLodings = (src.inputLodings) ? new InputLodings(*(src.inputLodings)) : nullptr;
    inputPoints = (src.inputPoints) ? new InputPoints(*(src.inputPoints)) : nullptr;
    for (auto i : src.inputFile)
      inputFile.push_back(new InputFile(*i));
  }
  return *this;
}

void pcrxml::Input::fill(QDomElement el) const
{
  flipZ.addToElement(el, "flipZ");
  samplingInterval.addToElement(el, "samplingInterval");
  migrDirection.addToElement(el, "migrDirection");
  if (inputLodings)
    inputLodings->appendTo(el);
  if (inputPoints)
    inputPoints->appendTo(el);
  for (auto i : inputFile)
    i->appendTo(el);
}
