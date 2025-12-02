/*!
\note
Do not edit, generated from libs/pcrxml/generate.py
*/
#include "stddefx.h"
#include "pcrgenxml_datasetconfiguration.h"
#include "pcrxml_childelementvisitor.h"
#include <qdom.h>

const std::string pcrxml::DataSetConfiguration::d_elementName("DataSetConfiguration");

//! ctor
pcrxml::DataSetConfiguration::DataSetConfiguration(const QDomElement &element)
    : Element(element, d_elementName), version(element, "version", true)
{
  try {
    ChildElementVisitor v(element);

    // required element
    v.checkRequiredChild("Input");
    input = new Input(v.processChild());
    // required element
    v.checkRequiredChild("Compute");
    compute = new Compute(v.processChild());
    // optional element
    if (v.currentChildEq("Interpolate"))
      interpolate = new Interpolate(v.processChild());
    // optional element
    if (v.currentChildEq("Lodings"))
      lodings = new Lodings(v.processChild());
  } catch (...) {
    clean();
    throw;
  }
}

pcrxml::DataSetConfiguration::DataSetConfiguration() : Element()

{
}

const std::string &pcrxml::DataSetConfiguration::elementName() const
{
  return d_elementName;
}

//! dtor
pcrxml::DataSetConfiguration::~DataSetConfiguration()
{
  clean();
}

//! clean
void pcrxml::DataSetConfiguration::clean()
{
  delete input;
  input = nullptr;
  delete compute;
  compute = nullptr;
  delete interpolate;
  interpolate = nullptr;
  delete lodings;
  lodings = nullptr;
}

//! copy ctor
pcrxml::DataSetConfiguration::DataSetConfiguration(const DataSetConfiguration &src)
    : pcrxml::Element(src), version(src.version)
{
  input = new Input(*(src.input));
  compute = new Compute(*(src.compute));
  interpolate = (src.interpolate) ? new Interpolate(*(src.interpolate)) : nullptr;
  lodings = (src.lodings) ? new Lodings(*(src.lodings)) : nullptr;
}

//! assignment operator
pcrxml::DataSetConfiguration &pcrxml::DataSetConfiguration::operator=(const DataSetConfiguration &src)
{
  if (this != &src) {
    clean();
    PRECOND(false);
    input = new Input(*(src.input));
    compute = new Compute(*(src.compute));
    interpolate = (src.interpolate) ? new Interpolate(*(src.interpolate)) : nullptr;
    lodings = (src.lodings) ? new Lodings(*(src.lodings)) : nullptr;
  }
  return *this;
}

void pcrxml::DataSetConfiguration::fill(QDomElement el) const
{
  version.addToElement(el, "version");
  if (input)
    input->appendTo(el);
  if (compute)
    compute->appendTo(el);
  if (interpolate)
    interpolate->appendTo(el);
  if (lodings)
    lodings->appendTo(el);
}
