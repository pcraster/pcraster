#include "stddefx.h"
#include "calc_parameter.h"
#include "calc_infoscript.h"
#include "calc_iscript.h"

//! ctor
calc::Parameter::Parameter(const calc::BindedSymbol &name, bool constant)
    : calc::UserSymbol(name), d_constantBinding(constant)
{
}

//! dtor
calc::Parameter::~Parameter()
{
}

//! see calc::Parameter::d_constantBinding
bool calc::Parameter::isConstantBinding() const
{
  return d_constantBinding;
}

std::string calc::Parameter::inputFilePath(const std::string &fileName) const
{
  return scriptConst().inputFilePath(fileName);
}

std::string calc::Parameter::outputFilePath(const std::string &fileName) const
{
  return scriptConst().outputFilePath(fileName);
}

//! print some info
void calc::Parameter::printSpecific(calc::InfoScript &i) const
{
  i.stream() << "Constant Binding: " << (isConstantBinding() ? "YES" : "NO");
}
