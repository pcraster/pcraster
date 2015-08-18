#include "stddefx.h"


#ifndef INCLUDED_CALC_PARAMETER
#include "calc_parameter.h"
#define INCLUDED_CALC_PARAMETER
#endif

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

#ifndef INCLUDED_CALC_ISCRIPT
#include "calc_iscript.h"
#define INCLUDED_CALC_ISCRIPT
#endif

//! ctor
calc::Parameter::Parameter(const calc::BindedSymbol& name,bool constant):
  calc::UserSymbol(name), d_constantBinding(constant)
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

std::string  calc::Parameter::inputFilePath(const std::string& fileName) const
{
  return scriptConst().inputFilePath(fileName);
}
std::string  calc::Parameter::outputFilePath(const std::string& fileName) const
{
  return scriptConst().outputFilePath(fileName);
}

//! print some info
void calc::Parameter::printSpecific(calc::InfoScript& i) const
{
  i.stream() << "Constant Binding: " << (isConstantBinding() ? "YES" : "NO");
}
