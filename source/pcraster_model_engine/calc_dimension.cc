#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DIMENSION
#include "calc_dimension.h"
#define INCLUDED_CALC_DIMENSION
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.
// Module headers.
#ifndef INCLUDED_CALC_DIMENSIONPARSER
#include "calc_dimensionparser.h"
#define INCLUDED_CALC_DIMENSIONPARSER
#endif



/*!
  \file
  This file contains the implementation of the Dimension class.
*/



//------------------------------------------------------------------------------

namespace calc {

} // namespace calc




//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DIMENSION MEMBERS
//------------------------------------------------------------------------------


//! default ctor, init all to 0
calc::Dimension::Dimension():
  std::vector<double>(NumBaseQuanties,0)
{
}

//! parse from string
calc::Dimension::Dimension(const std::string& str):
  std::vector<double>(NumBaseQuanties,0)
{
  typedef std::map<std::string,Base> Id2Base;
  Id2Base id2base;

  id2base.insert(std::make_pair("m"    ,Length));
  id2base.insert(std::make_pair("kg"   ,Mass));
  id2base.insert(std::make_pair("s"    ,Time));
  id2base.insert(std::make_pair("A"    ,ElectricCurrent));
  id2base.insert(std::make_pair("K"    ,Temperature));
  id2base.insert(std::make_pair("mol"  ,AmountOfSubstance));
  id2base.insert(std::make_pair("cd"   ,LuminousIntensity));
  id2base.insert(std::make_pair("E"    ,Currency));

  DimensionParser dp(str);
  for(size_t i=0; i < dp.symbols().size(); ++i) {
    Id2Base::const_iterator it=id2base.find(dp.symbols()[i].d_symbol);
    if (it != id2base.end()) {
      PRECOND(it->second < static_cast<int>(size()));
      at(it->second)=dp.symbols()[i].d_power;
    } else
      dp.throwUnknown(dp.symbols()[i].d_symbol);
  }
}


//! Copy constructor.
calc::Dimension::Dimension(Dimension const& rhs)
  : std::vector<double>(rhs)
{
  POSTCOND(size()==NumBaseQuanties);
}



calc::Dimension::~Dimension()
{
}



//! Assignment operator.
calc::Dimension& calc::Dimension::operator=(Dimension const& rhs)
{
  PRECOND(rhs.size()==NumBaseQuanties);
  if (this != &rhs) {
   for(size_t i=0; i != size(); ++i)
    at(i)=rhs[i];
  }
  return *this;
}

//! is this dimensionless? in other words all are 0
bool calc::Dimension::none() const
{
  for(const_iterator i=begin(); i != end(); ++i)
    if (*i!=0)
      return false;
  return true;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
