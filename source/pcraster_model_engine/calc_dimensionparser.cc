#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_DIMENSIONPARSER
#include "calc_dimensionparser.h"
#define INCLUDED_CALC_DIMENSIONPARSER
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#define BOOST_SPIRIT_USE_OLD_NAMESPACE
#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#include <boost/spirit/include/classic.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_COM_PARSERS
#include "com_parsers.h"
#define INCLUDED_COM_PARSERS
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.

/*!
  \file
  This file contains the implementation of the DimensionParser class.
*/



//------------------------------------------------------------------------------

namespace calc {

namespace dimensionParser {

struct Symbol
{
  DimensionParser *d_dp;
  Symbol(DimensionParser* dp)
    : d_dp(dp)
  { }

  /*!
   * \todo
   *   create Exception type that rememver first, as pointer
   *   to offending position in case of error
   */
  template<typename IteratorType>
  void operator()(IteratorType first, IteratorType last) const {
    if (std::string(first,last)=="X")
      throw com::Exception("XXXXXXXXX");
    d_dp->add(std::string(first,last));
  }
};

struct Power
{
  DimensionParser *d_dp;
  Power(DimensionParser* dp):
    d_dp(dp)
  { }
  template<typename IteratorType>
  void operator()(IteratorType first, IteratorType last) const {
    d_dp->set(com::strToInt(std::string(first,last)));
  }
};


} // namespace dimensionParser
} // namespace calc



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSIONPARSER MEMBERS
//------------------------------------------------------------------------------

void calc::DimensionParser::throwUnknown(const std::string& unknown)
{
    std::ostringstream s;
    s << "'" << unknown << "'" << +" is not a recognized unit dimension";
    throw com::Exception(s.str());
}


//------------------------------------------------------------------------------
// DEFINITION OF DIMENSIONPARSER MEMBERS
//------------------------------------------------------------------------------

calc::DimensionParser::DimensionParser(const std::string& str)
{
  com::PureAlphabeticNameGrammar symbol;
  com::NumberGrammar             number;

  std::string line(str);

  typedef boost::spirit::position_iterator<std::string::iterator> IteratorType;
  IteratorType begin(line.begin(), line.end(), "");
  IteratorType end;

  boost::spirit::parse_info<IteratorType> info =
         boost::spirit::parse<IteratorType>(begin, end,
     *(symbol[dimensionParser::Symbol(this)]      >>
       *(boost::spirit::space_p) >>
       !boost::spirit::ch_p(',') >>
       *(boost::spirit::space_p) >>
       !(number[dimensionParser::Power(this)])    >> *(boost::spirit::space_p)
      )
  );
  if (!info.full)
    throwUnknown(std::string(1,*(info.stop)));
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::DimensionParser::DimensionParser(DimensionParser const& rhs)

  : Base(rhs)

{
}
*/



calc::DimensionParser::~DimensionParser()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::DimensionParser& calc::DimensionParser::operator=(DimensionParser const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! add a symbol
void calc::DimensionParser::add(const std::string& symbol)
{
  d_symbols.push_back(SymbolPower(symbol));
}

//! set power of last added symbol
void calc::DimensionParser::set(int power)
{
  PRECOND(!d_symbols.empty());
  d_symbols.back().d_power=power;
}

const std::vector<calc::DimensionParser::SymbolPower>&
calc::DimensionParser::symbols() const
{
  return d_symbols;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



