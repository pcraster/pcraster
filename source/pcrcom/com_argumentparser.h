#ifndef INCLUDED_COM_ARGUMENTPARSER
#define INCLUDED_COM_ARGUMENTPARSER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
// /home/PcrTeamExtern/trunk/gcc-4/boost-1.44.0/include/boost/spirit.hpp:18:4
//   warning: #warning "This header is deprecated. Please use: 

#define BOOST_SPIRIT_USE_OLD_NAMESPACE
#ifndef INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#include <boost/spirit/include/classic.hpp>
#define INCLUDED_BOOST_SPIRIT_INCLUDE_CLASSIC
#endif

#ifndef INCLUDED_BOOST_ALGORITHM_STRING_TRIM
#include <boost/algorithm/string/trim.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING_TRIM
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // ArgumentParser declarations.
}



namespace com {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<class ValueType>
class ArgumentParser
{

  friend class ArgumentParserTest;

private:

  size_t           d_length;

  bool             d_full;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ArgumentParser      ()
    : d_length(0), d_full(false)
  {
  }

  /* virtual */    ~ArgumentParser     ()
  {
  }

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  ValueType        parse               (char const* const string);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           length              () const
  {
    return d_length;
  }

  bool             full                () const
  {
    return d_full;
  }

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

template<>
inline std::string ArgumentParser<std::string>::parse(char const* const string)
{
  PRECOND(std::strlen(string) > 0);

  std::string value(string);
  d_length = value.length();
  d_full = d_length && d_length == std::strlen(string);

  boost::trim(value);
  return value;
}

template<>
inline size_t ArgumentParser<size_t>::parse(char const* const string)
{
  PRECOND(std::strlen(string) > 0);

  size_t value(0);
  boost::spirit::rule<> rule =
         boost::spirit::uint_p[boost::spirit::assign(value)];
  d_length = std::distance(string, boost::spirit::parse(string, rule).stop);
  d_full = d_length && d_length == std::strlen(string);

  return value;
}

template<>
inline int ArgumentParser<int>::parse(char const* const string)
{
  PRECOND(std::strlen(string) > 0);

  int value(0);
  boost::spirit::rule<> rule =
         boost::spirit::int_p[boost::spirit::assign(value)];
  d_length = std::distance(string, boost::spirit::parse(string, rule).stop);
  d_full = d_length && d_length == std::strlen(string);

  return value;
}

template<>
inline double ArgumentParser<double>::parse(char const* const string)
{
  PRECOND(std::strlen(string) > 0);

  double value(0);
  boost::spirit::rule<> rule =
         boost::spirit::real_p[boost::spirit::assign(value)];
  d_length = std::distance(string, boost::spirit::parse(string, rule).stop);
  d_full = d_length && d_length == std::strlen(string);

  return value;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
