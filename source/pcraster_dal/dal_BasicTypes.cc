#ifndef INCLUDED_DAL_BASICTYPES
#include "dal_BasicTypes.h"
#define INCLUDED_DAL_BASICTYPES
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BasicTypes class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC BASICTYPES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BASICTYPES MEMBERS
//------------------------------------------------------------------------------


Uint1Type::Uint1Type()
{
}

bool Uint1Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Uint2Type::Uint2Type()
{
}

bool Uint2Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Uint4Type::Uint4Type()
{
}

bool Uint4Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Int1Type::Int1Type()
{
}

bool Int1Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Int2Type::Int2Type()
{
}

bool Int2Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Int4Type::Int4Type()
{
}

bool Int4Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Real4Type::Real4Type()
{
}

bool Real4Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

Real8Type::Real8Type()
{
}

bool Real8Type::canParse(std::string const& string) const
{
  return boost::spirit::classic::parse(string.c_str(), d_parser).full;
}

StringType::StringType()
{
}

bool StringType::canParse(std::string const&) const
{
  return true;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

