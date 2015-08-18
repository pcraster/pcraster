#ifndef INCLUDED_DAL_FEATUREPATH
#include "dal_FeaturePath.h"
#define INCLUDED_DAL_FEATUREPATH
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the FeaturePath class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FEATUREPATH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FEATUREPATH MEMBERS
//------------------------------------------------------------------------------

FeaturePath::FeaturePath()

  : _isValid(false)

{
}



FeaturePath::FeaturePath(
         FeaturePath const& rhs)

  : _isValid(rhs._isValid),
    _source(rhs._source),
    _layer(rhs._layer),
    _attribute(rhs._attribute)

{
}



FeaturePath::FeaturePath(
         std::string const& path,
         ParseStrategy strategy)

  : _isValid(false)

{
  std::string string(path);

  switch(strategy) {
    case WithAttribute: {
      // Path format: source/layer/attribute

      // Determine name of attribute.
      size_t i = string.find_last_of('/');

      if(i == string.size() - 1) {
        // The string ends with a slash, remove it.
        string.erase(i);
        i = string.find_last_of('/');
      }

      if(i != std::string::npos) {
        _attribute = string.substr(i + 1);
        string.erase(i);

        // Determine name of layer.
        i = string.find_last_of('/');

        if(i != std::string::npos) {
          _layer = string.substr(i + 1);
          string.erase(i);
        }

        // Determine name of source.
        _source = string;
      }

      _isValid = !_source.empty() && !_layer.empty();

      break;
    }
    case WithoutAttribute: {
      // Path format: source/layer

      // Determine name of layer.
      size_t i = string.find_last_of('/');

      if(i == string.size() - 1) {
        // The string ends with a slash, remove it.
        string.erase(i);
        i = string.find_last_of('/');
      }

      if(i != std::string::npos) {
        _layer = string.substr(i + 1);
        string.erase(i);
      }

      // Determine name of source.
      _source = string;

      _isValid = !_source.empty() && !_layer.empty();

      break;
    }
  }
}



FeaturePath::~FeaturePath()
{
}



FeaturePath& FeaturePath::operator=(
         FeaturePath const& rhs)
{
  if(this != &rhs) {
    _isValid = rhs._isValid;
    _source = rhs._source;
    _layer = rhs._layer;
    _attribute = rhs._attribute;
  }

  return *this;
}



bool FeaturePath::compare(
         FeaturePath const& rhs) const
{
  return _isValid == rhs._isValid &&
         _source == rhs._source &&
         _layer == rhs._layer &&
         _attribute == rhs._attribute;
}



bool FeaturePath::isValid() const
{
  return _isValid;
}



std::string const& FeaturePath::source() const
{
  assert(isValid());

  return _source;
}



std::string const& FeaturePath::layer() const
{
  assert(isValid());

  return _layer;
}



std::string const& FeaturePath::attribute() const
{
  assert(isValid());

  return _attribute;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool operator==(
         FeaturePath const& lhs,
         FeaturePath const& rhs)
{
  return lhs.compare(rhs);
}



bool operator!=(
         FeaturePath const& lhs,
         FeaturePath const& rhs)
{
  return !lhs.compare(rhs);
}



#ifdef DEBUG
std::ostream& operator<<(
         std::ostream& stream,
         FeaturePath const& path)
{
  stream
    << path._isValid                 << ": "
    << '<' << path._source    << '>' << '/'
    << '<' << path._layer     << '>' << '/'
    << '<' << path._attribute << '>'
    << '\n'
    ;
  return stream;
}
#endif



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

