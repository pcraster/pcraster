#ifndef INCLUDED_DEV_UTILS
#include "dev_Utils.h"
#define INCLUDED_DEV_UTILS
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif

#ifdef _WIN32
  #ifndef INCLUDED_WINDOWS
  #include <windows.h>
  #define INCLUDED_WINDOWS
  #endif
#endif

// Project headers.

// Module headers.



namespace dev {

// Code that is private to this module.
namespace detail {

} // namespace detail



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Normally a stringstream object is ended by adding std::ends to the stream.
  This is not necessary with MSVS-8.0 and makes testing even more difficult:
  \code
  stream << "bla" << std::ends;
  stream.str()
  \endcode
  does not equal
  \code
  std::string("bla");
  \endcode

  Using this function:
  \code
  stream << "bla";
  end(stream);
  stream.str()
  \endcode
  does equal
  \code
  std::string("bla");
  \endcode
*/
// void end(std::ostringstream& stream)
// {
// #if _MSC_VER // != 1400
//   stream << std::ends;
// #endif
// }



bool environmentVariableSet(
         std::string const& name)
{
  assert(!name.empty());

  return std::getenv(name.c_str()) != 0;
}



//! Returns the value of the environment variable \a name.
/*!
  \param     name Name of environment variable.
  \return    Value of environment variable \a name.
  \warning   An empty string is returned when the variable is not set or if
             the variable's value is an empty string. Use
             environmentVariableSet(std::string const&) to discern
             between these two cases.
*/
std::string environmentVariable(
         std::string const& name)
{
  assert(!name.empty());

  std::string result;

  if(char const* const value = std::getenv(name.c_str())) {
    result = value;
  }

  return result;
}



void setEnvironmentVariable(
         std::string const& name,
         std::string const& value)
{
  assert(!name.empty());

#ifndef _WIN32
  ::setenv(name.c_str(), value.c_str(), 1);
#else
  BOOL result = SetEnvironmentVariable(name.c_str(), value.c_str());
  assert(result != 0);
#endif
}



void unsetEnvironmentVariable(
         std::string const& name)
{
  assert(!name.empty());

#ifndef _WIN32
  ::unsetenv(name.c_str());
#else
  BOOL result = SetEnvironmentVariable(name.c_str(), 0);
  assert(result != 0);
#endif
}

} // namespace dev

