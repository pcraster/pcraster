#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_ALGORITHM_STRING
#include <boost/algorithm/string.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_DEV_CONFIGURE
#include "dev_Configure.h"
#define INCLUDED_DEV_CONFIGURE
#endif

#ifndef INCLUDED_DEV_FILESYSTEMUTILS
#include "dev_FilesystemUtils.h"
#define INCLUDED_DEV_FILESYSTEMUTILS
#endif

// External headers.

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
  \todo      Handle extensions, platform dependent, bat, com, exe, precedence.
  \todo      Write tests.
*/
boost::filesystem::path pathToExecutable(
         boost::filesystem::path const& path)
{
  boost::filesystem::path result;

  if(boost::filesystem::exists(path)) {
    result = boost::filesystem::system_complete(path).parent_path();
  }
  else if(!path.is_absolute()) {
    std::string pathVariable(std::getenv("PATH"));
    std::vector<std::string> directoryNames;
    boost::split(directoryNames, pathVariable, boost::is_any_of(
         DEV_PATH_VARIABLE_SEPARATOR));

#ifdef _WIN32
    // http://support.microsoft.com/kb/35284
    std::vector<std::string> extensions;
    extensions.push_back(".com");
    extensions.push_back(".exe");
    extensions.push_back(".bat");
#endif

    // Loop over all directories.
    BOOST_FOREACH(std::string const& directoryName, directoryNames) {
      if(boost::filesystem::exists(directoryName / path)) {
        result = boost::filesystem::system_complete(directoryName);
        break;
      }

#ifdef _WIN32
      bool found = false;
      boost::filesystem::path pathWithExtension(path);

      // Loop over all extensions.
      BOOST_FOREACH(std::string const& extension, extensions) {
        pathWithExtension.replace_extension(extension);

        if(boost::filesystem::exists(directoryName / pathWithExtension)) {
          result = boost::filesystem::system_complete(directoryName);
          found = true;
          break;
        }
      }

      if(found) {
        break;
      }
#endif
    }
  }

  return result;
}



boost::filesystem::path pathToPythonExtension(
         boost::filesystem::path const& path)
{
  boost::filesystem::path result;

  if(boost::filesystem::exists(path)) {
    result = boost::filesystem::system_complete(path).parent_path();
  }
  else if(!path.is_absolute()) {
    std::string pathVariable(std::getenv("PYTHONPATH"));
    std::vector<std::string> directoryNames;
    boost::split(directoryNames, pathVariable, boost::is_any_of(":"));

    BOOST_FOREACH(std::string const& directoryName, directoryNames) {
      if(boost::filesystem::exists(directoryName / path)) {
        result = boost::filesystem::system_complete(directoryName);
      }
    }
  }

  return result;
}



//! Returns the package install prefix of an executable pointed to by \a path.
/*!
  \param     path Path to executable.
  \return    Path to install prefix.
  \warning   \a path must exist.
*/
boost::filesystem::path prefix(
         boost::filesystem::path const& path)
{
  // /usr/bin/ls -> /usr
  boost::filesystem::path result(pathToExecutable(path));
  result.remove_filename().remove_filename();

  return result;
}

} // namespace dev

