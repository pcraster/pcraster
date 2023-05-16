#include "dev_FilesystemUtils.h"
#include "dev_Configure.h"
#include <boost/algorithm/string.hpp>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>


namespace dev {

// Code that is private to this module.
namespace detail {

bool accessible(std::filesystem::path const& path) {
  try{
    std::filesystem::file_status s = std::filesystem::status(path);
    if((s.permissions() & (std::filesystem::perms::owner_exec | std::filesystem::perms::group_exec | std::filesystem::perms::others_exec)) != std::filesystem::perms::none){
      return true;
    }
  }
  catch(std::filesystem::filesystem_error const& e){
    // permission denied
    return false;
  }
  return false;
}

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
std::filesystem::path pathToExecutable(
         std::filesystem::path const& path)
{
  std::filesystem::path result;

  if(std::filesystem::exists(path)) {
    result = std::filesystem::absolute(path).parent_path();
  }
  else if(!path.is_absolute()) {
    std::string pathVariable(std::getenv("PATH"));
    std::vector<std::string> directoryNames;
    boost::split(directoryNames, pathVariable, boost::is_any_of(":;"));

#ifdef _WIN32
    // http://support.microsoft.com/kb/35284
    std::vector<std::string> extensions;
    extensions.push_back(".com");
    extensions.push_back(".exe");
    extensions.push_back(".bat");
#endif

    // Loop over all directories.
    for(std::string const& directoryName : directoryNames) {
      if(detail::accessible(directoryName) && std::filesystem::exists(directoryName / path)) {
        result = std::filesystem::absolute(directoryName);
        break;
      }

#ifdef _WIN32
      bool found = false;
      std::filesystem::path pathWithExtension(path);

      // Loop over all extensions.
      for(std::string const& extension : extensions) {
        pathWithExtension.replace_extension(extension);

        if(std::filesystem::exists(directoryName / pathWithExtension)) {
          result = std::filesystem::absolute(directoryName);
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



std::filesystem::path pathToPythonExtension(
         std::filesystem::path const& path)
{
  std::filesystem::path result;

  if(std::filesystem::exists(path)) {
    result = std::filesystem::absolute(path).parent_path();
  }
  else if(!path.is_absolute()) {
    std::string pathVariable(std::getenv("PYTHONPATH"));
    std::vector<std::string> directoryNames;
    boost::split(directoryNames, pathVariable, boost::is_any_of(":;"));

    for(std::string const& directoryName : directoryNames) {
      if(detail::accessible(directoryName) && std::filesystem::exists(directoryName / path)) {
        result = std::filesystem::absolute(directoryName);
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
std::filesystem::path prefix(
         std::filesystem::path const& path)
{
  // /usr/bin/ls -> /usr
  std::filesystem::path result(pathToExecutable(path));
  result.remove_filename().remove_filename();

  return result;
}

} // namespace dev

