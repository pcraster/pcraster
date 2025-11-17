#include "dal_Environment.h"

#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "cpl_conv.h"

#include "dev_Utils.h"

#include <algorithm>
#include <iostream>



/*!
  \file
  This file contains the implementation of the Environment class.
*/


namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC ENVIRONMENT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF ENVIRONMENT MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     prefix Prefix of package install location.

  Reads and stores relevant settings from the environment.
*/
Environment::Environment(
         std::filesystem::path const& prefix)
{
  readFormatNames();
  handleGdalData(prefix);
}



//! Destructor.
/*!
*/
Environment::~Environment()
{
}



//! Reads the format names from $PCRASTER_DAL_FORMATS and stores the result.
/*!
*/
void Environment::readFormatNames()
{
  namespace ba = boost::algorithm;

  std::string const formats = dev::environmentVariable("PCRASTER_DAL_FORMATS");

  // Split string at the comma.
  if(!formats.empty()) {
    // Comma separated list of format names.
    std::string names(formats);

    ba::split(_formatNames, names, ba::is_any_of(","));
  }

  // Trim leading and trailing whitespace.
  for(std::string& name : _formatNames) {
    ba::trim(name);
  }

  // Remove empty names.
  _formatNames.erase(std::remove_if(_formatNames.begin(), _formatNames.end(),
         [](std::string const& string) { return string.empty(); }),
         _formatNames.end());

  // Remove duplicate names.
  dev::unique(_formatNames);
}



//! Handles GDAL_DATA environment variable.
/*!
  \param     prefix Prefix of package install location.
  \return    .
  \exception .
  \warning   .
  \sa        .

  Sets GDAL_DATA if it is not already set.

  GDAL_DATA is used by GDAL to find data files used by some of the drivers.
  For example, the WCS driver need access to this directory. If GDAL_DATA is
  not set correctly, the WCS driver will fail upon opening data.
*/
void Environment::handleGdalData(
         std::filesystem::path const& prefix)
{
  std::string const variable = dev::environmentVariable("GDAL_DATA");
  std::string const gdalData;

  if(dev::environmentVariableSet("GDAL_DATA")) {
    _gdalData = dev::environmentVariable("GDAL_DATA");
  }
  else {
    std::filesystem::path result(prefix); // /usr

    result = result / "share" / "gdal";     // /usr/share/gdal
    _gdalData = result.string();

    CPLSetConfigOption("GDAL_DATA", _gdalData.c_str());
  }
}



//! Returns the collection of format names found in $PCRASTER_DAL_FORMATS.
/*!
  \return    Format names.
*/
std::vector<std::string> const& Environment::formatNames() const
{
  return _formatNames;
}



std::string const& Environment::gdalData() const
{
  return _gdalData;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

