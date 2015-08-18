#include "com_fileformatinfo.h"

// Library headers.
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the FileFormatInfo class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILEFORMATINFO MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FILEFORMATINFO MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     description Description of file format.
  \param     extensions Default extension(s) of file format.
  \warning   \a description and \a extensions should not be empty.

  If a file format has more than one default extension, you can separate them
  by a | character:

  \code
  // Default extensions are fmt, frm and mtf.
  FileFormatInfo myFileInfo("My file format", "fmt|frm|mtf")
  \endcode

  Make sure that the most default extension is the first in \a extensions.
*/
com::FileFormatInfo::FileFormatInfo(const std::string& description,
                   const std::string& extensions)
  : d_description(description)
{
  assert(!description.empty());

  boost::split(d_extensions, extensions, boost::is_any_of("|"));
  assert(!extensions.empty());
}



com::FileFormatInfo::FileFormatInfo(
         std::string const& description,
         std::vector<std::string> const& extensions)

  : d_description(description), d_extensions(extensions)

{
  assert(!d_description.empty());
  assert(!d_extensions.empty());
}



//! Destructor.
com::FileFormatInfo::~FileFormatInfo()
{
}



bool com::FileFormatInfo::equals(const FileFormatInfo& formatInfo) const
{
  return d_description == formatInfo.d_description &&
         d_extensions == formatInfo.d_extensions;
}



//! Returns the description.
/*!
  \return    Description.
*/
const std::string& com::FileFormatInfo::description() const
{
  return d_description;
}



//! Returns the first (= most default) default extension.
/*!
  \return    Extension.
*/
const std::string& com::FileFormatInfo::extension() const
{
  return d_extensions[0];
}



//! Returns the default extensions.
/*!
  \return    Extensions.
*/
const std::vector<std::string>& com::FileFormatInfo::extensions() const
{
  return d_extensions;
}

// //! return true if \a extension if one of the extensions
// /*!
//  * \param extension without the . (dot)
//  */
// bool com::FileFormatInfo::matchesExtension(
//          std::string const& extension) const
// {
//   bool result = false;
// 
//   boost::filesystem::path extensionAsPath(extension);
// 
//   BOOST_FOREACH(std::string const& candidate, d_extensions) {
//     if(boost::filesystem::equivalent(candidate, extensionAsPath)) {
//       result = true;
//       break;
//     }
//   }
// 
//   return result;
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

bool com::operator==(const com::FileFormatInfo& lhs,
                   const com::FileFormatInfo& rhs)
{
  return lhs.equals(rhs);
}



bool com::operator!=(const com::FileFormatInfo& lhs,
                   const com::FileFormatInfo& rhs)
{
  return !lhs.equals(rhs);
}


//! Encapsulated PostScript
com::FileFormatInfo com::FileFormatInfo::eps()
{
  return FileFormatInfo("Encapsulated PostScript", "eps");
}

com::FileFormatInfo com::FileFormatInfo::png()
{
  return FileFormatInfo("Portable Network Graphics", "png");
}

com::FileFormatInfo com::FileFormatInfo::csf()
{
  return FileFormatInfo("PCRaster Raster", "csf|map");
}

com::FileFormatInfo com::FileFormatInfo::pcr()
{
  return FileFormatInfo("PCRaster File", "csf|map|tss");
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



