#include "dal_TextConstantDriver.h"
#include <memory>
#include <boost/lexical_cast.hpp>
#include "dal_FilesystemUtils.h"



/*!
  \file
  This file contains the implementation of the TextConstantDriver class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEXTCONSTANTDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TEXTCONSTANTDRIVER MEMBERS
//------------------------------------------------------------------------------

TextConstantDriver::TextConstantDriver()

  : ConstantDriver(Format("text", "Text constant file format",
         CONSTANT, Format::File, Format::Attribute)),
    TextFileDriver()

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".txt");
  format().setExtensions(extensions);
}



TextConstantDriver::~TextConstantDriver()
{
}



bool TextConstantDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  // TODO Take default extension(s) into account. See raster driver.
  return pathExists(name, space, address);
}



bool TextConstantDriver::open(
         Constant& /* constant */,
         std::istream& stream) const
{
  bool result = false;
  std::string line;
  std::getline(stream, line);

  if(!stream.fail() && stream.eof()) {
    // Contents in line should be a single floating point number.
    try {
      /* constant = */ boost::lexical_cast<double>(line);
    }
    catch(boost::bad_lexical_cast const&) {
    }
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Opening also reads the data set.
*/
Constant* TextConstantDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  Constant* result = 0;

  boost::filesystem::path path(pathForDataSpaceAddress(name, space, address));

  std::ifstream stream;

  if(TextFileDriver::open(stream, path)) {
#ifdef __GXX_EXPERIMENTAL_CXX0X__
    std::unique_ptr<Constant> constant(new Constant());
#else
    std::auto_ptr<Constant> constant(new Constant());
#endif

    if(open(*constant, stream)) {
      result = constant.release();
    }
  }

  return result;
}



void TextConstantDriver::read(
         Constant& constant,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  boost::filesystem::path path(pathForDataSpaceAddress(name, space, address));
  std::ifstream stream;

  if(!TextFileDriver::open(stream, path) || !open(constant, stream)) {
    throwCannotBeOpened(name, CONSTANT, space, address);
  }
}



Constant* TextConstantDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  Constant* result = open(name, space, address);

  if(!result) {
    throwCannotBeOpened(name, CONSTANT, space, address);
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

