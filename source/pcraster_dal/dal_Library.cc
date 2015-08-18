#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Library class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC LIBRARY MEMBERS
//------------------------------------------------------------------------------

static boost::shared_ptr<Library> libraryScope;

//! Initialises the library.
/*!
  This function does nothing when the library is already initialised.

  After calling this function the global library object can be used.
*/
void Library::initialise(
         boost::filesystem::path const& prefix,
         bool cacheDatasetInfo)
{
  if(!libraryScope) {
    libraryScope.reset(new Library(prefix, cacheDatasetInfo));
  }
}



//! Reclaims the resources taken by the library.
/*!
  This function does nothing when the library is not initialized.

  After calling this function the global library object can not be used
  anymore.
*/
void Library::cleanUp()
{
  if(libraryScope) {
    libraryScope.reset();
  }
}



//! Returns whether the library is initialised or not.
/*!
  \return    true or false
*/
bool Library::isInitialised()
{
  return bool(libraryScope);
}



Environment const& Library::environment() const
{
  return _environment;
}


bool Library::cacheDatasetInfo() const
{
  return _cacheDatasetInfo;
}

//------------------------------------------------------------------------------
// DEFINITION OF LIBRARY MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  \exception .
  \warning   .
  \sa        .

  Initialises and configures the library.
*/
Library::Library(
         boost::filesystem::path const& prefix,
         bool cacheDatasetInfo)

  : _environment(prefix),
    _cacheDatasetInfo(cacheDatasetInfo)

{
  addGraphicsFormats();
  addAttributeFormats();
}



//! Destructor.
/*!
  \todo      Shouldn't we delete the drivers managed by GDalRasterDriver
             (except for the ones currently installed in gdal's manager).
*/
Library::~Library()
{
  // TODO Clean up the cache data pool: its data is ours.
}



//! Adds the graphics formats we know of.
/*!
*/
void Library::addGraphicsFormats()
{
  std::vector<std::string> extensions;
  extensions.push_back(".png");
  _knownFormats.push_back(Format("PNG", "Portable Network Graphics", GRAPHIC,
         extensions, Format::Raster, Format::Graphics));

  extensions.clear();
  extensions.push_back(".eps");
  _knownFormats.push_back(Format("EPS", "Encapsulated PostScript", GRAPHIC,
         extensions, Format::Vector, Format::Graphics));
}



//! Adds the attribute formats we know of.
/*!
*/
void Library::addAttributeFormats()
{
  std::vector<std::string> extensions;
  extensions.push_back(".pcrmap");
  extensions.push_back(".csf");
  extensions.push_back(".map");

  _knownFormats.push_back(Format("CSF", "PCRaster Raster", RASTER,
         extensions, Format::Raster, Format::Attribute));

  extensions.clear();
  extensions.push_back(".pcrtss");
  extensions.push_back(".tss");

  _knownFormats.push_back(Format("TSS", "PCRaster Time Series", TABLE,
         extensions, Format::Vector, Format::Attribute));
}



//! Returns the collection of formats the library knows about.
/*!
  \return    Collection of formats.
  \exception .
  \warning   To know about a format doesn't mean the library can read or write
             the format, only that the library has heard of the format.
*/
Formats const& Library::knownFormats() const
{
  assert(isInitialised());

  return _knownFormats;
}



MemoryDataPool& Library::memoryDataPool()
{
  assert(isInitialised());

  return _memoryDataPool;
}



MemoryDataPool& Library::cacheDataPool()
{
  assert(isInitialised());

  return _cacheDataPool;
}



Cache<FeatureLayerGeometries>& Library::geometriesCache()
{
  assert(isInitialised());

  return _geometriesCache;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

Library* library() {
  // The lifetime of this object is managed by the Client class.
  // Library::initialise();

  // You did initialize the Dal lib, did you? Check dal::Client class.
  assert(libraryScope);

  return libraryScope.get();
}

} // namespace dal

