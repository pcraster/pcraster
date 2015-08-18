#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_GDALRASTERDRIVER
#include "dal_GDALRasterDriver.h"
#define INCLUDED_DAL_GDALRASTERDRIVER
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_OGRFEATUREDRIVER
#include "dal_OgrFeatureDriver.h"
#define INCLUDED_DAL_OGRFEATUREDRIVER
#endif



/*!
  \file
  This file contains the implementation of the Client class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

} // namespace detail



/*
 * do not use, throwing from here results in un-closed files
 *  as in gdal CVS: Wed Feb  1 21:44:18 WEST 2006
 * static void CPL_STDCALL GDALErrorHandler(CPLErr errorClass, int,
 *        const char* message) {
 * if(errorClass == CE_Failure || errorClass == CE_Fatal) {
 *   PRINT_VAR(message);
 *   throw Exception(message);
 * }
 * }
 */



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLIENT MEMBERS
//------------------------------------------------------------------------------

size_t Client::_count = 0;



Dal* Client::_dal(0);



Library& Client::library()
{
  assert(isInitialized());

  return *dal::library();
}



bool Client::isInitialized()
{
  return Library::isInitialised();
}



Dal& Client::dal()
{
  return *_dal;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLIENT MEMBERS
//------------------------------------------------------------------------------

Client::Client(
         boost::filesystem::path const& prefix,
         bool addAllDrivers,
         bool cacheDatasetInfo)
{
  if(_count == 0) {
    // CPLSetErrorHandler(GDALErrorHandler);
    // Don't throw in case of an error.
    CPLSetErrorHandler(CPLQuietErrorHandler);

    GDALRasterDriver::registerGDALDrivers();
    OgrFeatureDriver::registerOgrDrivers();

    Library::initialise(prefix, cacheDatasetInfo);

    assert(_dal == 0);
    _dal = new Dal(addAllDrivers);
  }

  ++_count;

  assert(Library::isInitialised());
}



Client::~Client()
{
  assert(_count > 0);

  if(_count == 1) {
    GDALRasterDriver::deregisterGDALDrivers();
    OgrFeatureDriver::deregisterOgrDrivers();

    Library::cleanUp();

    assert(!Library::isInitialised());

    assert(_dal != 0);
    delete _dal;
    _dal = 0;
  }

  --_count;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

