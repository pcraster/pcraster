#include "dal_Client.h"
#include "dal_Dal.h"
#include "dal_GDALRasterDriver.h"
#include "dal_Library.h"
#include "dal_OgrFeatureDriver.h"



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



Dal* Client::_dal(nullptr);



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
         std::filesystem::path const& prefix,
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

    assert(_dal == nullptr);
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

    assert(_dal != nullptr);
    delete _dal;
    _dal = nullptr;
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

