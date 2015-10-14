#ifndef INCLUDED_DEV_GDALCLIENT
#include "dev_GDalClient.h"
#define INCLUDED_DEV_GDALCLIENT
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_GDAL_PRIV
#include <gdal_priv.h>
#define INCLUDED_GDAL_PRIV
#endif

#ifndef INCLUDED_OGR_API
#include <ogr_api.h>
#define INCLUDED_OGR_API
#endif

// Project headers.
#ifndef INCLUDED_DEV_CONFIGURE
#include "dev_Configure.h"
#define INCLUDED_DEV_CONFIGURE
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the GDalClient class.
*/



namespace dev {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GDALCLIENT MEMBERS
//------------------------------------------------------------------------------

bool GDalClient::_weInitializedGdal = false;

unsigned short GDalClient::_count = 0;



//------------------------------------------------------------------------------
// DEFINITION OF GDALCLIENT MEMBERS
//------------------------------------------------------------------------------

GDalClient::GDalClient()
{
  if(_count == 0) {
    // The first GDALClient object created.
    // Use GetGDALDriverManager() to fetch the global singleton instance
    // of GDALDriverManager
    if(GetGDALDriverManager()->GetDriverCount() == 0) {
      // GDal library is not initialized yet.
      GDALAllRegister();

#ifdef DEVBASE_GDAL_LIBRARY_HAS_OGR_SUPPORT
      OGRRegisterAll();
#endif

      assert(GetGDALDriverManager()->GetDriverCount() > 0);
      _weInitializedGdal = true;
    }
  }

  ++_count;
}



GDalClient::~GDalClient()
{
  assert(_count > 0);

  if(_count == 1) {
    // Last GDalClient object is being destructed.
    if(_weInitializedGdal) {
      // We initialized the GDal library, so we need to clean up again.
#ifdef DEVBASE_GDAL_LIBRARY_HAS_OGR_SUPPORT
      OGRCleanupAll();
#endif

      GDALDestroyDriverManager();

      _weInitializedGdal = false;
    }
  }

  --_count;
}



bool GDalClient::isInitialized() const
{
  return _count > 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dev

