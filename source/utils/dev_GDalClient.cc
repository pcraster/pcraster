#include "dev_GDalClient.h"

#include <gdal_priv.h>
#include <ogr_api.h>
#include <ogrsf_frmts.h>

#include <cassert>



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
      for(int i = 0; i < GetGDALDriverManager()->GetDriverCount(); ++i) {
        auto* driver = GetGDALDriverManager()->GetDriver(i);
        GDALDeregisterDriver(driver);
      }
      // GDALDestroyDriverManager();
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

