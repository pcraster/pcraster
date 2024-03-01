#ifndef INCLUDED_DAL_CSFRASTERDRIVER
#define INCLUDED_DAL_CSFRASTERDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif

#include <filesystem>


namespace dal {
  // CSFRasterDriver declarations.
  class CSFMap;
}



namespace dal {



//! Driver class for raster i/o to CSF-2.0 formatted files.
/*!
  This class implements a raster file driver class for PCRasters CSF-2.0
  file format. It uses the CSFMap class for the real stuff.

  Supported extensions: .csf, .map and .pcrmap.

  Properties read from Raster::properties() and set in Raster::properties()
  by this driver are:

  <table>
    <tr>
      <td>csf::Angle</td>
      <td>REAL8</td>
      <td>0.0</td>
    </tr>
    <tr>
      <td>csf::ValueScale</td>
      <td>CSF_VS</td>
      <td>dal::typeIdToValueScale(TypeId)</td>
    </tr>
    <tr>
      <td>csf::Projection</td>
      <td>CSF_PT</td>
      <td>PT_YDECT2B</td>
    </tr>
    <tr>
      <td>dal::Legend</td>
      <td>Table</td>
      <td></td>
    </tr>
  </table>

  Properties read from Driver::properties() and set in Driver::properties()
  by this driver are:

  <table>
    <tr>
      <td>dal::FilenameConvention</td>
      <td>FilenameConvention</td>
      <td>DALConvention</td>
    </tr>
    <tr>
      <td>dal::DefaultExtension</td>
      <td>std::string</td>
      <td></td>
    </tr>
  </table>

  \todo Get rid of the const_cast used in the various member functions.
  \todo Refactor filenaming stuff out and put it in a filenaming class. Let
        other driver profit from this code too (GDALRasterDrivers for file
        drivers). PathNamer.
*/
class PCR_DAL_DECL CSFRasterDriver: public RasterDriver
{

  friend class CSFRasterDriverTest;

private:

  Raster*          open                (std::filesystem::path const& path,
                                        TypeId typeId) const;

  Raster*          read                (std::filesystem::path const& path,
                                        TypeId typeId) const;

  void             read                (Raster& raster,
                                        std::filesystem::path const& path) const;

  void             read                (Raster& raster,
                                        CSFMap& map) const;

  void             write               (Raster const& raster,
                                        std::filesystem::path const& path) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CSFRasterDriver     ();

  /* virtual */    ~CSFRasterDriver    () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  using RasterDriver::read;
  using RasterDriver::open;

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  Raster*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  Raster*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  void             read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  void             write               (Raster const& raster,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const override;

  void             browse              (std::vector<BrowseInfo>& attributes,
                                        std::string const& location) const override;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
