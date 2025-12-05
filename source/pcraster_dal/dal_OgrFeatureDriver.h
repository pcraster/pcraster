#ifndef INCLUDED_DAL_OGRFEATUREDRIVER
#define INCLUDED_DAL_OGRFEATUREDRIVER

#include "dal_FeatureDriver.h"

#include <set>


class OGRDataSource;
class OGRLayer;
class GDALDriver;
namespace dal {
  // OgrFeatureDriver declarations.
  class FeaturePath;
}



namespace dal {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo      Make sure memory is handled correctly. DataSource needs to be
             around as long as the features are used.
  \sa        .

  <table>
    <tr>
      <td>dal::FeaturePathParseStrategy</td>
      <td>FeaturePath::ParseStrategy</td>
      <td></td>
    </tr>
  </table>
*/
class OgrFeatureDriver: public FeatureDriver
{

  friend class OgrFeatureDriverTest;

private:

  //! Ogr driver used for I/O.
  GDALDriver*      _driver;

  //! List containing name of driver;
  char**           _driver_names{};

  static bool      driverIsAvailable   (std::string const& name);

  static GDALDriver* driverByName      (std::string const& name);

  static std::set<OGRDataSource*> _dataSources;

  void             init                ();

  /// bool             exists              (FeaturePath const& path,
  ///                                       DataSpace const& space,
  ///                                       DataSpaceAddress const& address) const;

  FeaturePath      featurePathFor      (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             readGeometry        (FeatureLayer& layer,
                                        OGRLayer& ogrLayer) const;

  void             readGeometryAndAttribute(
                                        FeatureLayer& layer,
                                        OGRLayer& ogrLayer) const;

  void             readAttribute       (FeatureLayer& layer,
                                        OGRLayer& ogrLayer) const;

  void             readAttribute       (FeatureLayer& layer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             readAttribute       (FeatureLayer& layer,
                                        OGRLayer& ogrLayer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             readGeometryAndAttribute(
                                        FeatureLayer& layer,
                                        OGRLayer& ogrLayer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             updateAttribute     (FeatureLayer& layer,
                                        OGRLayer& ogrLayer) const;

  void             updateAttribute     (FeatureLayer& layer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             updateAttribute     (FeatureLayer& layer,
                                        OGRLayer& ogrLayer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  TypeId           open                (OGRLayer& ogrLayer,
                                        std::string const& attributeName,
                                        TypeId typeId) const;

  TypeId           open                (OGRLayer& layer,
                                        FeaturePath const& path,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             filterOutUnsupportedFileNames(
                                        std::vector<std::string>& leaves) const;

protected:

public:

  static void      registerOgrDrivers  ();

  static void      deregisterOgrDrivers();

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   OgrFeatureDriver    (std::string const& name);

                   OgrFeatureDriver    (GDALDriver* driver);

  /* virtual */    ~OgrFeatureDriver   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  using FeatureDriver::read;
  using FeatureDriver::open;

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  FeatureLayer*    open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  FeatureLayer*    read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const override;

  void             read                (FeatureLayer& layer,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const override;

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
