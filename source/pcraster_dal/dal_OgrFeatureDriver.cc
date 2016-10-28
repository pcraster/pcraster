#ifndef INCLUDED_DAL_OGRFEATUREDRIVER
#include "dal_OgrFeatureDriver.h"
#define INCLUDED_DAL_OGRFEATUREDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_ALGORITHM_STRING_PREDICATE
#include <boost/algorithm/string/predicate.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING_PREDICATE
#endif

#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_FUNCTION
#include <boost/function.hpp>
#define INCLUDED_BOOST_FUNCTION
#endif

#include <boost/shared_ptr.hpp>

#ifndef INCLUDED_OGRSF_FRMTS
#include <ogrsf_frmts.h>
#define INCLUDED_OGRSF_FRMTS
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_CONFIGURE
#include "dev_Configure.h"
#define INCLUDED_DEV_CONFIGURE
#endif

// Module headers.
#ifndef INCLUDED_DAL_CLIENT
#include "dal_Client.h"
#define INCLUDED_DAL_CLIENT
#endif

#ifndef INCLUDED_DAL_DAL
#include "dal_Dal.h"
#define INCLUDED_DAL_DAL
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FEATUREPATH
#include "dal_FeaturePath.h"
#define INCLUDED_DAL_FEATUREPATH
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

#ifndef INCLUDED_DAL_LIBRARY
#include "dal_Library.h"
#define INCLUDED_DAL_LIBRARY
#endif

#ifndef INCLUDED_DAL_TABLEDRIVER
#include "dal_TableDriver.h"
#define INCLUDED_DAL_TABLEDRIVER
#endif



/*!
  \file
  This file contains the implementation of the OgrFeatureDriver class.
*/



namespace dal {

// Code that is private to this module.
namespace detail {

static std::vector<GDALDriver*> drivers;

TypeId fieldTypeToTypeId(
         OGRFieldType type)
{
  TypeId result = TI_NR_TYPES;

  switch(type) {
    case OFTInteger: {
      result = TI_INT4;
      break;
    }
    case OFTReal: {
      result = TI_REAL8;
      break;
    }
    case OFTString: {
      result = TI_STRING;
      break;
    }
    case OFTIntegerList:
    case OFTRealList:
    case OFTStringList:
    case OFTWideString:
    case OFTWideStringList:
    case OFTBinary:
    case OFTDate:
    case OFTTime:
    case OFTDateTime: {
      result = TI_NR_TYPES;
      break;
    }
  }

  return result;
}



std::string tableName(
         FeaturePath const& path,
         DataSpace const& /* space */)
{
  assert(path.isValid());
  assert(!path.layer().empty());
  assert(!path.attribute().empty());

  std::string result = boost::filesystem::path(path.source()).stem().string() +
    "/" + path.layer() + "/{fid," + path.attribute() + "}";

  /// size_t indexOfScenarios = space.indexOf(Scenarios);

  /// if(indexOfScenarios != space.rank()) {
  ///   // Prepend name by scenario.
  ///   result = address.coordinate<std::string>(indexOfScenarios) + "/" + result;
  // }

  return result;
}



// std::string colName(
//          DataSpace space,
//          DataSpaceAddress address)
// {
//   std::string result;
// 
//   size_t indexOfScenarios = space.indexOf(Scenarios);
// 
//   if(indexOfScenarios != space.rank()) {
//     space.eraseDimension(indexOfScenarios);
//     address.eraseCoordinate(indexOfScenarios);
//   }
// 
//   result = pathForDataSpaceAddress("x", space, address).string();
// 
//   // Remove starting "x_".
//   assert(result.find("x_") == 0);
//   result.erase(0, 2);
// 
//   return result;
// }


int fieldId(
         OGRLayer& ogrLayer,
         std::string const& name)
{
  int result = -1;

  // Feature definition is owned by the layer.
  OGRFeatureDefn* featureDefinition = ogrLayer.GetLayerDefn();
  assert(featureDefinition);
  result = featureDefinition->GetFieldIndex(name.c_str());

  return result;
}



// int fieldId(
//          Table const& table,
//          DataSpace const& space,
//          DataSpaceAddress const& address)
// {
//   int result = -1;
//   std::string colName = detail::colName(space, address);
// 
//   // Determine the type id of this column.
//   for(size_t i = 0; i < table.nrCols(); ++i) {
//     if(table.title(i) == colName) {
//       result = i;
//       break;
//     }
//   }
// 
//   return result;
// }



void read(
         Table& table,
         FeatureLayer& layer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address)
{
  assert(table.nrRecs() == 0);

  size_t attrId = table.indexOf(path.attribute());
  assert(attrId < table.nrCols());

  // if(!(attrId < table.nrCols())) {
  //   // TODO improve message. And test it. Just ask for a non-existing address.
  //   // Mentiond path.attribute().
  //   throwDataSourceError(tableName, TABLE, space, address, "Attribute field missing");
  // }

  // size_t fidId = table.indexOf("fid");

  // if(!(fidId < table.nrCols())) {
  //   // TODO improve message. And test it. Mention "fid".
  //   throwDataSourceError(tableName, TABLE, space, address, "Feature-id field missing");
  // }

  // if(table.typeId(fidId) != TI_INT4) {
  //   // TODO improve message. And test it.
  //   throwDataSourceError(tableName, TABLE, space, address, "Feature-id field must be integral");
  // }

  /// TypeId typeId = table.typeId(attrId);

  /// if(typeId = TI_STRING) {
  ///   // TODO for some reason qt converts REAL8 to string. Assume that happened.
  ///   typeId = TI_REAL4;
  /// }

  /// if(typeId = TI_REAL8) {
  ///   typeId = TI_REAL4;
  /// }

  /// if(typeId != TI_REAL4) {
  ///   // TODO improve message. And test it.
  ///   throwDataSourceError(name, TABLE, space, address, "Attribute column must be floating point");
  /// }

  /// table.setTypeId(attrId, typeId);

  assert(layer.typeId() != TI_NR_TYPES);
  table.setTypeId(attrId, layer.typeId());

  // // Turn off all columns except the ones we need: fidId and attrId.
  // // TODO is this needed? maybe assert that those columns are not even there?
  // for(size_t i = 0; i < table.nrCols(); ++i) {
  //   if(i != fidId && i != attrId) {
  //     table.setTypeId(i, TI_NR_TYPES);
  //   }
  // }

  std::string tableName(detail::tableName(path, space));

  // This assumes the table is previously opened by dal and, hence, the driver
  // is known.
  TableDriver const& driver(dynamic_cast<TableDriver const&>(
         *Client::dal().driverByDataset(tableName, space)));
  driver.read(table, tableName, space, address);

  if(table.nrRecs() != layer.nrGeometries()) {
    // TODO Improve and test.
    // Maybe append/interpret the missing one as missing values?
    throwDataSourceError(tableName, TABLE, space, address,
         "Number of attribute values does not equal the number of features");
  }

}

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC OGRFEATUREDRIVER MEMBERS
//------------------------------------------------------------------------------

void OgrFeatureDriver::registerOgrDrivers()
{
  assert(detail::drivers.empty());

  auto* manager = GetGDALDriverManager();

  for(int i = 0; i < manager->GetDriverCount(); ++i) {
    auto* driver = manager->GetDriver(i);
    auto metadata = driver->GetMetadata();

    if(CSLFetchBoolean(metadata, GDAL_DCAP_VECTOR, FALSE)) {
        detail::drivers.push_back(driver);
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement. Do we need to do anything?
*/
void OgrFeatureDriver::deregisterOgrDrivers()
{
  detail::drivers.clear();

  assert(detail::drivers.empty());
}



//! Returns a use-only pointer to the Ogr driver named \a name.
/*!
  \param     name Name of driver to return.
  \return    Pointer to Ogr driver or 0 when no such driver exists.
  \warning   Currently the names are compared case-sensitively.
*/
GDALDriver* OgrFeatureDriver::driverByName(
         std::string const& name)
{
  assert(Library::isInitialised());

  GDALDriver* result = 0;

  for(size_t i = 0; i < detail::drivers.size(); ++i) {
    if(detail::drivers[i]->GetDescription() == name) {
      result = detail::drivers[i];
      break;
    }
  }

  return result;
}



//! Returns whether a driver with name \a name is known.
/*!
  \param     name Name of driver to look for.
  \return    true or false
  \warning   Currently the names are compared case-sensitively.
*/
bool OgrFeatureDriver::driverIsAvailable(
         std::string const& name)
{
  return driverByName(name) != 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF OGRFEATUREDRIVER MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     name Name of Ogr driver the driver.
  \exception Exception When no driver is known by the \a name passed in.
  \sa        .

  The Ogr driver is layered in this object to do the actual work.
*/
OgrFeatureDriver::OgrFeatureDriver(
         std::string const& name)

  : FeatureDriver(Format(name, std::string("OGR feature driver for ") + name,
         FEATURE, Format::File, Format::Attribute)),
    _driver(0)

{
  _driver = driverByName(name);

  if(!_driver) {
    throw Exception((boost::format(
         "OGR feature driver for %1%: Not available")
         % name).str());
  }

  assert(_driver);

  init();
}



OgrFeatureDriver::OgrFeatureDriver(
         GDALDriver* driver)

  : FeatureDriver(Format(driver->GetDescription(),
         std::string("OGR feature driver for ") + driver->GetDescription(),
         FEATURE, Format::File, Format::Attribute)),
    _driver(driver)

{
  assert(std::find(detail::drivers.begin(), detail::drivers.end(), driver) !=
         detail::drivers.end());

  init();
}



//! Destructor.
/*!
*/
OgrFeatureDriver::~OgrFeatureDriver()
{
  CSLDestroy(_driver_names);
}



void OgrFeatureDriver::init()
{
  _driver_names = CSLAddString(NULL, _driver->GetDescription());

  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;

  char** metadata = _driver->GetMetadata();

  // GDAL_DCAP_CREATECOPY?
  if(CSLFetchBoolean(metadata, ODrCCreateDataSource, FALSE)) {
    properties |= Writer;
  }

  if(CSLFetchBoolean(metadata, ODrCDeleteDataSource, FALSE)) {
    properties |= Deleter;
  }

  std::vector<std::string> extensions;

  if(name() == "CSV") {
    extensions.push_back(".csv");
  }
  else if(name() == "ESRI Shapefile") {
    extensions.push_back(".shp");
  }
  else if(name() == "GeoJSON") {
    extensions.push_back(".json");
    extensions.push_back(".geojson");
  }
  else if(name() == "GML") {
    extensions.push_back(".gml");
  }
  else if(name() == "KML") {
    extensions.push_back(".kml");
  }
  else if(name() == "VRT") {
    extensions.push_back(".vrt");
  }

  format().setExtensions(extensions);
}



/// bool OgrFeatureDriver::exists(
///          FeaturePath const& path,
///          DataSpace const& space,
///          DataSpaceAddress const& address) const
/// {
///   // TODO check attribute table
///   // TODO
///   return false;
/// 
///   // bool result = false;
/// 
///   // if(path.isValid()) {
///   //   result = dal::pathExists(path.source());
/// 
///   //   if(!path.attribute().empty()) {
///   //     
///   //   }
///   // }
/// 
///   // return result;
/// 
///   return path.isValid() && dal::pathExists(path.source(), space, address);
/// }



//!
/*!
  \param     .
  \return    Feature path to use for name/space, or default constructed path
             if feature layer name/space does not exist.
  \exception .
  \warning   .
  \sa        .
*/
FeaturePath OgrFeatureDriver::featurePathFor(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& /* address */) const
{
  // Collection of properties for this name/space combination. This collection
  // may be empty or have some information obtained the previous time this
  // function was called.
  // TODO How about two data sets, without extension, with the same name
  // TODO but different types of data:
  // TODO - concentration(.shp)
  // TODO - concentration(.sql3)
  // TODO Data type should be part of the properties key!
  Properties& properties(this->properties(name, space));

  FeaturePath result;
  bool found = false;
  FeaturePath::ParseStrategy strategy;
  FilenameConvention convention;
  std::string extension;

  if(properties.hasValue(DAL_FEATURE_DRIVER_PARSE_STRATEGY)) {
    // Parse strategy to use when opening the data set is known.
    strategy = properties.value<FeaturePath::ParseStrategy>(
         DAL_FEATURE_DRIVER_PARSE_STRATEGY);
    result = FeaturePath(name, strategy);
    found = true;
  }
  else {
    // Determine parse strategy.

    // Geometry is stored as a layer in a single feature data set.
    // Data space and address are not relevant at this point.

    // A function to determine wheter a dataset exists.
    auto callBack = [&](std::string const& name) {
        auto dataset = GDALOpenEx(name.c_str(), GDAL_OF_VECTOR,
            this->_driver_names, NULL, NULL);
        bool result = dataset != NULL;
        GDALClose(dataset);
        return result;
    };

    // First, assume attribute name is not present.
    strategy = FeaturePath::WithoutAttribute;
    result = FeaturePath(name, strategy);

    if(result.isValid()) {
      boost::tie(found, convention, extension) =
         dal::determineFilenameCharacteristics(callBack,
              result.source(), DataSpace(), DataSpaceAddress(),
              format().extensions());
    }

    if(!found) {
      // Second, assume attribute name is present.
      strategy = FeaturePath::WithAttribute;
      result = FeaturePath(name, strategy);

      if(result.isValid()) {
        convention = DALConvention;
        boost::tie(found, convention, extension) =
              dal::determineFilenameCharacteristics(callBack,
                   result.source(), DataSpace(), DataSpaceAddress(),
                   format().extensions());
      }
    }

    if(!found) {
      // We're out of ideas.
      result = FeaturePath();
    }
    else {
      // We found data!
      properties.setValue<FeaturePath::ParseStrategy>(
         DAL_FEATURE_DRIVER_PARSE_STRATEGY, strategy);
      properties.setValue<FilenameConvention>(
         DAL_FILENAME_CONVENTION, convention);

      if(!extension.empty()) {
        properties.setValue<std::string>(DAL_DEFAULT_EXTENSION, extension);
      }
    }
  }

  if(found) {
    assert(result.isValid());
    boost::filesystem::path path(result.source() + defaultExtension(name,
         space));
    result = FeaturePath(
         (path / result.layer() / result.attribute()).generic_string(),
              strategy);
    assert(result.isValid());
  }

  return result;
}



bool OgrFeatureDriver::exists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  DataSpace newSpace(space);
  newSpace.eraseDimension(Space);
  DataSpaceAddress newAddress(space.eraseCoordinates(address, Space));

  // Not sure whether we can optimize the check for data at the address passed.
  // So we may as well use open for the test.
  boost::shared_ptr<FeatureLayer> layer(open(name, newSpace, newAddress,
         TI_NR_TYPES));

  return bool(layer);
}



TypeId OgrFeatureDriver::open(
         OGRLayer& ogrLayer,
         std::string const& attributeName,
         TypeId typeId) const
{
  // Feature definition is owned by the layer.
  OGRFeatureDefn* featureDefinition = ogrLayer.GetLayerDefn();
  assert(featureDefinition);

  int fieldId = featureDefinition->GetFieldIndex(attributeName.c_str());
  assert(fieldId == -1 || fieldId >= 0);

  if(fieldId == -1) {
    // No such field.
    typeId = TI_NR_TYPES;
  }
  else if(typeId == TI_NR_TYPES) {
    // Determine field type.
    // Field definition is owned by the feature definition.
    OGRFieldDefn* fieldDefinition(featureDefinition->GetFieldDefn(fieldId));
    assert(fieldDefinition);

    typeId = detail::fieldTypeToTypeId(fieldDefinition->GetType());
  }

  return typeId;
}



TypeId OgrFeatureDriver::open(
         OGRLayer& /* layer */,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  std::string tableName = detail::tableName(path, space);

  // Try to open the attribute table, whatever the format.
  boost::shared_ptr<Dataset> dataset;
  boost::tie(dataset, boost::tuples::ignore) = Client::dal().open(tableName,
      space, address, TABLE);

  int fieldId = -1;

  if(!dataset) {
    // No such table.
    typeId = TI_NR_TYPES;
  }
  else {
    // See whether there is a column with a name corresponding to the requested
    // address.
    Table const& table(dynamic_cast<Table const&>(*dataset));

    fieldId = table.indexOf(path.attribute());

    if(fieldId == -1) {
      // No such field.
      typeId = TI_NR_TYPES;
    }
    else if(typeId == TI_NR_TYPES) {
      typeId = table.typeId(fieldId);
    }
  }

  return typeId;
}



FeatureLayer* OgrFeatureDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  FeatureLayer* layer = 0;
  FeaturePath path(featurePathFor(name, space, address));

  if(path.isValid()) {
    auto dataset = static_cast<GDALDataset*>(GDALOpenEx(path.source().c_str(),
        GDAL_OF_VECTOR, _driver_names, NULL, NULL));

    if(dataset) {
      // Layer is owned by the data source.
      OGRLayer* ogrLayer = dataset->GetLayerByName(path.layer().c_str());

      if(ogrLayer) {
        // Feature definition is owned by the layer.
        OGRFeatureDefn* featureDefinition = ogrLayer->GetLayerDefn();
        OGRwkbGeometryType geometryType = featureDefinition->GetGeomType();

        if(geometryType != wkbNone && geometryType != wkbUnknown) {
          // Determine the geometry of the layer. The geometry may already be
          // read a previous time, maybe for another attribute of the same
          // layer.
          std::string key = (boost::filesystem::path(
              path.source()) /* .normalize() */ / path.layer()).string();
          FeatureLayerGeometries* geometries = 0;

          if(Client::library().geometriesCache().contains(key)) {
            geometries = Client::library().geometriesCache().object(key);
            Client::library().geometriesCache().incrementUseCount(geometries);
          }
          else {
            // May be expensive...
            OGREnvelope extent;
            ogrLayer->GetExtent(&extent, TRUE);
            double west = extent.MinX;
            double north = extent.MaxY;
            double east = extent.MaxX;
            double south = extent.MinY;

            geometries = new FeatureLayerGeometries(west, north, east, south);
            Client::library().geometriesCache().insert(key, geometries);
            Client::library().geometriesCache().incrementUseCount(geometries);
          }

          assert(Client::library().geometriesCache().contains(key));
          assert(Client::library().geometriesCache().contains(geometries));

          if(path.attribute().empty()) {
            layer = new FeatureLayer(geometries);
          }
          else {
            if(space.rank() == 0) {
              typeId = open(*ogrLayer, path.attribute(), typeId);
            }
            else {
              typeId = open(*ogrLayer, path, space, address, typeId);
            }

            if(typeId != TI_NR_TYPES) {
              layer = new FeatureLayer(geometries, path.attribute(), typeId);
            }
          }

          // This will delete the geometries from memory if no layer was
          // created. If a layer was created, its constructor incremented the
          // use count, so the geometries object will remain available.
          Client::library().geometriesCache().decrementUseCount(geometries);
        }
      }

      GDALClose(dataset);
    }
  }

  // Can be zero.
  return layer;
}



FeatureLayer* OgrFeatureDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId typeId) const
{
  assert(!space.hasSpace());

  FeatureLayer* result = open(name, space, address, typeId);

  if(!result) {
    throwCannotBeOpened(name, FEATURE, space, address);
  }

  read(*result, name, space, address);

  return result;
}



void OgrFeatureDriver::readGeometry(
         FeatureLayer& layer,
         OGRLayer& ogrLayer) const
{
  assert(layer.nrGeometries() == 0);
  assert(!layer.hasValues());

  OGRFeature* feature;
  long featureId;
  OGRGeometry* geometry;

  ogrLayer.ResetReading();

  while((feature = ogrLayer.GetNextFeature())) {
    featureId = feature->GetFID();
    geometry = feature->StealGeometry();
    assert(geometry);

    layer.insert(featureId, geometry);

    OGRFeature::DestroyFeature(feature);
  }
}



void OgrFeatureDriver::readGeometryAndAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer) const
{
  assert(layer.nrGeometries() == 0);
  assert(!layer.hasValues());
  assert(layer.hasAttribute());

  int fieldId = detail::fieldId(ogrLayer, layer.name());
  assert(fieldId >= 0);

  OGRFeature* feature;
  long featureId;
  OGRGeometry* geometry;

  ogrLayer.ResetReading();

  // FEATURE optimize, see GetFieldAs*List.
  assert(layer.typeId() != TI_NR_TYPES);

  while((feature = ogrLayer.GetNextFeature())) {
    featureId = feature->GetFID();
    geometry = feature->StealGeometry();
    assert(geometry);

    switch(layer.typeId()) {
      case TI_INT4: {
        layer.insert<INT4>(featureId, geometry,
            feature->GetFieldAsInteger(fieldId));
        break;
      }
      case TI_REAL4: {
        layer.insert<REAL4>(featureId, geometry,
            static_cast<REAL4>(feature->GetFieldAsDouble(fieldId)));
        break;
      }
      case TI_REAL8: {
        layer.insert<REAL8>(featureId, geometry,
            feature->GetFieldAsDouble(fieldId));
        break;
      }
      case TI_STRING: {
        layer.insert<std::string>(featureId, geometry,
            feature->GetFieldAsString(fieldId));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    OGRFeature::DestroyFeature(feature);
  }
}



void OgrFeatureDriver::readAttribute(
         FeatureLayer& layer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(layer.nrGeometries() > 0);
  assert(layer.hasAttribute());
  assert(!layer.hasValues());

  std::string tableName = detail::tableName(path, space);

  boost::shared_ptr<Dataset> dataset;
  boost::tie(dataset, boost::tuples::ignore) = Client::dal().open(
    tableName, space, address, TABLE);

  if(!dataset) {
    throwCannotBeOpened(tableName, TABLE);
  }

  Table& table(dynamic_cast<Table&>(*dataset));

  detail::read(table, layer, path, space, address);

  size_t attrId = table.indexOf(path.attribute());
  assert(attrId < table.nrCols());

  size_t fidId = table.indexOf("fid");
  assert(fidId < table.nrCols());

  Array<INT4> const& featureIds(table.col<INT4>(fidId));

  // FEATURE Add support for the other types.
  switch(layer.typeId()) {
    case TI_REAL4: {
      Array<REAL4> const& values(table.col<REAL4>(attrId));
      assert(featureIds.size() == values.size());

      for(size_t i = 0; i < featureIds.size(); ++i) {
        layer.setValue<REAL4>(featureIds[i], values[i]);
      }

      break;
    }
    case TI_REAL8: {
      Array<REAL8> const& values(table.col<REAL8>(attrId));
      assert(featureIds.size() == values.size());

      for(size_t i = 0; i < featureIds.size(); ++i) {
        layer.setValue<REAL8>(featureIds[i], values[i]);
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void OgrFeatureDriver::readGeometryAndAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(layer.nrGeometries() == 0);
  assert(!layer.hasValues());
  assert(layer.hasAttribute());

  if(space.rank() == 0) {
    readGeometryAndAttribute(layer, ogrLayer);
  }
  else {
    readGeometry(layer, ogrLayer);
    readAttribute(layer, path, space, address);
  }
}



void OgrFeatureDriver::updateAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer) const
{
  assert(layer.nrGeometries() > 0);
  assert(layer.hasValues());
  assert(layer.hasAttribute());

  int fieldId = detail::fieldId(ogrLayer, layer.name());
  assert(fieldId >= 0);

  OGRFeature* feature;
  long featureId;

  ogrLayer.ResetReading();

  // FEATURE optimize, see GetFieldAs*List.
  assert(layer.typeId() != TI_NR_TYPES);

  while((feature = ogrLayer.GetNextFeature())) {
    featureId = feature->GetFID();

    switch(layer.typeId()) {
      case TI_INT4: {
        layer.replace<INT4>(featureId,
            feature->GetFieldAsInteger(fieldId));
        break;
      }
      case TI_REAL4: {
        layer.replace<REAL4>(featureId,
            static_cast<REAL4>(feature->GetFieldAsDouble(fieldId)));
        break;
      }
      case TI_REAL8: {
        layer.replace<REAL8>(featureId,
            feature->GetFieldAsDouble(fieldId));
        break;
      }
      case TI_STRING: {
        layer.replace<std::string>(featureId,
            feature->GetFieldAsString(fieldId));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    OGRFeature::DestroyFeature(feature);
  }
}



void OgrFeatureDriver::updateAttribute(
         FeatureLayer& layer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  std::string tableName = detail::tableName(path, space);

  boost::shared_ptr<Dataset> dataset;
  boost::tie(dataset, boost::tuples::ignore) = Client::dal().open(tableName,
    space, address, TABLE);

  if(!dataset) {
    throwCannotBeOpened(tableName, TABLE);
  }

  Table& table(dynamic_cast<Table&>(*dataset));

  // TODO Reserve memory based on current number of features in the layer.
  detail::read(table, layer, path, space, address);

  size_t attrId = table.indexOf(path.attribute());
  assert(attrId < table.nrCols());

  size_t fidId = table.indexOf("fid");
  assert(fidId < table.nrCols());

  assert(layer.nrGeometries() == table.nrRecs());

  // (columnToWrite, joinFrom, sourceTable, columnToRead, joinTo)
  // layer.values().assign(1, 0, table, attrId, fidId);

  assert(layer.values().typeId(0) == table.typeId(attrId));

  assert(table.typeId(fidId) == TI_INT4);
  Array<INT4> const& fids(table.col<INT4>(fidId));

  switch(layer.values().typeId(0)) {
    case TI_REAL4: {
      Array<REAL4> const& values(table.col<REAL4>(attrId));

      for(size_t i = 0; i < values.size(); ++i) {
        layer.replace<REAL4>(fids[i], values[i]);
      }

      break;
    }
    case TI_REAL8: {
      Array<REAL8> const& values(table.col<REAL8>(attrId));

      for(size_t i = 0; i < values.size(); ++i) {
        layer.replace<REAL8>(fids[i], values[i]);
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void OgrFeatureDriver::updateAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(layer.nrGeometries() > 0);
  assert(layer.hasAttribute());
  assert(layer.hasValues());

  if(space.rank() == 0) {
    updateAttribute(layer, ogrLayer);
  }
  else {
    updateAttribute(layer, path, space, address);
  }
}



void OgrFeatureDriver::readAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer) const
{
  assert(layer.nrGeometries() > 0);
  assert(layer.hasAttribute());
  assert(!layer.hasValues());

  int fieldId = detail::fieldId(ogrLayer, layer.name());
  assert(fieldId >= 0);

  OGRFeature* feature;
  long featureId;

  ogrLayer.ResetReading();

  // FEATURE optimize, see GetFieldAs*List.
  assert(layer.typeId() != TI_NR_TYPES);

  while((feature = ogrLayer.GetNextFeature())) {
    featureId = feature->GetFID();

    switch(layer.typeId()) {
      case TI_INT4: {
        layer.setValue<INT4>(featureId,
            feature->GetFieldAsInteger(fieldId));
        break;
      }
      case TI_REAL4: {
        layer.setValue<REAL4>(featureId,
            static_cast<REAL4>(feature->GetFieldAsDouble(fieldId)));
        break;
      }
      case TI_REAL8: {
        layer.setValue<REAL8>(featureId,
            feature->GetFieldAsDouble(fieldId));
        break;
      }
      case TI_STRING: {
        layer.setValue<std::string>(featureId,
            feature->GetFieldAsString(fieldId));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    OGRFeature::DestroyFeature(feature);
  }
}



void OgrFeatureDriver::readAttribute(
         FeatureLayer& layer,
         OGRLayer& ogrLayer,
         FeaturePath const& path,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(layer.nrGeometries() > 0);
  assert(layer.hasAttribute());
  assert(!layer.hasValues());

  if(space.rank() == 0) {
    readAttribute(layer, ogrLayer);
  }
  else {
    readAttribute(layer, path, space, address);
  }
}



void OgrFeatureDriver::read(
         FeatureLayer& layer,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  FeaturePath path(featurePathFor(name, space, address));
  GDALDataset* dataset = nullptr;

  try {
    GDALDataset* dataset = static_cast<GDALDataset*>(GDALOpenEx(
        path.source().c_str(), GDAL_OF_VECTOR, _driver_names, NULL, NULL));

    if(!dataset) {
      throwCannotBeOpened(name, FEATURE, space, address);
    }

    // Layer is owned by the data source.
    OGRLayer* ogrLayer = dataset->GetLayerByName(path.layer().c_str());

    if(!ogrLayer) {
      // FEATURE  Layer not present in feature data source, improve msg.
      throwCannotBeOpened(name, FEATURE, space, address);
    }

    // Feature definition is owned by the layer.
    OGRFeatureDefn* featureDefinition = ogrLayer->GetLayerDefn();
    OGRwkbGeometryType geometryType = featureDefinition->GetGeomType();

    if(geometryType == wkbNone || geometryType == wkbUnknown) {
      // FEATURE  Unknow geometry type.
      throwCannotBeOpened(name, FEATURE, space, address);
    }

    if(layer.nrGeometries() == 0) {
      if(!layer.hasAttribute()) {
        readGeometry(layer, *ogrLayer);
      }
      else {
        readGeometryAndAttribute(layer, *ogrLayer, path, space, address);
      }
    }
    else if(layer.hasAttribute()) {
      if(!layer.hasValues()) {
        readAttribute(layer, *ogrLayer, path, space, address);
      }
      else {
        updateAttribute(layer, *ogrLayer, path, space, address);
      }
    }
    else {
      // Do nothing. This is a geometry-only data set, and the geometry is
      // already read.
    }
  }
  catch(...) {
    assert(dataset);
    GDALClose(dataset);
    throw;
  }

  GDALClose(dataset);
}



void OgrFeatureDriver::filterOutUnsupportedFileNames(
         std::vector<std::string>& leaves) const
{
  std::vector<size_t> ids;
  std::vector<std::string> extensions;

  if(name() == "ESRI Shapefile") {
    extensions.push_back(".dbf");
    extensions.push_back(".shx");
  }

  for(size_t i = 0; i < leaves.size(); ++i) {
    BOOST_FOREACH(std::string const& extension, extensions) {
      if(boost::algorithm::ends_with(leaves[i], extension)) {
        ids.push_back(i);
        break;
      }
    }
  }

  if(!ids.empty()) {
    for(int i = ids.size() - 1; i >= 0; --i) {
      leaves.erase(leaves.begin() + ids[i]);
    }
  }
}



void OgrFeatureDriver::read(
         void* cell,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  // FEATURE optimize, we only need to read a single record.
  // - determine feature id
  // - get value at record corresponding with this id.
  assert(typeId == TI_REAL4);
  assert(space.hasSpace());
  size_t index = space.indexOf(Space);
  SpatialCoordinate const& spatialAddress(
         address.coordinate<SpatialCoordinate>(index));

  DataSpace newSpace(space);
  DataSpaceAddress newAddress = newSpace.eraseCoordinates(address, Space);
  newSpace.eraseDimension(Space);

  boost::shared_ptr<FeatureLayer> layer(read(name, newSpace, newAddress,
         typeId));
  assert(layer);

  layer->value(spatialAddress, *static_cast<REAL4*>(cell));
}



void OgrFeatureDriver::browse(
         std::vector<BrowseInfo>& attributes,
         std::string const& location) const
{
  // Determine list of candidate file names of files to consider.
  boost::filesystem::path path(location);
  std::vector<std::string> leaves;
  possibleFileBasedAttributeFileNames(path, leaves);
  filterOutUnsupportedFileNames(leaves);

  std::string name; // , extension;
  // std::vector<size_t> ids;
  // boost::u32regex regex;
  // boost::u16match match;

  // std::vector<std::string> const& extensions(format().extensions());

  OGRwkbGeometryType geometryType;

  // Iterate over all files.
  for(int i = 0; i < int(leaves.size()); ++i) {
    auto dataset = static_cast<GDALDataset*>(GDALOpenEx(
        (path / leaves[i]).string().c_str(), GDAL_OF_VECTOR, _driver_names,
        NULL, NULL));

    if(dataset) {
      // Ok, this is a feature layer data set.

      // Loop over all layers.
      for(int l = 0; l < dataset->GetLayerCount(); ++l) {
        // Layer is owned by the data source.
        OGRLayer* ogrLayer = dataset->GetLayer(l);
        assert(ogrLayer);

        if(ogrLayer->GetFeatureCount() <= 0) {
          continue;
        }

        // Loop over all attributes.

        // Feature definition is owned by the layer.
        OGRFeatureDefn* featureDefinition = ogrLayer->GetLayerDefn();

        // TODO Only handle supported geometry types.
        geometryType = featureDefinition->GetGeomType();

        if(geometryType == wkbNone || geometryType == wkbUnknown) {
          continue;
        }

        for(int f = 0; f < featureDefinition->GetFieldCount(); ++f) {
          OGRFieldDefn* fieldDefinition = featureDefinition->GetFieldDefn(f);

          name = leaves[i] + "/" + featureDefinition->GetName() + "/" +
              fieldDefinition->GetNameRef();

          DataSpace space;
          DataSpaceAddress address;

          // TODO Determine value scale.
          // TODO Determine data space.
          attributes.push_back(BrowseInfo(name, space, FEATURE,
              TI_NR_TYPES, VS_NOTDETERMINED, this->name()));
        }
      }

      assert(dataset);
      GDALClose(dataset);
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

