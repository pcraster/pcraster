#include "ag_DataObject.h"

// Library headers.
#include <algorithm>
#include <iterator>
#include <set>
#include <sstream>
#include <boost/format.hpp>
#include <QPoint>

// PCRaster library headers.
#include "com_rawpalette.h"
#include "dal_Client.h"
#include "dal_Dal.h"
#include "dal_Dataset.h"
#include "dal_DataSpaceAddressMapper.h"
#include "dal_Raster.h"
#include "dal_SpaceDimensions.h"
#include "dal_SpaceStepCoordinateMapper.h"
#include "dal_StackInfo.h"
#include "dal_StepCoordinateMapper.h"
#include "dal_Table.h"
#include "dal_TimeStepCoordinateMapper.h"
#include "dal_Utils.h"
#include "com_exception.h"
#include "qt_Animation.h"

// Module headers.
#include "AguilaXSD.h"



/*!
  \file
  This file contains the implementation of the DataObject class.
*/


namespace ag {

//------------------------------------------------------------------------------

class DataObjectPrivate: private boost::noncopyable
{
public:

  bool             d_notifyNeeded;

  TableDataSources d_tableDataSources;

  RasterDataSources d_rasterDataSources;

  FeatureDataSources d_featureDataSources;

  VectorDataSources _vectorDataSources;

  dal::DataSpace   d_dataSpace;

  dal::DataSpaceAddress d_dataSpaceAddress;

  double           d_map2DZoom;
  double           d_map2DScale;

  QPointF          _map2DOffset;

  size_t           d_quadLength;
  double           d_map3DScale;

  DataProperties   d_properties;

  qt::Animation    d_animManager;

  dal::DataSpaceAddressMapper d_globalToWorldMapper;

  //! If valid, the default background colour of a visualisation.
  QColor           d_backgroundColor;

  boost::any       _selectedValue;

  DataObjectPrivate()
    : d_notifyNeeded(false),
      d_map2DZoom(1.0), d_map2DScale(0.0), d_quadLength(1), d_map3DScale(1.0),
      d_animManager(300)
  {
  }


  ~DataObjectPrivate()
  {
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAOBJECT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAOBJECT MEMBERS
//------------------------------------------------------------------------------

DataObject::DataObject()

  : QObject(), VisSubject(),
    d_data(new DataObjectPrivate())

{
  connect(&d_data->d_animManager, SIGNAL(process(size_t)),
         this, SLOT(setTimeStep(size_t)));
}



DataObject::~DataObject()
{
  delete d_data;

  for(size_t i = 0; i < d_palettesFromXML.size(); ++i) {
    delete d_palettesFromXML[i];
  }
}



int DataObject::firstTimeStep() const
{
  return d_data->d_animManager.nrSteps()
         ? static_cast<int>(d_data->d_animManager.firstStep())
         : -1;
}



int DataObject::lastTimeStep() const
{
  return d_data->d_animManager.nrSteps()
         ? static_cast<int>(d_data->d_animManager.lastStep())
         : -1;
}



size_t DataObject::timeSpan() const
{
  return d_data->d_animManager.timeSpan();
}



void DataObject::read()
{
  std::vector<std::string> const& scenarios(dal::scenarios(dataSpace()));

  if(scenarios.empty()) {
    tableDataSources().read(dataSpace(), dataSpaceAddress());
    rasterDataSources().read(dataSpace(), dataSpaceAddress());
    featureDataSources().read(dataSpace(), dataSpaceAddress());
    vectorDataSources().read(dataSpace(), dataSpaceAddress());
  }
  else {
    size_t index = dataSpace().indexOf(dal::Scenarios);
    dal::DataSpaceAddress address(dataSpaceAddress());

    // Loop over all scenarios.
    for(size_t i = 0; i < scenarios.size(); ++i) {
      std::string const& scenario(scenarios[i]);

      address.setCoordinate<std::string>(index, scenario);

      tableDataSources().read(dataSpace(), address);
      rasterDataSources().read(dataSpace(), address);
      featureDataSources().read(dataSpace(), address);
      vectorDataSources().read(dataSpace(), address);
    }
  }
}



void DataObject::setQuantile(
         float quantile,
         bool notify)
{
  dal::DataSpace const& space(d_data->d_dataSpace);

  if(space.hasCumProbabilities()) {
    size_t index = space.indexOf(dal::CumulativeProbabilities);
    dal::DataSpaceAddress address(d_data->d_dataSpaceAddress);
    address.setCoordinate<float>(index, quantile);
    setDataSpaceAddress(address, notify);
  }
}



void DataObject::setTimeStep(
         size_t time)
{
  dal::DataSpace const& space(d_data->d_dataSpace);

  if(space.hasTime()) {
    size_t index = space.indexOf(dal::Time);
    dal::DataSpaceAddress address(d_data->d_dataSpaceAddress);
    address.setCoordinate<size_t>(index, time);
    setDataSpaceAddress(address);
  }
}



void DataObject::setXY(
         double x,
         double y,
         bool notify)
{
  dal::SpaceDimensions envelope(this->envelope());

  if(envelope.contains(x, y)) {
    // Coordinates fall within the area for which we have data.
    dal::SpatialCoordinate spatialAddress(x, y);
    dal::DataSpace const& space(d_data->d_dataSpace);
    size_t index = space.indexOf(dal::Space);
    dal::DataSpaceAddress address(d_data->d_dataSpaceAddress);

    address.setCoordinate<dal::SpatialCoordinate>(index, spatialAddress);

    if(space.hasRaster() && space.hasFeatures()) {
      // Make sure the spatial addresses in raster and feature space are
      // equal.
      ++index;
      assert(space.dimension(index).meaning() == dal::Space);
      assert(space.dimension(index).discretisation() ==
         dal::BorderedDiscretisation);
      address.setCoordinate<dal::SpatialCoordinate>(index, spatialAddress);
    }

    setDataSpaceAddress(address, notify);
  }
  else {
    unsetCoordinates(dal::Space, notify);
  }
}



void DataObject::unsetCoordinates(
         dal::Meaning meaning,
         bool notify)
{
  dal::DataSpaceAddress address(
         dataSpace().unsetCoordinates(dataSpaceAddress(), meaning));

  setDataSpaceAddress(address, notify);
}



void DataObject::setDataSpaceAddress(
         dal::DataSpaceAddress const& address,
         bool notify)
{
  dal::DataSpace const& space = d_data->d_dataSpace;
  dal::DataSpaceAddress& currentAddress = d_data->d_dataSpaceAddress;

  bool addressChanged = false;
  bool timeChanged = false;

  size_t indexOfTime = space.indexOf(dal::Time);

  if(!space.equal(currentAddress, address)) {
    addressChanged = true;

    if(indexOfTime < space.rank()) {
      if(currentAddress.coordinate<size_t>(indexOfTime) !=
         address.coordinate<size_t>(indexOfTime)) {
        timeChanged = true;
      }
    }
  }

  // Now set current status. Not sooner!
  if(addressChanged) {
    d_data->d_dataSpaceAddress = address;
  }

  if(timeChanged) {
    d_data->d_animManager.setCurrent(
         d_data->d_dataSpaceAddress.coordinate<size_t>(indexOfTime));
  }

  if(addressChanged) {
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setMap2DZoom(
         double zoom,
         bool notify)
{
  // Limit zoom level to 1/1000 which is already crazy small.
  zoom = std::max(zoom, 0.001);

  if(d_data->d_map2DZoom != zoom) {
    d_data->d_map2DZoom = zoom;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::map2DZoomBy(
         double factor,
         bool notify)
{
  setMap2DZoom(d_data->d_map2DZoom * factor, notify);
}



void DataObject::setMap2DScale(
         double scale,
         bool notify)
{
  if(!dal::comparable(d_data->d_map2DScale, scale)) {
    d_data->d_map2DScale = scale;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



//!
/*!
  \tparam    .
  \param     dx Number of world coordinates to move.
  \param     dy Number of world coordinates to move.
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void DataObject::map2DMoveBy(
         double dx,
         double dy,
         bool notify)
{
  map2DMoveBy(QPointF(dx, dy), notify);
}



void DataObject::map2DMoveBy(
         QPointF const& offset,
         bool notify)
{
  if(!dal::comparable(offset.x(), 0.0) || !dal::comparable(offset.y(), 0.0)) {
    d_data->_map2DOffset += offset;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setQuadLength(
         size_t quadLength,
         bool notify)
{
  if(d_data->d_quadLength != quadLength) {
    d_data->d_quadLength = quadLength;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setMap3DScale(
         double scale,
         bool notify)
{
  if(!dal::comparable(d_data->d_map3DScale, scale)) {
    d_data->d_map3DScale = scale;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



dal::DataSpace const& DataObject::dataSpace() const
{
  return d_data->d_dataSpace;
}



dal::DataSpace DataObject::dataSpace(
         DataGuide const& guide) const
{
  assert(isValid(guide));

  dal::DataSpace result;

  switch(guide.type()) {
    case geo::STACK: {
      result = rasterDataSources().data(guide).dataSpace();
      break;
    }
    case geo::FEATURE: {
      result = featureDataSources().data(guide).dataSpace();
      break;
    }
    case geo::VECTOR: {
      result = vectorDataSources().data(guide).dataSpace();
      break;
    }
    case geo::TIMESERIES: {
      result = tableDataSources().data(guide).dataSpace();
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



dal::DataSpace DataObject::dataSpace(
         std::vector<DataGuide> const& guides) const
{
  dal::DataSpace result;

  for(size_t i = 0; i < guides.size(); ++i) {
    result |= dataSpace(guides[i]);
  }

  return result;
}



dal::DataSpaceAddress const& DataObject::dataSpaceAddress() const
{
  return d_data->d_dataSpaceAddress;
}



double DataObject::map2DZoom() const
{
  return d_data->d_map2DZoom;
}



double DataObject::map2DScale() const
{
  return d_data->d_map2DScale;
}



QPointF const& DataObject::map2DOffset() const
{
  return d_data->_map2DOffset;
}



size_t DataObject::quadLength() const
{
  return d_data->d_quadLength;
}



double DataObject::map3DScale() const
{
  return d_data->d_map3DScale;
}



bool DataObject::isValid(
         DataGuide const& guide) const
{
  bool result = false;

  switch(guide.type()) {
    case geo::TIMESERIES: {
      result = tableDataSources().isValid(guide);
      break;
    }
    case geo::FEATURE: {
      result = featureDataSources().isValid(guide);
      break;
    }
    case geo::STACK: {
      result = rasterDataSources().isValid(guide);
      break;
    }
    case geo::VECTOR: {
      result = vectorDataSources().isValid(guide);
      break;
    }
    default: {
      assert(false);
    }
  }

  return result;
}



//! Returns the name of data source \a guide.
/*!
  \param     guide Data source to return the name for.
  \return    name
  \sa        DataObjectBase::name(DataGuide const&).
*/
std::string DataObject::name(
         DataGuide const& guide) const
{
  std::string name;

  if(guide.type() == geo::TIMESERIES) {
    name = tableDataSources().name(guide);
  }
  else if(guide.type() == geo::STACK) {
    name = rasterDataSources().name(guide);
  }
  else if(guide.type() == geo::FEATURE) {
    name = featureDataSources().name(guide);
  }
  else if(guide.type() == geo::VECTOR) {
    name = vectorDataSources().name(guide);
  }
  else {
    assert(false);
  }

  assert(!name.empty());

  return name;
}



std::string DataObject::description(
         DataGuide const& guide) const
{
  std::string result;
  dal::DataSpace space;

  if(guide.type() == geo::TIMESERIES) {
    Table const& table = tableDataSources().data(guide);
    space = table.dataSpace();
    result = table.name();
  }
  else if(guide.type() == geo::STACK) {
    Raster const& raster = rasterDataSources().data(guide);
    space = raster.dataSpace();
    result = raster.name();
  }
  else if(guide.type() == geo::FEATURE) {
    FeatureLayer const& layer = featureDataSources().data(guide);
    space = layer.dataSpace();
    result = layer.name();
  }
  else if(guide.type() == geo::VECTOR) {
    Vector const& vector = vectorDataSources().data(guide);
    space = vector.dataSpace();
    result = vector.name();
  }
  else {
    assert(false);
  }

  if(space.hasScenarios()) {
    size_t index = space.indexOf(dal::Scenarios);
    assert(space.dimension(index).nrValues() == 1);
    std::string scenario = space.dimension(index).value<std::string>(0);
    result += " (" + scenario + ")";
  }

  assert(!result.empty());

  return result;
}



//! Returns the title of the data source as set in the draw properties of \a guide.
/*!
  \param     guide Data source to return the title for.
  \return    title

  The default title set in the draw propties of the data source is the name
  of the data source as returned by name(DataGuide const&). When present this
  title is overridden by a legend or similar. This logic is implemented in the
  DataProperties class.
*/
std::string DataObject::title(
         DataGuide const& guide) const
{
  return properties().drawProperties(guide).title();
}



std::string DataObject::label(
         DataGuide const& guide) const
{
  std::string result;

  switch(guide.type()) {
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
        case VS_LDD:
          UINT1 value;
          this->value<UINT1>(value, guide);
          result = properties().label<UINT1>(guide, value);
          break;
        }
        case VS_NOMINAL:
        case VS_ORDINAL: {
          INT4 value;
          this->value<INT4>(value, guide);
          result = properties().label<INT4>(guide, value);
          break;
        }
        case VS_SCALAR:
        case VS_DIRECTION: {
          REAL4 value;
          this->value<REAL4>(value, guide);
          result = properties().label<REAL4>(guide, value);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    case geo::FEATURE: {
      FeatureLayer const& layer(featureDataSources().data(guide));

      if(layer.hasAttribute()) {
        switch(guide.valueScale()) {
          case VS_BOOLEAN: {
            UINT1 value;
            this->value<UINT1>(value, guide);
            result = properties().label<UINT1>(guide, value);
            break;
          }
          case VS_NOMINAL:
          case VS_ORDINAL: {
            INT4 value;
            this->value<INT4>(value, guide);
            result = properties().label<INT4>(guide, value);
            break;
          }
          case VS_SCALAR: {
            REAL4 value;
            this->value<REAL4>(value, guide);
            result = properties().label<REAL4>(guide, value);
            break;
          }
          default: {
            assert(false);
            break;
          }
        }
      }

      break;
    }
    case geo::VECTOR: {
      REAL4 value;
      this->value<REAL4>(value, guide);
      result = properties().label<REAL4>(guide, value);

      break;
    }
    case geo::TIMESERIES: {
      REAL4 value;
      this->value<REAL4>(value, guide);
      result = properties().label<REAL4>(guide, value);

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



TableDataSources& DataObject::tableDataSources()
{
  return d_data->d_tableDataSources;
}




TableDataSources const& DataObject::tableDataSources() const
{
  return d_data->d_tableDataSources;
}




RasterDataSources& DataObject::rasterDataSources()
{
  return d_data->d_rasterDataSources;
}



RasterDataSources const& DataObject::rasterDataSources() const
{
  return d_data->d_rasterDataSources;
}



FeatureDataSources& DataObject::featureDataSources()
{
  return d_data->d_featureDataSources;
}



FeatureDataSources const& DataObject::featureDataSources() const
{
  return d_data->d_featureDataSources;
}



VectorDataSources& DataObject::vectorDataSources()
{
  return d_data->_vectorDataSources;
}



VectorDataSources const& DataObject::vectorDataSources() const
{
  return d_data->_vectorDataSources;
}






template<>
bool DataObject::compatibleData<dal::Raster>(
         dal::Raster const& /* raster */,
         dal::DataSpace const& /* space */)
{
  return true;
  // return compatibleRasterData(raster);
}



template<>
bool DataObject::compatibleData<dal::Table>(
         dal::Table const& /* table */,
         dal::DataSpace const& /* space */)
{
  return true;
}



//! Returns true if data in \a fileName is compatible with data in data object.
/*!
  \param     fileName File name of file with data.
  \return    true or false.

  \todo      Improve code so that compatibleRasterData succeeds when opening
             a raster does not give us all the information about the raster.
             Maybe we need a type of open which determines all header
             information without storing the values in memory yet?
             Seems a bit inefficient though.
*/
bool DataObject::compatibleData(
         std::string const& name,
         dal::DataSpace const& space)
{
  {
    dal::DataSpaceQueryResult result;
    boost::tie(result, boost::tuples::ignore) = dal::Client::dal().search(name,
         dal::RASTER, space, dal::NarrowSpaceWhenNeeded,
         dal::HaltOnFirstItemFound);

    if(result) {
      return true;

      /// FEATURE what about this function at all. Is compatibility an issue?
      /// dal::Driver* driver = dal::Client::dal().driverByDataset(name, space);
      /// assert(driver);

      /// std::auto_ptr<dal::Raster> raster(dynamic_cast<dal::Raster*>(
      ///    driver->open(name, space, result.address())));
      /// assert(raster.get());

      /// return compatibleRasterData(*raster.get());
    }
  }

  {
    dal::DataSpaceQueryResult result;
    boost::tie(result, boost::tuples::ignore) = dal::Client::dal().search(name,
         dal::TABLE, space, dal::NarrowSpaceWhenNeeded,
         dal::HaltOnFirstItemFound);

    if(result) {
      return true;
    }
  }

  return false;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make variants which also take driver and address as arguments.
             Will speed up things somewhat. See Viewer::displayTimePlot which
             will benefit from this.
*/
DataGuide DataObject::add(
         std::string const& name,
         dal::DataSpace const& space)
{
  dal::DatasetType type = dal::Client::dal().datasetType(name, space);
  DataGuide guide;

  switch(type) {
    case dal::RASTER: {
      guide = addStack(name, space);
      break;
    }
    case dal::FEATURE: {
      guide = addFeatureLayer(name, space);
      break;
    }
    case dal::TABLE: {
      guide = addTimeSeries(name, space);
      break;
    }
    case dal::VECTOR: {
      guide = addVector(name, space);
      break;
    }
    case dal::MATRIX: {
      dal::throwCannotBeOpened(name, type);
      break;
    }
    default: {
      dal::throwCannotBeOpened(name);
      break;
    }
  }

  // Data has just been added. It might be that data has been added for a
  // dimension which had an invalid coordinate untill now, for example because
  // a time dimension was set up but no dynamic data was added yet. Let the
  // space object assign a valid initial coordinate in those cases. Valid
  // coordinates retain their value.

  // It might also be that the new data triggered the addition of a new
  // dimension. For now we reset the address. This is a bit rude, because
  // current cursor information is lost.

  dal::DataSpaceAddress& address(d_data->d_dataSpaceAddress);

  if(d_data->d_dataSpace.size() != address.size()) {
    address = d_data->d_dataSpace.address();
    // Mmm, assigns a scenario coordinate too. We need some way to deal with
    // scenarios. The address should mean the same address for each scenario.
    address = d_data->d_dataSpace.initialiseInvalidCoordinates(address);
  }
  else if(!d_data->d_dataSpace.isValid(address)) {
    address = d_data->d_dataSpace.initialiseInvalidCoordinates(address);
  }

  /*
  if(dataSpace.hasScenarios()) {
    size_t index = dataSpace.indexOf(dal::Scenarios);
    address.setCoordinate<std::string>(index, "*");
  }
  */

  // reconfigureAnimationManager();

  // if(d_data->d_dataSpace.hasTime()) {
  //   size_t index = d_data->d_dataSpace.indexOf(dal::Time);
  //   if(address.isValid(index)) {
  //     d_data->d_animManager.setCurrent(address.coordinate<size_t>(index));
  //   }
  // }

  setNotifyNeeded(true);

  return guide;
}



/*
DataGuide DataObject::add(std::string const& name)
{
  dal::DatasetType type = d_data->d_dal.datasetType(name);
  DataGuide guide;

  switch(type) {
    case dal::RASTER: {
      guide = addStack(name);
      break;
    }
    case dal::TABLE: {
      guide = addTimeSeries(name);
      break;
    }
    case dal::MATRIX: case dal::FEATURE: {
      dal::throwNotAvailable(name, type);
    }
    default: {
      dal::throwNotAvailable(name);
    }
  }

  setNotifyNeeded(true);

  return guide;
}
*/



/*!
  \brief     Adds stack with name \a pn to data object and returns a DataGuide
             object for it.
  \param     pn Name of the stack to add.
  \return    DataGuide for stack \a pn.
*/
DataGuide DataObject::addStack(
         std::string const& name,
         dal::DataSpace const& space)
{
  DataGuide guide = rasterDataSources().add(name, space);

  Raster const& raster(rasterDataSources().data(guide));

  // FEATURE make sure the new space is compatible with the existing.
  // FEATURE see check below in this function
  // FEATURE see compatibleData and friends
  d_data->d_dataSpace |= raster.dataSpace();

  switch(guide.valueScale()) {
    case VS_BOOLEAN: {
      d_data->d_properties.addBooleanStackProperties(*this, guide);
      break;
    }
    case VS_NOMINAL: {
      d_data->d_properties.addNominalStackProperties(*this, guide);
      break;
    }
    case VS_ORDINAL: {
      d_data->d_properties.addOrdinalStackProperties(*this, guide);
      break;
    }
    case VS_SCALAR: {
      d_data->d_properties.addScalarStackProperties(*this, guide);
      break;
    }
    case VS_DIRECTION: {
      d_data->d_properties.addDirectionalStackProperties(*this, guide);
      break;
    }
    case VS_LDD: {
      d_data->d_properties.addLddStackProperties(*this, guide);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  // TMP ----------------------------
  // static size_t nr = 0;
  // dal::DataSpace const& subSpace(rasterDataSources().data(guide).dataSpace());
  // if(subSpace.hasTime()) {
  //   namespace bp = boost::posix_time;
  //   namespace bg = boost::gregorian;
  //   size_t id = subSpace.indexOf(dal::Time);
  //   dal::Dimension const& dimension(subSpace.dimension(id));
  //   size_t first = dimension.value<size_t>(0);
  //   if(nr == 0) {
  //     localToWorldMapper(guide).setMapper(id,
  //            new dal::TimeStepCoordinateMapper(first,
  //            bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
  //            bp::time_duration(0, 0, 0, 0)),
  //            bp::time_duration(24, 0, 0, 0)));
  //   }
  //   else if(nr == 1) {
  //     localToWorldMapper(guide).setMapper(id,
  //            new dal::TimeStepCoordinateMapper(first,
  //            bp::ptime(bg::date(2006, boost::gregorian::Jan, 1),
  //            bp::time_duration(0, 0, 0, 0)),
  //            bp::time_duration(6, 0, 0, 0)));
  //   }
  //   ++nr;
  // }
  // /TMP ----------------------------

  reconfigureDataSpaceAndMappers();

  return guide;
}



DataGuide DataObject::addFeatureLayer(
         std::string const& name,
         dal::DataSpace const& space)
{
  DataGuide guide = featureDataSources().add(name, space);

  FeatureLayer const& layer(featureDataSources().data(guide));

  d_data->d_dataSpace |= layer.dataSpace();

  if(!layer.hasAttribute()) {
    d_data->d_properties.addGeometryDataProperties(*this, guide);
  }
  else {
    switch(guide.valueScale()) {
      case VS_BOOLEAN: {
        d_data->d_properties.addBooleanFeatureProperties(*this, guide);
        break;
      }
      case VS_NOMINAL: {
        d_data->d_properties.addNominalFeatureProperties(*this, guide);
        break;
      }
      case VS_ORDINAL: {
        d_data->d_properties.addOrdinalFeatureProperties(*this, guide);
        break;
      }
      case VS_SCALAR: {
        d_data->d_properties.addScalarFeatureProperties(*this, guide);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  reconfigureDataSpaceAndMappers();

  return guide;
}



DataGuide DataObject::addTimeSeries(
         std::string const& name,
         dal::DataSpace const& space)
{
  DataGuide guide = tableDataSources().add(name, space);

  d_data->d_dataSpace |= tableDataSources().data(guide).dataSpace();
  d_data->d_properties.addScalarTimeSeriesProperties(*this, guide);

  reconfigureDataSpaceAndMappers();

  return guide;
}



DataGuide DataObject::addVector(
         std::string const& name,
         dal::DataSpace const& space)
{
  DataGuide guide = vectorDataSources().add(name, space);

  Vector const& vector(vectorDataSources().data(guide));

  d_data->d_dataSpace |= vector.dataSpace();
  d_data->d_properties.addVectorProperties(*this, guide);

  reconfigureDataSpaceAndMappers();

  return guide;
}



void DataObject::clear()
{
  if(nrDataSets()) {
    tableDataSources().clear();
    rasterDataSources().clear();
    featureDataSources().clear();
    vectorDataSources().clear();
    setNotifyNeeded(true);
    notify();
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Update data space, some dimension(s) might be removed now. Update
             configuration of animation manager.
*/
void DataObject::remove(
         DataGuide const& guide)
{
  if(guide.type() == geo::TIMESERIES) {
    tableDataSources().remove(guide);
  }
  else if(guide.type() == geo::STACK) {
    rasterDataSources().remove(guide);
  }
  else if(guide.type() == geo::FEATURE) {
    featureDataSources().remove(guide);
  }
  else if(guide.type() == geo::VECTOR) {
    vectorDataSources().remove(guide);
  }
  else {
    assert(false);
  }

  // d_data->d_animManager.setSteps(timeSteps());

  // FIXME: What about reference counting? This data guide might be still in
  // FIXME: use.
  d_data->d_properties.remove(guide);
}



const DataGuide& DataObject::dataGuide(
         geo::DataGuide const& guide) const
{
  const DataGuide* result = 0;

  if(guide.type() == geo::TIMESERIES) {
    result = &tableDataSources().dataGuide(guide);
  }
  else if(guide.type() == geo::STACK) {
    result = &rasterDataSources().dataGuide(guide);
  }
  else if(guide.type() == geo::FEATURE) {
    result = &featureDataSources().dataGuide(guide);
  }
  else if(guide.type() == geo::VECTOR) {
    result = &vectorDataSources().dataGuide(guide);
  }
  else {
    assert(false);
  }

  assert(result);
  return *result;
}



size_t DataObject::nrDataSets() const
{
  size_t nr = 0;

  nr += tableDataSources().size();
  nr += nrSpatialDataSets();

  return nr;
}



size_t DataObject::nrSpatialDataSets() const
{
  size_t nr = 0;

  nr += rasterDataSources().size();
  nr += featureDataSources().size();
  nr += vectorDataSources().size();

  return nr;
}



std::vector<DataGuide> DataObject::dataGuides() const
{
  std::vector<DataGuide> guides;
  guides.insert(guides.end(), tableDataSources().guides_begin(),
         tableDataSources().guides_end());
  guides.insert(guides.end(), rasterDataSources().guides_begin(),
         rasterDataSources().guides_end());
  guides.insert(guides.end(), featureDataSources().guides_begin(),
         featureDataSources().guides_end());
  guides.insert(guides.end(), vectorDataSources().guides_begin(),
         vectorDataSources().guides_end());
  return guides;
}



Dataset& DataObject::dataset(
         DataGuide const& guide)
{
  Dataset* result = 0;

  switch(guide.type()) {
    case geo::STACK: {
      result = &rasterDataSources().data(guide);
      break;
    }
    case geo::FEATURE: {
      result = &featureDataSources().data(guide);
      break;
    }
    case geo::VECTOR: {
      result = &vectorDataSources().data(guide);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  assert(result);
  return *result;
}



//! Sets the enabled status of the dataset referenced by \a guide.
/*!
  \param     guide Guide of dataset to {en,dis}able.
  \param     enabled Status.

  When the enabled status of the dataset changes, notify() is called.
*/
void DataObject::setEnabled(
         DataGuide const& guide,
         bool enabled)
{
  assert(isValid(guide));

  if(d_data->d_properties.isEnabled(guide) != enabled) {
    d_data->d_properties.setEnabled(guide, enabled);
    setNotifyNeeded(true);
  }

  notify();
}



void DataObject::setSelected(
         bool selected,
         bool notify)
{
  bool changed = false;

  // Loop over all data guides.
  for(DataProperties::const_iterator it = d_data->d_properties.begin();
                   it != d_data->d_properties.end(); ++it) {

    assert(isValid(*it));

    if(d_data->d_properties.isSelected(*it) != selected) {

      d_data->d_properties.setSelected(*it, selected);
      changed = true;
    }
  }

  if(changed) {
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setSelected(
         DataGuide const& guide,
         bool selected,
         bool notify)
{
  assert(isValid(guide));

  if(d_data->d_properties.isSelected(guide) != selected) {
    d_data->d_properties.setSelected(guide, selected);
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setSelected(
         std::vector<DataGuide> const& guides,
         bool selected)
{
  bool changed = false;

  for(std::vector<DataGuide>::const_iterator it = guides.begin();
         it != guides.end(); ++it) {

    assert(isValid(*it));

    if(d_data->d_properties.isSelected(*it) != selected) {
      d_data->d_properties.setSelected(*it, selected);
      changed = true;
    }
  }

  if(changed) {
    setNotifyNeeded(true);
  }

  notify();
}


void DataObject::setPalette(
         DataGuide const& guide,
         com::RawPalette const* palette,
         bool notify)
{
  assert(palette);

  if(properties().palette(guide) != palette) {
    properties().setPalette(guide, palette);
    setNotifyNeeded(true);
  }

  if(notify) {
   this->notify();
  }
}


double DataObject::maxCutoff(
         DataGuide const& guide) const
{
  return this->properties().rangeDrawProperties(guide).maxCutoff();
}



double DataObject::minCutoff(
         DataGuide const& guide) const
{
  return this->properties().rangeDrawProperties(guide).minCutoff();
}



size_t DataObject::nrClasses(
         DataGuide const& guide) const
{
  return this->properties().rangeDrawProperties(guide).nrClasses();
}



//! set draw properties from XML
/*!
 * all yet defined pcrxml::DrawProperties sub elements are supported, except
 * for those listed in this method's todo section
 *
 * \todo pallette
 * \todo confidenceLevel (of colourAssignment)
 */
void DataObject::setXML(
         DataGuide const& guide,
         pcrxml::DrawProperties const& dp,
         bool notify)
{
   if (dp.legendBorderValuesType().present()) {
     pcrxml::LegendBorderValuesType const &l(dp.legendBorderValuesType().get());
     if (l.rounded().present())
      setClassificationMode(guide, com::Classifier::AUTO,false);
     if (l.exact().present())
      setClassificationMode(guide, com::Classifier::EXACT,false);
   }

   if (dp.minimumCutOff().present() && dp.maximumCutOff().present())
     setCutoffs(guide, dp.minimumCutOff().get(), dp.maximumCutOff().get(),false);
   if (dp.minimumCutOff().present())
     setMinCutoff(guide, dp.minimumCutOff().get(), false);
   if (dp.maximumCutOff().present())
     setMaxCutoff(guide, dp.maximumCutOff().get(), false);

   if (dp.numberOfColours().present())
     setNrClasses(guide, dp.numberOfColours().get(), false);
   if (dp.colourAssignment().present()) {
     pcrxml::ColourAssignment const &ca(dp.colourAssignment().get());
     if (ca.linear().present())
       setClassificationAlgorithm(guide,com::Classifier::LIN,false);
     if (ca.trueLogarithmic().present())
       setClassificationAlgorithm(guide,com::Classifier::LOG,false);
     if (ca.shiftedLogarithmic().present())
       setClassificationAlgorithm(guide,com::Classifier::TLOG,false);
     if (ca.confidenceLevel().present()) {
      // NOT YET configurable from here
     }
   }
   if (dp.drawMode().present()) {
     pcrxml::DrawMode const &dm(dp.drawMode().get());
     if (dm.fill())
      setDrawerType(guide, COLOURFILL,false);
     if (dm.contour())
      setDrawerType(guide, CONTOUR,false);
   }

   if(dp.palette().present()) {
     std::auto_ptr<com::RawPalette> palette(new com::RawPalette());
     palette->setMaximum(255);
     pcrxml::Palette const &xp(dp.palette().get());
     for(size_t i=0; i < xp.rgb().size(); ++i) {
       palette->insert(palette->end(), com::RgbTuple(
         xp.rgb()[i].r(),
         xp.rgb()[i].g(),
         xp.rgb()[i].b()));
     }

     d_palettesFromXML.push_back(palette.release());
     setPalette(guide, d_palettesFromXML.back(), false);
   }

   if(notify) {
     this->notify();
   }
}



void DataObject::setDateMapper(
         DataGuide const& guide,
         pcrxml::DateMapper const& dm,
         bool notify)
{
  // FEATURE what about the feature data? and vector
  dal::DataSpace const& space(rasterDataSources().data(guide).dataSpace());
  assert(space.hasTime());
  size_t id = space.indexOf(dal::Time);

  std::auto_ptr<dal::TimeStepCoordinateMapper> newMapper(
         new dal::TimeStepCoordinateMapper(dm.index(),
         pcrxsd::toPosixTime(dm.timeOfIndex()),
         pcrxsd::toPosixTimeDuration(dm.duration())));
  dal::TimeStepCoordinateMapper const* currentMapper =
         dynamic_cast<dal::TimeStepCoordinateMapper const*>(
         localToWorldMapper(guide).mapper(id));

  if(!currentMapper || *newMapper != *currentMapper) {
    localToWorldMapper(guide).setMapper(id, newMapper.release());
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setNrClasses(
         DataGuide const& guide,
         size_t nrClasses,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.nrClassesRequested() != nrClasses) {
    properties.setNrClasses(nrClasses);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}


void DataObject::setMaxCutoff(
         DataGuide const& guide,
         double maxCutoff,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.maxCutoff() != maxCutoff) {
    properties.setMaxCutoff(maxCutoff);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}




void DataObject::setMinCutoff(
         DataGuide const& guide,
         double minCutoff,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.minCutoff() != minCutoff) {
    properties.setMinCutoff(minCutoff);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setCutoffs(
         DataGuide const& guide,
         double minCutoff,
         double maxCutoff,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(!dal::comparable(properties.minCutoff(), minCutoff) ||
     !dal::comparable(properties.maxCutoff(), maxCutoff)) {
    properties.setCutoffs(minCutoff, maxCutoff);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setClassificationMode(
         DataGuide const& guide,
         com::Classifier::Mode mode,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.mode() != mode) {
    properties.setMode(mode);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setProbabilityScale(
         DataGuide const& guide,
         RangeDrawProps::ProbabilityScale scale,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.probabilityScale() != scale) {
    properties.setProbabilityScale(scale);
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setClassificationAlgorithm(
         DataGuide const& guide,
         RangeDrawProps::Algorithm algorithm,
         bool notify)
{
  RangeDrawProps& properties =
         this->properties().rangeDrawProperties(guide);

  if(properties.algorithm() != algorithm) {
    properties.setAlgorithm(algorithm);
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setClassificationProperties(
         DataGuide const& guide,
         RangeDrawProps::Algorithm algorithm,
         double minCutoff,
         double maxCutoff,
         bool notify)
{
  bool classifyNeeded = false;
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.algorithm() != algorithm) {
    properties.setAlgorithm(algorithm);
    classifyNeeded = true;
  }

  if(!dal::comparable(properties.minCutoff(), minCutoff) ||
     !dal::comparable(properties.maxCutoff(), maxCutoff)) {
    properties.setCutoffs(minCutoff, maxCutoff);
    classifyNeeded = true;
  }

  if(classifyNeeded) {
    properties.classify();
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::setDrawerType(
         DataGuide const& guide,
         DrawerType type,
         bool notify)
{
  RangeDrawProps& properties = this->properties().rangeDrawProperties(guide);

  if(properties.drawerType() != type) {
    properties.setDrawerType(type);
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::mergeDataProperties(
         DataGuide const& guide1,
         DataGuide const& guide2)
{
  d_data->d_properties.mergeDataProperties(guide1, guide2);

  // We don't know whether a notify is really needed, but to be sure, we signal
  // the object that the observers should check their state.
  setNotifyNeeded(true);
}



DataProperties& DataObject::properties()
{
  return d_data->d_properties;
}



const DataProperties& DataObject::properties() const
{
  return d_data->d_properties;
}



bool DataObject::isAvailable(
         DataGuide const& guide) const
{
  std::vector<DataGuide> guides = this->dataGuides();
  return std::find(guides.begin(), guides.end(), guide) != guides.end();
}



bool DataObject::isEnabled(
         DataGuide const& guide) const
{
  assert(isValid(guide));

  return d_data->d_properties.isEnabled(guide);
}



bool DataObject::isSelected(
         DataGuide const& guide) const
{
  assert(isValid(guide));

  return d_data->d_properties.isSelected(guide);
}



bool DataObject::hasSelectedData() const
{
  return d_data->d_properties.hasSelectedData();
}



std::vector<DataGuide> DataObject::selectedData() const
{
  return d_data->d_properties.selectedData();
}



REAL4 DataObject::selectedValue() const
{
  assert(hasSelectedValue());

  return boost::any_cast<REAL4>(d_data->_selectedValue);
}



bool DataObject::hasSelectedValue() const
{
  return !d_data->_selectedValue.empty();
}



void DataObject::setSelectedValue(
         REAL4 value,
         bool notify)
{
  if(!hasSelectedValue() || !dal::comparable(selectedValue(), value)) {
    d_data->_selectedValue = value;

    std::vector<DataGuide> guides(dataGuides());

    BOOST_FOREACH(DataGuide const& guide, guides) {
      Dataset& dataset(this->dataset(guide));

      if(dataset.dataSpace().hasCumProbabilities()) {
        dataset.setSelectedValue(value);
      }
    }

    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



void DataObject::unsetSelectedValue(
         bool notify)
{
  if(hasSelectedValue()) {
    d_data->_selectedValue = boost::any();

    std::vector<DataGuide> guides(dataGuides());

    BOOST_FOREACH(DataGuide const& guide, guides) {
      Dataset& dataset(this->dataset(guide));

      if(dataset.dataSpace().hasCumProbabilities()) {
        dataset.unsetSelectedValue();
      }
    }

    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



// bool DataObject::hasSelectedValue(
//          DataGuide const& guide) const
// {
//   bool result = false;
// 
//   if(guide.valueScale() == VS_SCALAR) {
//     if(guide.type() == geo::STACK) {
//       result = rasterDataSources().data(guide).hasSelectedValue();
//     }
//     else if(guide.type() == geo::FEATURE) {
//       result = featureDataSources().data(guide).hasSelectedValue();
//     }
//   }
// 
//   return result;
// }



// void DataObject::unsetSelectedValue(
//          DataGuide const& guide,
//          bool notify)
// {
//   if(hasSelectedValue(guide)) {
//     if(guide.type() == geo::STACK) {
//       rasterDataSources().data(guide).unsetSelectedValue();
//     }
//     else if(guide.type() == geo::FEATURE) {
//       featureDataSources().data(guide).unsetSelectedValue();
//     }
// 
//     setNotifyNeeded(true);
//   }
// 
//   if(notify) {
//     this->notify();
//   }
// }



void DataObject::replaceClassifier(
         DataGuide const& guide,
         com::Classifier const& classifier,
         bool notify)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  properties().replaceClassifier(guide, classifier);
  setNotifyNeeded(true);

  if(notify) {
    this->notify();
  }
}



void DataObject::pushClassifier(
         DataGuide const& guide,
         com::Classifier const& classifier,
         bool notify)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  properties().pushClassifier(guide, classifier);
  setNotifyNeeded(true);

  if(notify) {
    this->notify();
  }
}



void DataObject::popClassifier(
         DataGuide const& guide,
         bool notify)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  properties().popClassifier(guide);
  setNotifyNeeded(true);

  if(notify) {
    this->notify();
  }
}



void DataObject::popClassifiers(
         DataGuide const& guide,
         bool notify)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  properties().popClassifiers(guide);
  setNotifyNeeded(true);

  if(notify) {
    this->notify();
  }
}



qt::Animation& DataObject::animationManager() const
{
  return d_data->d_animManager;
}



void DataObject::notify()
{
  if(notifyNeeded()) {
    reconfigureDataSpaceAndMappers();
    reconfigureAnimationManager();
    read();
    VisSubject::notify();
    postNotify();
  }
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  - A notify is not needed anymore after this call.
*/
void DataObject::postNotify()
{
  setNotifyNeeded(false);
}



//! Sets whether a notify is needed to update the observers.
/*!
  \param     needed What's it gonna be boy? True or false.
  \sa        notifyNeeded()
*/
void DataObject::setNotifyNeeded(
         bool needed)
{
  d_data->d_notifyNeeded = needed;
}



//! Returns whether a notify is needed to update the observers.
/*!
  \return    True or false
  \sa        setNotifyNeeded(bool)
*/
bool DataObject::notifyNeeded() const
{
  return d_data->d_notifyNeeded;
}


void DataObject::reconfigureAnimationManager()
{
  dal::DataSpace const& space(d_data->d_dataSpace);
  dal::DataSpaceAddress const& address(d_data->d_dataSpaceAddress);

  if(space.hasTime()) {
    size_t index = space.indexOf(dal::Time);
    if(address.isValid(index)) {
      d_data->d_animManager.setRangeOfSteps(
             space.dimension(index).value<size_t>(0),
             space.dimension(index).value<size_t>(1),
             space.dimension(index).value<size_t>(2));
      d_data->d_animManager.setCurrent(address.coordinate<size_t>(index));
    }
  }
}



bool DataObject::hasSpace(
         DataGuide const& guide) const
{
  bool result = false;
  dal::DataSpace const& space = d_data->d_dataSpace;

  if(space.hasSpace()) {
    // At least some datasets have space. Let's check this one has.
    result = guide.type() == geo::STACK || guide.type() == geo::FEATURE;
  }

  return result;
}



bool DataObject::hasTimeSeries(
         DataGuide const& guide) const
{
  dal::DataSpace const& space = d_data->d_dataSpace;

  if(space.hasTime()) {
    // At least some datasets have time. Let's check this one has.
    if(guide.valueScale() == VS_SCALAR) {
      if(guide.type() == geo::STACK) {
        Raster const& raster = rasterDataSources().data(guide);
        return raster.dataSpace().hasTime(); // Dynamic stack.
      }
      else if(guide.type() == geo::FEATURE) {
        FeatureLayer const& layer = featureDataSources().data(guide);
        return layer.dataSpace().hasTime();
      }
      else if(guide.type() == geo::VECTOR) {
        Vector const& vector = vectorDataSources().data(guide);
        return vector.dataSpace().hasTime();
      }
      else if(guide.type() == geo::TIMESERIES) {
        return true;
      }
    }
  }

  return false;
}



bool DataObject::hasCumProbabilities(
         DataGuide const& guide) const
{
  dal::DataSpace const& space = d_data->d_dataSpace;

  if(space.hasCumProbabilities()) {
    if(guide.valueScale() == VS_SCALAR) {
      if(guide.type() == geo::STACK) {
        Raster const& raster = rasterDataSources().data(guide);
        return raster.dataSpace().hasCumProbabilities(); // Quantiles.
      }
      else if(guide.type() == geo::FEATURE) {
        FeatureLayer const& layer = featureDataSources().data(guide);
        return layer.dataSpace().hasCumProbabilities();
      }
    }
  }

  return false;
}



dal::DataSpaceAddressMapper const& DataObject::globalToWorldMapper() const
{
  return d_data->d_globalToWorldMapper;
}



//! Returns the mapper which maps local coordinates to real world coordinates for data set \a guide.
/*!
  \param     guide Data set to return mapper for.
  \return    Data space address mapper.
  \exception .
  \warning   .
  \sa        .
*/
dal::DataSpaceAddressMapper& DataObject::localToWorldMapper(
         DataGuide const& guide) const
{
  dal::DataSpaceAddressMapper* result = 0;

  switch(guide.type()) {
    case geo::STACK: {
      result = &d_data->d_rasterDataSources.data(guide).localToWorldMapper();
      break;
    }
    case geo::FEATURE: {
      result = &d_data->d_featureDataSources.data(guide).localToWorldMapper();
      break;
    }
    case geo::VECTOR: {
      result = &d_data->_vectorDataSources.data(guide).localToWorldMapper();
      break;
    }
    case geo::TIMESERIES: {
      result = &d_data->d_tableDataSources.data(guide).localToWorldMapper();
      break;
    }
    default: {
      result = 0;
      break;
    }
  }

  assert(result);
  return *result;
}



dal::DataSpaceAddressMapper& DataObject::globalToLocalMapper(
         DataGuide const& guide) const
{
  dal::DataSpaceAddressMapper* result = 0;

  switch(guide.type()) {
    case geo::STACK: {
      result = &d_data->d_rasterDataSources.data(guide).globalToLocalMapper();
      break;
    }
    case geo::FEATURE: {
      result = &d_data->d_featureDataSources.data(guide).globalToLocalMapper();
      break;
    }
    case geo::VECTOR: {
      result = &d_data->_vectorDataSources.data(guide).globalToLocalMapper();
      break;
    }
    case geo::TIMESERIES: {
      result = &d_data->d_tableDataSources.data(guide).globalToLocalMapper();
      break;
    }
    default: {
      result = 0;
      break;
    }
  }

  assert(result);
  return *result;
}



//! Collects step mappings for each dimension and for each data set.
/*!
  \param     timeMappings Collection for storing the dimension and time step
             mapping information.
  \param     rowMapping Collection for storing the dimension and row mapping
             information.
  \param     colMapping Collection for storing the dimension and column step
             mapping information.
  \return    The collections passed in are updated.
*/
void DataObject::localStepMappings(
         std::vector<dal::DimensionTimeStepMapping>& timeMappings,
         std::vector<dal::DimensionSpaceStepMapping>& spaceMappings)
{
  assert(timeMappings.empty());
  assert(spaceMappings.empty());

  std::vector<DataGuide> guides(dataGuides());

  // Loop over all dimensions of the central data space.
  for(size_t i = 0; i < dataSpace().rank(); ++i) {
    dal::Dimension const& mainDimension(dataSpace().dimension(i));

    // Loop over all data guides.
    for(size_t j = 0; j < guides.size(); ++j) {

      // Analyse data set data space.
      dal::DataSpace const& space(dataSpace(guides[j]));

      size_t dimensionId = space.indexOf(mainDimension);

      if(dimensionId != space.rank()) {
        dal::Dimension const& subDimension(space.dimension(dimensionId));

        // Data set data space has the dimension.
        if(mainDimension.meaning() == dal::Time) {
          dal::TimeStepMapper const* mapper =
              dynamic_cast<dal::TimeStepMapper const*>(
                   localToWorldMapper(guides[j]).mapper(dimensionId));

          if(mapper) {
            // Possibly zero!
            timeMappings.push_back(dal::DimensionTimeStepMapping(
              space.dimension(dimensionId), mapper));
          }
        }
        else if(subDimension.meaning() == dal::Space) {
          switch(subDimension.discretisation()) {
            case dal::RegularDiscretisation: {
              assert(mainDimension.meaning() == dal::Space);
              assert(mainDimension.discretisation() ==
                   dal::RegularDiscretisation);

              dal::SpaceStepMapper const* mapper =
                   dynamic_cast<dal::SpaceStepMapper const*>(
                   localToWorldMapper(guides[j]).mapper(dimensionId));

              if(mapper) {
                // Possibly zero!
                spaceMappings.push_back(dal::DimensionSpaceStepMapping(
                   space.dimension(dimensionId), mapper));
              }

              break;
            }
            case dal::BorderedDiscretisation: {
              /// FEATURE what about feature data space BorderedDiscretisation.
              break;
            }
            default: {
              assert(false);
              break;
            }
          }
        }
      }
    }
  }
}



  // Use mapping information collected to determine the mappers to map
  // addresses in the global (data object) data space to addresses in the
  // local (data set) data space.
  // global (data object) <-> local (data set)
//! Determines mappers for mapping global to local coordinates.
/*!
  \param     timeMappings Collection containing the dimension and time step
             mapping information.
  \param     rowMapping Collection containing the dimension and row mapping
             information.
  \param     colMapping Collection containing the dimension and column step
             mapping information.
  \param     timeStepMappers Collection with global <-> local mappers for each
             mapping in \a timeMappings.
  \param     rowStepMappers Collection with global <-> local mappers for each
             mapping in \a rowMappings.
  \param     colStepMappers Collection with global <-> local mappers for each
             mapping in \a colMappings.
  \param     timeStepMapper Global mapper for mapping global <-> local.
  \return    .
  \exception .
  \warning   .
  \sa        .


*/
void  DataObject::globalStepMappings(
         std::vector<dal::DimensionTimeStepMapping> const& timeMappings,
         std::vector<dal::DimensionSpaceStepMapping> const& spaceMappings,
         std::vector<dal::StepMapper>& timeStepMappers,
         std::vector<dal::StepMapper>& spaceStepMappers,
         dal::TimeStepMapper& timeStepMapper,
         dal::SpaceStepMapper& spaceStepMapper)
{
  for(size_t i = 0; i < dataSpace().rank(); ++i) {
    if(dataSpace().dimension(i).meaning() == dal::Time) {
      if(!timeMappings.empty()) {
        dal::Dimension dimension;
        timeStepMap(timeMappings, &dimension, &timeStepMapper,
              &timeStepMappers);
        assert(dimension.meaning() == dal::Time);
        d_data->d_dataSpace.dimension(i) = dimension;
        // assert(dimension == dataSpace().dimension(i));
      }
    }
    else if(dataSpace().dimension(i).meaning() == dal::Space) {
      switch(dataSpace().dimension(i).discretisation()) {
        case dal::RegularDiscretisation: {
          if(!spaceMappings.empty()) {
            dal::Dimension dimension;
            spaceStepMap(spaceMappings, &dimension, &spaceStepMapper,
                &spaceStepMappers);
            assert(dimension.meaning() == dal::Space);
            d_data->d_dataSpace.dimension(i) = dimension;
            // assert(dimension == dataSpace().dimension(i));
          }

          break;
        }
        case dal::BorderedDiscretisation: {
          /// FEATURE how about feature BorderedDiscretisation
          break;
        }
        default: {
          assert(false);
          break;
        }
      }
    }
  }
}



void DataObject::setGlobalToWorldMappers(
         std::vector<dal::StepMapper> const& timeStepMappers,
         std::vector<dal::StepMapper> const& spaceStepMappers)
{
  std::vector<DataGuide> guides(dataGuides());

  // For each individual data set, set a mapper to map addresses in the
  // global (data object) data space to addresses in the local (data set)
  // data space.
  size_t t = 0;    // index of current item in timeStepMappers collection.
  size_t s = 0;    // index of current item in spaceStepMappers collection.

  // Loop over the dimensions of the central data space.
  for(size_t i = 0; i < dataSpace().rank(); ++i) {
    dal::Dimension const& mainDimension(dataSpace().dimension(i));

    for(size_t j = 0; j < guides.size(); ++j) {
      dal::DataSpace const& space(dataSpace(guides[j]));
      size_t dimensionId = space.indexOf(mainDimension);

      if(dimensionId != space.rank()) {
        dal::Dimension const& subDimension(space.dimension(dimensionId));

        // Data set data space has the dimension.
        if(mainDimension.meaning() == dal::Time) {
          dal::TimeStepMapper const* mapper =
              dynamic_cast<dal::TimeStepMapper const*>(
                   localToWorldMapper(guides[j]).mapper(dimensionId));

          if(mapper) {
            globalToLocalMapper(guides[j]).setMapper(dimensionId,
              new dal::StepCoordinateMapper(timeStepMappers[t++],
                   dal::UsePrevious));
          }
        }
        else if(subDimension.meaning() == dal::Space) {
          switch(subDimension.discretisation()) {
            case dal::RegularDiscretisation: {
              assert(mainDimension.meaning() == dal::Space);
              assert(mainDimension.discretisation() ==
                   dal::RegularDiscretisation);

              dal::SpaceStepMapper const* mapper =
                   dynamic_cast<dal::SpaceStepMapper const*>(
                        localToWorldMapper(guides[j]).mapper(dimensionId));

              if(mapper) {
                globalToLocalMapper(guides[j]).setMapper(dimensionId,
                  new dal::StepCoordinateMapper(spaceStepMappers[s++],
                        dal::SetToMissingValue));
              }

              break;
            }
            case dal::BorderedDiscretisation: {
              // FEATURE how about FEATURE BorderedDiscretisation
              break;
            }
            default: {
              assert(false);
              break;
            }
          }
        }
      }
    }
  }
}



void DataObject::setGlobalToWorldMapper(
         dal::TimeStepMapper const& timeStepMapper,
         dal::SpaceStepMapper const& spaceStepMapper)
{
  // Configure global to world mapper.
  dal::DataSpaceAddressMapper& mapper(d_data->d_globalToWorldMapper);
  dal::DataSpace const& space(d_data->d_dataSpace);

  mapper.setDataSpace(space);

  if(space.hasTime() && timeStepMapper.isValid()) {
    size_t id = space.indexOf(dal::Time);

    // Time dimension.
    if(space.dimension(id).discretisation() == dal::RegularDiscretisation) {
      mapper.setMapper(id, new dal::TimeStepCoordinateMapper(timeStepMapper));
    }
  }

  if(space.hasRaster()) {
    size_t id = space.indexOf(dal::Space);

    // Space dimension.
    assert(space.dimension(id).discretisation() == dal::RegularDiscretisation);
    mapper.setMapper(id, new dal::SpaceStepCoordinateMapper(spaceStepMapper));
  }
}



void DataObject::reconfigureDataSpaceAndMappers()
{
  // For each data set determine the mapper to translate 'global' (data object)
  // dimension coordinates to 'local' (data set) dimension coordinates.

  // local -> local step mappers for each dimension and for all data sets.
  // Mappings from dimension id to step mapper. These translate steps to steps.
  std::vector<dal::DimensionTimeStepMapping> timeMappings;
  std::vector<dal::DimensionSpaceStepMapping> spaceMappings;
  localStepMappings(timeMappings, spaceMappings);

  // global -> local step mappers for each dimension and for all data sets.
  // Determine step mappers for each dimension.
  std::vector<dal::StepMapper> timeStepMappers;
  std::vector<dal::StepMapper> spaceStepMappers;
  dal::TimeStepMapper timeStepMapper;
  dal::SpaceStepMapper spaceStepMapper;
  globalStepMappings(
         timeMappings, spaceMappings,
         timeStepMappers, spaceStepMappers,
         timeStepMapper, spaceStepMapper);

  setGlobalToWorldMappers(timeStepMappers, spaceStepMappers);
  setGlobalToWorldMapper(timeStepMapper, spaceStepMapper);
}



void DataObject::setBackgroundColour(
         QColor const& color,
         bool notify)
{
  if(d_data->d_backgroundColor != color) {
    d_data->d_backgroundColor = color;
    setNotifyNeeded(true);
  }

  if(notify) {
    this->notify();
  }
}



QColor const& DataObject::backgroundColour() const
{
  return d_data->d_backgroundColor;
}



//! Returns the properties of the rasters.
/*!
  \return    Raster properties.
  \warning   The data space must contain raster properties.
*/
dal::RasterDimensions const& DataObject::rasterDimensions() const
{
  dal::DataSpace const& space(d_data->d_dataSpace);

  assert(space.hasRaster());

  size_t index = space.indexOf(dal::Space);

  return space.dimension(index).value<dal::RasterDimensions>(0);
}



//! Returns the properties of the feature layers.
/*!
  \return    Feature properties.
  \warning   The data space must contain feature layer properties.
*/
dal::SpaceDimensions const& DataObject::featureDimensions() const
{
  dal::DataSpace const& space(d_data->d_dataSpace);

  assert(space.hasFeatures());

  size_t index = space.indexOf(dal::Space);

  if(space.hasRaster()) {
    ++index;
    assert(space.dimension(index).meaning() == dal::Space);
    assert(space.dimension(index).discretisation() ==
         dal::BorderedDiscretisation);
  }

  return space.dimension(index).value<dal::SpaceDimensions>(0);
}



//! Returns the envelope around all raster and feature data sets.
/*!
  \return    Spatial envelope.
  \warning   The data space must contain raster and/or feature layer properties.
*/
dal::SpaceDimensions DataObject::envelope() const
{
  dal::SpaceDimensions result;
  dal::DataSpace const& space(d_data->d_dataSpace);
  assert(space.hasSpace());

  if(space.hasRaster() && space.hasFeatures()) {
    result = rasterDimensions() | featureDimensions();
  }
  else if(space.hasRaster()) {
    result = rasterDimensions();
  }
  else {
    result = featureDimensions();
  }

  return result;
}



//! Returns the spatial address of the cursor.
/*!
  \return    Spatial address.
  \warning   The data space must contain spatial properties.
*/
dal::SpatialCoordinate const& DataObject::spatialAddress() const
{
  dal::DataSpace const& space(d_data->d_dataSpace);
  assert(space.hasSpace());

  size_t index = space.indexOf(dal::Space);
  dal::SpatialCoordinate const& result(
         d_data->d_dataSpaceAddress.coordinate<dal::SpatialCoordinate>(index));

#ifdef DEBUG_DEVELOP
  // Sanity checks.
  if(space.hasRaster() && space.hasFeatures()) {
    ++index;
    assert(space.dimension(index).meaning() == dal::Space);
    assert(space.dimension(index).discretisation() ==
         dal::BorderedDiscretisation);
    assert(result ==
         d_data->d_dataSpaceAddress.coordinate<dal::SpatialCoordinate>(index));
  }
#endif

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

