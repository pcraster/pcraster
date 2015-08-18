#include "ag_DataProperties.h"

// Library headers.
#include <cmath>
#include <map>

// PCRaster library headers.
#include "dev_Algorithm.h"
#include "csftypes.h"
#include "com_bitvector.h"
#include "com_classclassifier.h"
#include "com_classifier.h"
#include "com_rangemap.h"
#include "com_rawpalette.h"

// Module headers.
#include "ag_BooleanDrawProps.h"
#include "ag_DataGuide.h"
#include "ag_DataObject.h"
#include "ag_DataProperty.h"
#include "ag_LddDrawProps.h"
#include "ag_NominalDrawProps.h"
#include "ag_OrdinalDrawProps.h"
#include "ag_RangeDrawProps.h"
#include "ag_RasterDataSources.h"
#include "ag_Table.h"
#include "ag_TableDataSources.h"



/*!
  \file
  This file contains the implementation of the DataProperties class.
*/



//------------------------------------------------------------------------------

namespace ag {

struct ltDataGuide
{
  bool operator()(const DataGuide& lhs, const DataGuide& rhs) const
  {
    return lhs.address() < rhs.address();
  }
};

class DataPropertiesPrivate {

public:

  std::vector<DataGuide>     _guides;
  std::vector<DataProperty*> _dataProperties;
  std::map<DataGuide, GeometryDrawProps*, ltDataGuide> _geometryDrawProperties;
  std::map<DataGuide, BooleanDrawProps*, ltDataGuide> _booleanDrawProperties;
  std::map<DataGuide, NominalDrawProps*, ltDataGuide> _nominalDrawProperties;
  std::map<DataGuide, OrdinalDrawProps*, ltDataGuide> _ordinalDrawProperties;
  std::map<DataGuide, LddDrawProps*, ltDataGuide> _lddDrawProperties;
  std::map<DataGuide, RangeDrawProps*, ltDataGuide> _rangeDrawProperties;

  std::map<DataGuide, GeometryDrawProps*, ltDataGuide>
         _mergedGeometryDrawProperties;
  std::map<DataGuide, RangeDrawProps*, ltDataGuide> _mergedRangeDrawProperties;

  std::vector<com_ClassClassifier<UINT1> *> _booleanClassifiers;
  std::vector<com_ClassClassifier<INT4> *> _nominalClassifiers;
  std::vector<com_ClassClassifier<INT4> *> _ordinalClassifiers;
  std::vector<com_ClassClassifier<UINT1> *> _lddClassifiers;
  std::vector<com::Classifier*> _rangeClassifiers;

  DataPropertiesPrivate()
  {
  }

  ~DataPropertiesPrivate()
  {
    dev::forWhole(_dataProperties, dev::Delete<DataProperty>());

    dev::forWhole(_booleanClassifiers,
         dev::Delete<com_ClassClassifier<UINT1> >());
    dev::forWhole(_nominalClassifiers,
         dev::Delete<com_ClassClassifier<INT4> >());
    dev::forWhole(_ordinalClassifiers,
         dev::Delete<com_ClassClassifier<INT4> >());
    dev::forWhole(_lddClassifiers,
         dev::Delete<com_ClassClassifier<UINT1> >());
    dev::forWhole(_rangeClassifiers, dev::Delete<com::Classifier>());

    for(std::map<DataGuide, DrawProps*, ltDataGuide>::iterator it =
         _geometryDrawProperties.begin();
         it != _geometryDrawProperties.end(); ++it) {
      delete (*it).second;
    }

    for(std::map<DataGuide, BooleanDrawProps*, ltDataGuide>::iterator it =
         _booleanDrawProperties.begin(); it != _booleanDrawProperties.end();
         ++it) {
      delete (*it).second;
    }

    for(std::map<DataGuide, NominalDrawProps*, ltDataGuide>::iterator it =
         _nominalDrawProperties.begin(); it != _nominalDrawProperties.end();
         ++it) {
      delete (*it).second;
    }

    for(std::map<DataGuide, OrdinalDrawProps*, ltDataGuide>::iterator it =
         _ordinalDrawProperties.begin(); it != _ordinalDrawProperties.end();
         ++it) {
      delete (*it).second;
    }

    for(std::map<DataGuide, LddDrawProps*, ltDataGuide>::iterator it =
         _lddDrawProperties.begin(); it != _lddDrawProperties.end(); ++it) {
      delete (*it).second;
    }

    for(std::map<DataGuide, RangeDrawProps*, ltDataGuide>::iterator it =
         _rangeDrawProperties.begin(); it != _rangeDrawProperties.end();
         ++it) {
      delete (*it).second;
    }

    {
      std::set<RangeDrawProps*> drawProperties;

      for(std::map<DataGuide, RangeDrawProps*, ltDataGuide>::iterator it =
         _mergedRangeDrawProperties.begin();
         it != _mergedRangeDrawProperties.end(); ++it) {
        drawProperties.insert((*it).second);
      }

      for(std::set<RangeDrawProps*>::iterator it = drawProperties.begin();
              it != drawProperties.end(); ++it) {
        delete *it;
      }
    }

    {
      std::set<GeometryDrawProps*> drawProperties;

      for(std::map<DataGuide, GeometryDrawProps*, ltDataGuide>::iterator it =
         _mergedGeometryDrawProperties.begin();
         it != _mergedGeometryDrawProperties.end(); ++it) {
        drawProperties.insert((*it).second);
      }

      for(std::set<GeometryDrawProps*>::iterator it = drawProperties.begin();
              it != drawProperties.end(); ++it) {
        delete *it;
      }
    }
  }
};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAPROPERTIES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATAPROPERTIES MEMBERS
//------------------------------------------------------------------------------

DataProperties::DataProperties()

  : _data(new DataPropertiesPrivate())

{
}



DataProperties::~DataProperties()
{
}


//! Checks the integrity of the object.
/*!
  \warning   All bets are off if the integrity is not ok (dump!).

  This function should be called by any function which changes the object.
*/
void DataProperties::assertIntegrity()
{
  assert(_data->_guides.size() == _data->_dataProperties.size());
}



//! Returns the index of \a guide.
/*!
  \param     guide Must be valid and present in the object.
*/
size_t DataProperties::index(
         DataGuide const& guide) const
{
  iterator it = std::find(_data->_guides.begin(),
                   _data->_guides.end(), guide);
  assert(it != _data->_guides.end());

  return it - _data->_guides.begin();
}



void DataProperties::add(
         DataGuide const& guide)
{
  assert(guide.isValid());
  assert(!hasCommonDataPropertiesFor(guide));

  _data->_guides.push_back(guide);
  _data->_dataProperties.push_back(new DataProperty());

  assertIntegrity();
}



void DataProperties::add(
         DataGuide const& guide,
         DataProperty const& dataProperty)
{
  assert(guide.isValid());
  assert(!hasCommonDataPropertiesFor(guide));

  _data->_guides.push_back(guide);
  _data->_dataProperties.push_back(new DataProperty(dataProperty));

  assertIntegrity();

  assert(hasCommonDataPropertiesFor(guide));
}



//! Copies the properties from \a properties to properties of \a guide.
/*!
  \param     guide Data guide to copy properties to.
  \param     properties Properties to copy.
  \warning   Existing settings for \a guide are overwritten.
  \sa        add(DataGuide const&, DataProperty const&)
*/
void DataProperties::copy(
         DataGuide const& guide,
         DataProperty const& properties)
{
  assert(guide.isValid());

  if(!hasCommonDataPropertiesFor(guide)) {
    add(guide, properties);
  }
  else {
    DataProperty& target = commonDataProperties(guide);
    target = properties;
  }

  assertIntegrity();

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addGeometryDataProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  if(!hasCommonDataPropertiesFor(guide)) {
    std::string title = dataObject.name(guide);

    /// FEATURE fix palette.
    _data->_geometryDrawProperties[guide] = new DrawProps(title,
         com::defaultPalette(VS_BOOLEAN));

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      See if we want column titles as titles (if present). Similar to
             choosing the legend title (if present) over the name of the
             data source.
*/
void DataProperties::addScalarTimeSeriesProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  if(!hasCommonDataPropertiesFor(guide)) {

    Table const& table = dataObject.tableDataSources().data(guide);
    std::string title = dataObject.name(guide);

    assert(table.nrCols() >= 2);

    // Handle drawproperties for assigning colours to different timeseries.
    assert(_data->_nominalDrawProperties.find(guide) ==
            _data->_nominalDrawProperties.end());

    com_ClassClassifier<INT4>* classClassifier = new com_ClassClassifier<INT4>();
    _data->_nominalClassifiers.push_back(classClassifier);
    classClassifier->setClasses(1, table.nrCols() - 1);
    _data->_nominalDrawProperties[guide] = new NominalDrawProps(
         title, com::defaultPalette(VS_NOMINAL), classClassifier);

    // Handle drawproperties for displaying timeseries values.
    assert(_data->_rangeDrawProperties.find(guide) ==
            _data->_rangeDrawProperties.end());

    // Create default draw properties.
    com::Classifier* rawValueClassifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(rawValueClassifier);
    rawValueClassifier->installLin();

    // Determine min and max of dependant variables.
    if(!table.allMV()) {
      REAL4 min = table.min<REAL4>();
      REAL4 max = table.max<REAL4>();

      rawValueClassifier->setNrClasses(1);
      rawValueClassifier->setExtremes(min, max);
      rawValueClassifier->setCutoffs(min, max);
      rawValueClassifier->classify();
    }

    _data->_rangeDrawProperties[guide] = new RangeDrawProps(
         title, com::defaultPalette(VS_SCALAR), rawValueClassifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void DataProperties::addBooleanStackProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_BOOLEAN);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_booleanDrawProperties.find(guide) ==
                     _data->_booleanDrawProperties.end());

    Raster const& raster = dataObject.rasterDataSources().data(guide);

    std::string title;

    // Classifier.
    com_ClassClassifier<UINT1>* classifier = new com_ClassClassifier<UINT1>();
    _data->_booleanClassifiers.push_back(classifier);

    // Use legend if present.
    if(raster.hasLegend()) {
      dal::Table legend(raster.legend());
      title = !legend.title().empty() ? legend.title() : dataObject.name(guide);

      // Convert INT4 legend ids to UINT1 ids.
      dal::Array<INT4> const& ids(legend.col<INT4>(0));
      dal::Array<UINT1> newIds(ids.size());

      for(size_t i = 0; i < ids.size(); ++i) {
        newIds[i] = ids[i];
      }

      classifier->setClasses(
         newIds.elements(),
         legend.col<std::string>(1).elements(),
         legend.nrRecs());
    }
    else {
      title = dataObject.name(guide);

      UINT1 min, max;
      if(raster.min<UINT1>(min) && raster.max<UINT1>(max)) {
        std::vector<com_LegendClass<UINT1> > classes;
        classes.push_back(com_LegendClass<UINT1>(0, "false"));
        classes.push_back(com_LegendClass<UINT1>(1, "true"));
        classifier->setClasses(classes);
      }
    }

    // Draw properties.
    _data->_booleanDrawProperties[guide] = new BooleanDrawProps(
         title, com::defaultPalette(VS_BOOLEAN), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Use legend if present in raster.
*/
void DataProperties::addNominalStackProperties(
         const DataObject& dataObject, const DataGuide& guide)
{
  assert(guide.valueScale() == VS_NOMINAL);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_nominalDrawProperties.find(guide) ==
                     _data->_nominalDrawProperties.end());

    Raster const& raster = dataObject.rasterDataSources().data(guide);

    com_ClassClassifier<INT4>* classifier =
           new com_ClassClassifier<INT4>();
    _data->_nominalClassifiers.push_back(classifier);

    std::string title;

    // Use legend if present.
    if(raster.hasLegend()) {
      dal::Table legend(raster.legend());
      title = !legend.title().empty() ? legend.title() : dataObject.name(guide);
      classifier->setClasses(
         legend.col<INT4>(0).elements(),
         legend.col<std::string>(1).elements(),
         legend.nrRecs());
    }
    else {
      title = dataObject.name(guide);

      INT4 min, max;                           // Min and max in raster.
      if(raster.min<INT4>(min) && raster.max<INT4>(max)) {
        classifier->setClasses(raster.classes<INT4>());
      }
    }

    _data->_nominalDrawProperties[guide] = new NominalDrawProps(
           title, com::defaultPalette(VS_NOMINAL), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
void DataProperties::addOrdinalStackProperties(
         const DataObject& dataObject, const DataGuide& guide)
{
  assert(guide.valueScale() == VS_ORDINAL);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_ordinalDrawProperties.find(guide) ==
                     _data->_ordinalDrawProperties.end());

    Raster const& raster = dataObject.rasterDataSources().data(guide);

    std::string title;

    title = dataObject.name(guide);

    com_ClassClassifier<INT4>* classifier =
                   new com_ClassClassifier<INT4>();
    _data->_ordinalClassifiers.push_back(classifier);

    // Use legend if present.
    if(raster.hasLegend()) {
      dal::Table legend(raster.legend());
      title = !legend.title().empty() ? legend.title() : dataObject.name(guide);
      classifier->setClasses(
         legend.col<INT4>(0).elements(),
         legend.col<std::string>(1).elements(),
         legend.nrRecs());
    }
    else {
      INT4 min, max;
      if(raster.min<INT4>(min) && raster.max<INT4>(max)) {
        std::vector<INT4> classes;
        classes.resize(max - min + 1);
        INT4 v = min;
        for(size_t i = 0; i < classes.size(); ++i, ++v) {
          classes[i] = v;
        }

        classifier->setClasses(classes);
      }
    }

    _data->_ordinalDrawProperties[guide] = new OrdinalDrawProps(
           title, com::defaultPalette(VS_ORDINAL), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addScalarStackProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_rangeDrawProperties.find(guide) ==
                     _data->_rangeDrawProperties.end());

    Raster const& raster = dataObject.rasterDataSources().data(guide);
    std::string title = dataObject.name(guide);

    com::Classifier* classifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(classifier);
    classifier->installLin();

    REAL4 min, max;
    if(raster.min<REAL4>(min) && raster.max<REAL4>(max)) {
      classifier->setNrClasses(100);
      classifier->setExtremes(min, max);
      classifier->setCutoffs(min, max);
      classifier->classify();
    }

    _data->_rangeDrawProperties[guide] = new RangeDrawProps(
           title, com::defaultPalette(VS_SCALAR), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addDirectionalStackProperties(
         const DataObject& dataObject, const DataGuide& guide)
{
  assert(guide.valueScale() == VS_DIRECTION);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_rangeDrawProperties.find(guide) ==
                     _data->_rangeDrawProperties.end());

    Raster const& raster = dataObject.rasterDataSources().data(guide);
    std::string title = dataObject.name(guide);

    com::Classifier* displayValueClassifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(displayValueClassifier);
    displayValueClassifier->installLin();

    com::Classifier* rawValueClassifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(rawValueClassifier);
    rawValueClassifier->installLin();

    REAL4 max;
    if(raster.max<REAL4>(max)) {
      // Display values: values as presented to the user (degrees).
      displayValueClassifier->setNrClasses(100);
      displayValueClassifier->setExtremes(0.0, 360.0);
      displayValueClassifier->setCutoffs(0.0, 360.0);
      displayValueClassifier->setMode(com::Classifier::EXACT);
      displayValueClassifier->classify();

      com::RangeMap<double, double> map(0.0, 360.0, 0.0, 2 * M_PI);

      // Raw values: values as present in the raster (radians).
      // Sync classification of raw values with classification of display
      // values.
      rawValueClassifier->setNrClasses(displayValueClassifier->nrClasses());
      rawValueClassifier->setExtremes(0.0, 2 * M_PI);
      if(!displayValueClassifier->borders().empty()) {
        double lowerBorder, upperBorder;
        lowerBorder = map.map(displayValueClassifier->borders().front());
        upperBorder = map.map(displayValueClassifier->borders().back());
        rawValueClassifier->setCutoffs(lowerBorder, upperBorder);
      }
      rawValueClassifier->setMode(com::Classifier::EXACT);
      rawValueClassifier->classify();
    }

    _data->_rangeDrawProperties[guide] = new RangeDrawProps(
           title, com::defaultPalette(VS_DIRECTION), rawValueClassifier,
           displayValueClassifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addLddStackProperties(
         const DataObject& dataObject, const DataGuide& guide)
{
  assert(guide.valueScale() == VS_LDD);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_lddDrawProperties.find(guide) ==
                     _data->_lddDrawProperties.end());

    // Raster const& raster = dataObject.rasterDataSources().data(guide);
    std::string title = dataObject.name(guide);

    com_ClassClassifier<UINT1>* classifier = new com_ClassClassifier<UINT1>();
    _data->_lddClassifiers.push_back(classifier);

    std::vector<com_LegendClass<UINT1> > classes;
    classes.push_back(com_LegendClass<UINT1>(1, "sw"));
    classes.push_back(com_LegendClass<UINT1>(2, "s"));
    classes.push_back(com_LegendClass<UINT1>(3, "se"));
    classes.push_back(com_LegendClass<UINT1>(4, "w"));
    classes.push_back(com_LegendClass<UINT1>(5, "pit"));
    classes.push_back(com_LegendClass<UINT1>(6, "e"));
    classes.push_back(com_LegendClass<UINT1>(7, "nw"));
    classes.push_back(com_LegendClass<UINT1>(8, "n"));
    classes.push_back(com_LegendClass<UINT1>(9, "ne"));
    classifier->setClasses(classes);

    _data->_lddDrawProperties[guide] = new LddDrawProps(
           title, com::defaultPalette(VS_LDD), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addBooleanFeatureProperties(
         DataObject const& /* dataObject */,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_BOOLEAN);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_booleanDrawProperties.find(guide) ==
         _data->_booleanDrawProperties.end());

    /// FeatureLayer const& layer = dataObject.featureDataSources().data(guide);

    std::string title;

    // Classifier.
    com_ClassClassifier<UINT1>* classifier = new com_ClassClassifier<UINT1>();
    _data->_booleanClassifiers.push_back(classifier);

    /// FEATURE
    /// // Use legend if present.
    /// if(raster.hasLegend()) {
    ///   dal::Table legend(raster.legend());
    ///   title = !legend.title().empty() ? legend.title() : dataObject.name(guide);

    ///   // Convert INT4 legend ids to UINT1 ids.
    ///   dal::Array<INT4> const& ids(legend.col<INT4>(0));
    ///   dal::Array<UINT1> newIds(ids.size());

    ///   for(size_t i = 0; i < ids.size(); ++i) {
    ///     newIds[i] = ids[i];
    ///   }

    ///   classifier->setClasses(
    ///      newIds.elements(),
    ///      legend.col<std::string>(1).elements(),
    ///      legend.nrRecs());
    /// }
    /// else {
    ///   title = dataObject.name(guide);

    ///   UINT1 min, max;
    ///   if(layer.min<UINT1>(min) && layer.max<UINT1>(max)) {
    ///     std::vector<com_LegendClass<UINT1> > classes;
    ///     classes.push_back(com_LegendClass<UINT1>(0, "false"));
    ///     classes.push_back(com_LegendClass<UINT1>(1, "true"));
    ///     classifier->setClasses(classes);
    ///   }
    /// }

    // Draw properties.
    _data->_booleanDrawProperties[guide] = new BooleanDrawProps(
         title, com::defaultPalette(VS_BOOLEAN), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addNominalFeatureProperties(
         DataObject const& /* dataObject */,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_NOMINAL);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_nominalDrawProperties.find(guide) ==
                     _data->_nominalDrawProperties.end());

    /// Raster const& raster = dataObject.rasterDataSources().data(guide);

    com_ClassClassifier<INT4>* classifier =
           new com_ClassClassifier<INT4>();
    _data->_nominalClassifiers.push_back(classifier);

    std::string title;

    /// FEATURE
    /// // Use legend if present.
    /// if(raster.hasLegend()) {
    ///   dal::Table legend(raster.legend());
    ///   title = !legend.title().empty() ? legend.title() : dataObject.name(guide);
    ///   classifier->setClasses(
    ///      legend.col<INT4>(0).elements(),
    ///      legend.col<std::string>(1).elements(),
    ///      legend.nrRecs());
    /// }
    /// else {
    ///   title = dataObject.name(guide);

    ///   INT4 min, max;                           // Min and max in raster.
    ///   if(raster.min<INT4>(min) && raster.max<INT4>(max)) {
    ///     classifier->setClasses(raster.classes<INT4>());
    ///   }
    /// }

    _data->_nominalDrawProperties[guide] = new NominalDrawProps(
           title, com::defaultPalette(VS_NOMINAL), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addOrdinalFeatureProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_ORDINAL);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_ordinalDrawProperties.find(guide) ==
                     _data->_ordinalDrawProperties.end());

    /// Raster const& raster = dataObject.rasterDataSources().data(guide);

    std::string title;

    title = dataObject.name(guide);

    com_ClassClassifier<INT4>* classifier =
                   new com_ClassClassifier<INT4>();
    _data->_ordinalClassifiers.push_back(classifier);

    // FEATURE
    /// // Use legend if present.
    /// if(raster.hasLegend()) {
    ///   dal::Table legend(raster.legend());
    ///   title = !legend.title().empty() ? legend.title() : dataObject.name(guide);
    ///   classifier->setClasses(
    ///      legend.col<INT4>(0).elements(),
    ///      legend.col<std::string>(1).elements(),
    ///      legend.nrRecs());
    /// }
    /// else {
    ///   INT4 min, max;
    ///   if(raster.min<INT4>(min) && raster.max<INT4>(max)) {
    ///     std::vector<INT4> classes;
    ///     classes.resize(max - min + 1);
    ///     INT4 v = min;
    ///     for(size_t i = 0; i < classes.size(); ++i, ++v) {
    ///       classes[i] = v;
    ///     }

    ///     classifier->setClasses(classes);
    ///   }
    /// }

    _data->_ordinalDrawProperties[guide] = new OrdinalDrawProps(
           title, com::defaultPalette(VS_ORDINAL), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addScalarFeatureProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_rangeDrawProperties.find(guide) ==
                     _data->_rangeDrawProperties.end());

    FeatureLayer const& layer = dataObject.featureDataSources().data(guide);

    std::string title = dataObject.name(guide);

    com::Classifier* classifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(classifier);
    classifier->installLin();

    REAL4 min, max;
    if(layer.min<REAL4>(min) && layer.max<REAL4>(max)) {
      classifier->setNrClasses(100);
      classifier->setExtremes(min, max);
      classifier->setCutoffs(min, max);
      classifier->classify();
    }

    _data->_rangeDrawProperties[guide] = new RangeDrawProps(
           title, com::defaultPalette(VS_SCALAR), classifier);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::addVectorProperties(
         DataObject const& dataObject,
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  if(!hasCommonDataPropertiesFor(guide)) {

    assert(_data->_rangeDrawProperties.find(guide) ==
         _data->_rangeDrawProperties.end());

    Vector const& vector = dataObject.vectorDataSources().data(guide);
    std::string title = dataObject.name(guide);

    com::Classifier* classifier = new com::Classifier();
    _data->_rangeClassifiers.push_back(classifier);
    classifier->installLin();

    REAL4 min, max;

    if(vector.min<REAL4>(min) && vector.max<REAL4>(max)) {
      classifier->setNrClasses(3);
      // classifier->setExtremes(min, max);
      classifier->setExtremes(0, max);
      // classifier->setCutoffs(min, max);
      classifier->setCutoffs(0, max);
      classifier->classify();
    }

    _data->_rangeDrawProperties[guide] = new RangeDrawProps(
           title, com::defaultPalette(VS_SCALAR), classifier);
    _data->_rangeDrawProperties[guide]->setDrawerType(VECTORS);

    add(guide);
    assertIntegrity();
  }

  assert(hasCommonDataPropertiesFor(guide));
}



void DataProperties::remove(
         DataGuide const& guide)
{
  assert(hasCommonDataPropertiesFor(guide));

  size_t i = index(guide);

  delete _data->_dataProperties[i];
  _data->_guides.erase(_data->_guides.begin() + i);
  _data->_dataProperties.erase(_data->_dataProperties.begin() + i);

  assert(!hasCommonDataPropertiesFor(guide));

  switch(guide.type()) {
    case geo::TIMESERIES: {
      assert(guide.valueScale() == VS_SCALAR);
      removeScalarTimeSeriesProperties(guide);
      break;
    }
    case geo::VECTOR: {
      assert(guide.valueScale() == VS_SCALAR);
      removeVectorPropertiesProperties(guide);
      break;
    }
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          removeBooleanStackProperties(guide);
          break;
        }
        case VS_NOMINAL: {
          removeNominalStackProperties(guide);
          break;
        }
        case VS_ORDINAL: {
          removeOrdinalStackProperties(guide);
          break;
        }
        case VS_SCALAR: {
          removeScalarStackProperties(guide);
          break;
        }
        case VS_DIRECTION: {
          removeDirectionalStackProperties(guide);
          break;
        }
        case VS_LDD: {
          removeLddStackProperties(guide);
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
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          removeBooleanFeatureProperties(guide);
          break;
        }
        case VS_NOMINAL: {
          removeNominalFeatureProperties(guide);
          break;
        }
        case VS_ORDINAL: {
          removeOrdinalFeatureProperties(guide);
          break;
        }
        case VS_SCALAR: {
          removeScalarFeatureProperties(guide);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  assertIntegrity();
}



void DataProperties::removeBooleanDrawProperties(
         const DataGuide& guide)
{
  assert(_data->_booleanDrawProperties.find(guide) !=
              _data->_booleanDrawProperties.end());
  BooleanDrawProps* drawProps = _data->_booleanDrawProperties[guide];
  _data->_booleanDrawProperties.erase(guide);

  std::vector<com_ClassClassifier<UINT1> *>::iterator iterator =
         std::find(_data->_booleanClassifiers.begin(),
         _data->_booleanClassifiers.end(), &drawProps->classifier());
  assert(iterator != _data->_booleanClassifiers.end());

  delete drawProps;
  delete *iterator;
  _data->_booleanClassifiers.erase(iterator);
}



void DataProperties::removeNominalDrawProperties(
         const DataGuide& guide)
{
  assert(_data->_nominalDrawProperties.find(guide) !=
              _data->_nominalDrawProperties.end());
  NominalDrawProps* drawProps = _data->_nominalDrawProperties[guide];
  _data->_nominalDrawProperties.erase(guide);

  std::vector<com_ClassClassifier<INT4> *>::iterator iterator =
         std::find(_data->_nominalClassifiers.begin(),
         _data->_nominalClassifiers.end(), &drawProps->classifier());
  assert(iterator != _data->_nominalClassifiers.end());

  delete drawProps;
  delete *iterator;
  _data->_nominalClassifiers.erase(iterator);
}



void DataProperties::removeOrdinalDrawProperties(
         const DataGuide& guide)
{
  assert(_data->_ordinalDrawProperties.find(guide) !=
              _data->_ordinalDrawProperties.end());
  OrdinalDrawProps* drawProps = _data->_ordinalDrawProperties[guide];
  _data->_ordinalDrawProperties.erase(guide);

  std::vector<com_ClassClassifier<INT4> *>::iterator iterator =
         std::find(_data->_ordinalClassifiers.begin(),
         _data->_ordinalClassifiers.end(), &drawProps->classifier());
  assert(iterator != _data->_ordinalClassifiers.end());

  delete drawProps;
  delete *iterator;
  _data->_ordinalClassifiers.erase(iterator);
}



void DataProperties::removeLddDrawProperties(
         const DataGuide& guide)
{
  assert(_data->_lddDrawProperties.find(guide) !=
              _data->_lddDrawProperties.end());
  LddDrawProps* drawProps = _data->_lddDrawProperties[guide];
  _data->_lddDrawProperties.erase(guide);

  std::vector<com_ClassClassifier<UINT1> *>::iterator iterator =
         std::find(_data->_lddClassifiers.begin(),
         _data->_lddClassifiers.end(), &drawProps->classifier());
  assert(iterator != _data->_lddClassifiers.end());

  delete drawProps;
  delete *iterator;
  _data->_lddClassifiers.erase(iterator);
}



void DataProperties::removeRangeDrawProperties(const DataGuide& guide)
{
  assert(_data->_rangeDrawProperties.find(guide) !=
                     _data->_rangeDrawProperties.end());

  RangeDrawProps* drawProps = _data->_rangeDrawProperties[guide];
  _data->_rangeDrawProperties.erase(guide);

  eraseRangeClassifier(drawProps->rawValueClassifier());

  if(drawProps->displayValueClassifier()) {
    eraseRangeClassifier(drawProps->displayValueClassifier());
  }

  delete drawProps;
}



void DataProperties::removeScalarTimeSeriesProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  // Remove props for assigning colours to diff. timeseries.
  removeNominalDrawProperties(guide);

  // Remove props for displaying timeseries value.
  removeRangeDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeVectorPropertiesProperties(
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  removeRangeDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeBooleanStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_BOOLEAN);

  removeBooleanDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeNominalStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_NOMINAL);

  removeNominalDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeOrdinalStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_ORDINAL);

  removeOrdinalDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeScalarStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  removeRangeDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeDirectionalStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_DIRECTION);

  removeRangeDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeLddStackProperties(
         const DataGuide& guide)
{
  assert(guide.valueScale() == VS_LDD);

  removeLddDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeBooleanFeatureProperties(
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_BOOLEAN);

  removeBooleanDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeNominalFeatureProperties(
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_NOMINAL);

  removeNominalDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeOrdinalFeatureProperties(
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_ORDINAL);

  removeOrdinalDrawProperties(guide);

  assertIntegrity();
}



void DataProperties::removeScalarFeatureProperties(
         DataGuide const& guide)
{
  assert(guide.valueScale() == VS_SCALAR);

  removeRangeDrawProperties(guide);

  assertIntegrity();
}



//! Copies data properties of \a guide from \a properties.
/*!
  \param     guide Guide to copy properties of.
  \param     properties Properties object to copy from.
  \exception .
  \warning   Existing settings for \a guide are overwritten.
  \sa        .
*/
void DataProperties::copy(
         DataGuide const& guide,
         DataProperties const& properties)
{
  copy(guide, properties.commonDataProperties(guide));

  switch(guide.type()) {
    case geo::TIMESERIES: {
      assert(guide.valueScale() == VS_SCALAR);
      copyScalarTimeSeriesProperties(guide, properties);
      break;
    }
    case geo::VECTOR: {
      assert(guide.valueScale() == VS_SCALAR);
      copyVectorProperties(guide, properties);
      break;
    }
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          copyBooleanStackProperties(guide, properties);
          break;
        }
        case VS_NOMINAL: {
          copyNominalStackProperties(guide, properties);
          break;
        }
        case VS_ORDINAL: {
          copyOrdinalStackProperties(guide, properties);
          break;
        }
        case VS_SCALAR: {
          copyScalarStackProperties(guide, properties);
          break;
        }
        case VS_DIRECTION: {
          copyDirectionalStackProperties(guide, properties);
          break;
        }
        case VS_LDD: {
          copyLddStackProperties(guide, properties);
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
      if(guide.valueScale() == VS_UNDEFINED) {
        // Feature layer has only geometry.
        copyGeometryProperties(guide, properties);
      }
      else {
        switch(guide.valueScale()) {
          case VS_BOOLEAN: {
            copyBooleanStackProperties(guide, properties);
            break;
          }
          case VS_NOMINAL: {
            copyNominalStackProperties(guide, properties);
            break;
          }
          case VS_ORDINAL: {
            copyOrdinalStackProperties(guide, properties);
            break;
          }
          case VS_SCALAR: {
            copyScalarStackProperties(guide, properties);
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
    default: {
      assert(false);
      break;
    }
  }
}



void DataProperties::addNominalDrawProperties(DataGuide const& guide,
         DataProperties const& properties)
{
  assert(!hasNominalDrawPropertiesFor(guide));
  assert(properties.hasNominalDrawPropertiesFor(guide));

  NominalDrawProps& source = properties.nominalDrawProperties(guide);

  // Classifier.
  com_ClassClassifier<INT4>* classifier =
         new com_ClassClassifier<INT4>(source.classifier());
  _data->_nominalClassifiers.push_back(classifier);

  // Draw properties.
  NominalDrawProps* props = new NominalDrawProps(source.title(),
         source.palette(), classifier);
  _data->_nominalDrawProperties[guide] = props;
}



void DataProperties::addRangeDrawProperties(DataGuide const& guide,
         DataProperties const& properties)
{
  assert(!hasRangeDrawPropertiesFor(guide));
  assert(properties.hasRangeDrawPropertiesFor(guide));

  RangeDrawProps const& source = properties.rangeDrawProperties(guide);

  // Classifiers.
  com::Classifier* displayClassifier = 0;
  if(source.displayValueClassifier()) {
    displayClassifier = new com::Classifier(*(source.displayValueClassifier()));
    _data->_rangeClassifiers.push_back(displayClassifier);
  }

  assert(source.rawValueClassifier());
  com::Classifier* rawClassifier =
         new com::Classifier(*(source.rawValueClassifier()));
  _data->_rangeClassifiers.push_back(rawClassifier);

  // Draw properties.
  RangeDrawProps* props = new RangeDrawProps(source.title(),
         source.palette(), rawClassifier, displayClassifier);
  _data->_rangeDrawProperties[guide] = props;
}



void DataProperties::copyNominalDrawProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assert(hasNominalDrawPropertiesFor(guide));
  assert(properties.hasNominalDrawPropertiesFor(guide));

  NominalDrawProps const& source = properties.nominalDrawProperties(guide);
  NominalDrawProps& target = nominalDrawProperties(guide);

  static_cast<DrawProps&>(target) = source;

  target.classifier() = source.classifier();

  // Not defined yet.
  // assert(target == source);

  assertIntegrity();
}



void DataProperties::copyRangeDrawProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  RangeDrawProps const& source = properties.rangeDrawProperties(guide);
  RangeDrawProps& target = rangeDrawProperties(guide);

  static_cast<DrawProps&>(target) = source;
  assert(static_cast<DrawProps const&>(target) ==
                   static_cast<DrawProps const&>(source));

  RangeDrawProps::ClassifierTuples const& sourceClassifiers(
    source.classifiers());
  RangeDrawProps::ClassifierTuples& targetClassifiers(target.classifiers());

  if(targetClassifiers.size() > sourceClassifiers.size()) {
    // Remove classifiers which target has more than source.
    for(size_t i = sourceClassifiers.size(); i < targetClassifiers.size();
         ++i) {
      RangeDrawProps::ClassifierTuple& tuple(targetClassifiers[i]);
      com::Classifier* raw(boost::get<0>(tuple));
      com::Classifier* display(boost::get<1>(tuple));

      eraseRangeClassifier(raw);

      if(display) {
        eraseRangeClassifier(display);
      }
    }

    targetClassifiers.resize(sourceClassifiers.size());
  }
  else if(targetClassifiers.size() < sourceClassifiers.size()) {
    // Add empty classifier tuples for those which source has more than target.
    for(size_t i = targetClassifiers.size(); i < sourceClassifiers.size();
         ++i) {
      targetClassifiers.push_back(RangeDrawProps::ClassifierTuple());
    }
  }

  assert(sourceClassifiers.size() == targetClassifiers.size());

  for(size_t i = 0; i < sourceClassifiers.size(); ++i) {
    RangeDrawProps::ClassifierTuple const& sourceTuple(sourceClassifiers[i]);
    com::Classifier const* sourceRaw(boost::get<0>(sourceTuple));
    com::Classifier const* sourceDisplay(boost::get<1>(sourceTuple));

    RangeDrawProps::ClassifierTuple& targetTuple(targetClassifiers[i]);
    com::Classifier* targetRaw(boost::get<0>(targetTuple));
    com::Classifier* targetDisplay(boost::get<1>(targetTuple));

    assert(sourceRaw);
    if(targetRaw) {
      *targetRaw = *sourceRaw;
      assert(*targetRaw == *sourceRaw);
    }
    else {
      com::Classifier* classifier = new com::Classifier(*(sourceRaw));
      _data->_rangeClassifiers.push_back(classifier);
      boost::get<0>(targetTuple) = classifier;
      assert(*boost::get<0>(targetTuple) == *classifier);
    }

    if(sourceDisplay) {
      if(targetDisplay) {
        *targetDisplay = *sourceDisplay;
        assert(*targetDisplay == *sourceDisplay);
      }
      else {
        com::Classifier* classifier = new com::Classifier(*(sourceDisplay));
        _data->_rangeClassifiers.push_back(classifier);
        boost::get<1>(targetTuple) = classifier;
        assert(*boost::get<1>(targetTuple) == *classifier);
      }
    }
    else if(targetDisplay) {
      eraseRangeClassifier(targetDisplay);
      boost::get<1>(targetTuple) = 0;
    }
  }

/*
  if(source.displayValueClassifier()) {
    if(target.displayValueClassifier()) {
      *target.displayValueClassifier() = *source.displayValueClassifier();
    }
    else {
      com::Classifier* displayClassifier =
            new com::Classifier(*(source.displayValueClassifier()));
      _data->_rangeClassifiers.push_back(displayClassifier);
      target.setDisplayValueClassifier(displayClassifier);
    }
  }
  else {
    if(target.displayValueClassifier()) {
      delete *std::find(_data->_rangeClassifiers.begin(),
            _data->_rangeClassifiers.end(),
            target.displayValueClassifier());
      target.unsetDisplayValueClassifier();
    }
  }
*/

  assert(source.rawValueClassifier());
  assert(target.rawValueClassifier());

  // *target.rawValueClassifier() = *source.rawValueClassifier();

  target.setDrawerType(source.drawerType());
  target.setProbabilityScale(source.probabilityScale());
  target.classify();
  assert(target == source);
  assertIntegrity();
}



void DataProperties::copyScalarTimeSeriesProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assert(guide.isValid());
  assert(properties.hasNominalDrawPropertiesFor(guide));
  assert(properties.hasRangeDrawPropertiesFor(guide));

  if(!hasRangeDrawPropertiesFor(guide)) {
    addNominalDrawProperties(guide, properties);
    addRangeDrawProperties(guide, properties);
  }
  else {
    copyNominalDrawProperties(guide, properties);
    copyRangeDrawProperties(guide, properties);
  }

  assertIntegrity();
}



void DataProperties::copyVectorProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assertIntegrity();

  assert(guide.isValid());
  assert(properties.hasRangeDrawPropertiesFor(guide));

  if(!hasRangeDrawPropertiesFor(guide)) {
    addRangeDrawProperties(guide, properties);
  }
  else {
    copyRangeDrawProperties(guide, properties);
  }

  assertIntegrity();
}



void DataProperties::copyGeometryProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assert(properties._data->_geometryDrawProperties.find(guide) !=
         properties._data->_geometryDrawProperties.end());

  DrawProps const& source = properties.geometryDrawProperties(guide);

  // Draw properties.
  DrawProps* props = new DrawProps(source.title(), source.palette());

  _data->_geometryDrawProperties[guide] = props;

  assertIntegrity();
}



void DataProperties::copyBooleanStackProperties(
         DataGuide const& guide, DataProperties const& properties)
{
  assert(properties._data->_booleanDrawProperties.find(guide) !=
         properties._data->_booleanDrawProperties.end());

  const BooleanDrawProps& source = properties.booleanDrawProperties(guide);

  // Classifier.
  com_ClassClassifier<UINT1>* classifier =
         new com_ClassClassifier<UINT1>(source.classifier());
  _data->_booleanClassifiers.push_back(classifier);

  // Draw properties.
  BooleanDrawProps* props = new BooleanDrawProps(source.title(),
         source.palette(), classifier);

  _data->_booleanDrawProperties[guide] = props;

  assertIntegrity();
}



void DataProperties::copyNominalStackProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assert(_data->_nominalDrawProperties.find(guide) ==
         _data->_nominalDrawProperties.end());
  assert(properties._data->_nominalDrawProperties.find(guide) !=
         properties._data->_nominalDrawProperties.end());

  const NominalDrawProps& source = properties.nominalDrawProperties(guide);

  // Classifier.
  com_ClassClassifier<INT4>* classifier =
         new com_ClassClassifier<INT4>(source.classifier());
  _data->_nominalClassifiers.push_back(classifier);

  // Draw properties.
  NominalDrawProps* props = new NominalDrawProps(source.title(),
         source.palette(), classifier);

  _data->_nominalDrawProperties[guide] = props;

  assertIntegrity();
}



void DataProperties::copyOrdinalStackProperties(
         const DataGuide& guide, const DataProperties& properties)
{
  assert(properties._data->_ordinalDrawProperties.find(guide) !=
         properties._data->_ordinalDrawProperties.end());

  const OrdinalDrawProps& source = properties.ordinalDrawProperties(guide);

  // Classifier.
  com_ClassClassifier<INT4>* classifier =
         new com_ClassClassifier<INT4>(source.classifier());
  _data->_ordinalClassifiers.push_back(classifier);

  // Draw properties.
  OrdinalDrawProps* props = new OrdinalDrawProps(source.title(),
         source.palette(), classifier);

  _data->_ordinalDrawProperties[guide] = props;

  assertIntegrity();
}



void DataProperties::copyLddStackProperties(
         const DataGuide& guide, const DataProperties& properties)
{
  assert(properties._data->_lddDrawProperties.find(guide) !=
         properties._data->_lddDrawProperties.end());

  const LddDrawProps& source = properties.lddDrawProperties(guide);

  // Classifier.
  com_ClassClassifier<UINT1>* classifier =
         new com_ClassClassifier<UINT1>(source.classifier());
  _data->_lddClassifiers.push_back(classifier);

  // Draw properties.
  LddDrawProps* props = new LddDrawProps(source.title(),
         source.palette(), classifier);

  _data->_lddDrawProperties[guide] = props;

  assertIntegrity();
}



void DataProperties::copyScalarStackProperties(
         DataGuide const& guide, DataProperties const& properties)
{
  assert(guide.isValid());
  assert(properties.hasRangeDrawPropertiesFor(guide));

  if(!hasRangeDrawPropertiesFor(guide)) {
    addRangeDrawProperties(guide, properties);
  }
  else {
    copyRangeDrawProperties(guide, properties);
  }

  assertIntegrity();
}



void DataProperties::copyDirectionalStackProperties(
         DataGuide const& guide,
         DataProperties const& properties)
{
  assert(guide.isValid());
  assert(properties.hasRangeDrawPropertiesFor(guide));

  if(!hasRangeDrawPropertiesFor(guide)) {
    addRangeDrawProperties(guide, properties);
  }
  else {
    copyRangeDrawProperties(guide, properties);
  }

  assertIntegrity();

  /// assert(_data->_rangeDrawProperties.find(guide) ==
  ///        _data->_rangeDrawProperties.end());
  /// assert(properties._data->_rangeDrawProperties.find(guide) !=
  ///        properties._data->_rangeDrawProperties.end());

  /// const RangeDrawProps& source = properties.rangeDrawProperties(guide);

  /// // Classifiers.
  /// com::Classifier* displayClassifier = 0;
  /// if(source.displayValueClassifier()) {
  ///   displayClassifier = new com::Classifier(*(source.displayValueClassifier()));
  ///   _data->_rangeClassifiers.push_back(displayClassifier);
  /// }

  /// assert(source.rawValueClassifier());
  /// com::Classifier* rawClassifier =
  ///        new com::Classifier(*(source.rawValueClassifier()));
  /// _data->_rangeClassifiers.push_back(rawClassifier);

  /// // Draw properties.
  /// RangeDrawProps* props = new RangeDrawProps(source.title(),
  ///        source.palette(), rawClassifier, displayClassifier);
  /// _data->_rangeDrawProperties[guide] = props;

  /// assertIntegrity();
}



void DataProperties::setEnabled(const DataGuide& guide,
                   bool enabled)
{
  commonDataProperties(guide).setEnabled(enabled);
}



void DataProperties::setSelected(bool selected)
{
  for(iterator it = begin(); it != end(); ++it) {
    commonDataProperties(*it).setSelected(selected);
  }
}



void DataProperties::setSelected(const DataGuide& guide,
                   bool selected)
{
  commonDataProperties(guide).setSelected(selected);
}



void DataProperties::setSelected(
                   const std::vector<DataGuide>& guides, bool selected)
{
  for(std::vector<DataGuide>::const_iterator it = guides.begin();
                   it != guides.end(); ++it) {

    setSelected(*it, selected);
  }
}



void DataProperties::setPalette(
         DataGuide const& guide,
         com::RawPalette const* palette)
{
  switch(guide.valueScale()) {
    case VS_BOOLEAN: {
      booleanDrawProperties(guide).setPalette(palette);
      break;
    }
    case VS_NOMINAL: {
      nominalDrawProperties(guide).setPalette(palette);
      break;
    }
    case VS_ORDINAL: {
      ordinalDrawProperties(guide).setPalette(palette);
      break;
    }
    case VS_LDD: {
      lddDrawProperties(guide).setPalette(palette);
      break;
    }
    case VS_SCALAR: {
      rangeDrawProperties(guide).setPalette(palette);
      break;
    }
    case VS_DIRECTION: {
      rangeDrawProperties(guide).setPalette(palette);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void DataProperties::mergeDataProperties(
         DataGuide const& guide1,
         DataGuide const& guide2)
{
  assert(guide1.valueScale() == guide2.valueScale());
  assert(guide1.type() == guide2.type());

  switch(guide1.valueScale()) {
    case VS_BOOLEAN: {
      break;
    }
    case VS_NOMINAL: {
      break;
    }
    case VS_ORDINAL: {
      break;
    }
    case VS_LDD: {
      break;
    }
    case VS_SCALAR: {
      mergeRangeDataProperties(guide1, guide2);
      break;
    }
    case VS_DIRECTION: {
      break;
    }
    case VS_UNDEFINED: {
      if(guide1.type() == geo::FEATURE) {
        mergeGeometryDataProperties(guide1, guide2);
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void DataProperties::mergeGeometryDataProperties(
         DataGuide const& guide1,
         DataGuide const& guide2)
{
  // Determine whether guide1 or guide2 already merged with some other guide(s).
  // If both of them already merged, merge the two sets.
  // If one of them already merged, merge the other with that set.
  // If non of them already merged, create a new set of merged properties.

  if(drawPropertiesAreMerged(guide1) && drawPropertiesAreMerged(guide2)) {
    // Not triggered yet so no way to test code.
    assert(false);
  }
  else if(drawPropertiesAreMerged(guide1) && !drawPropertiesAreMerged(guide2)) {
    // Not triggered yet so no way to test code.
    assert(false);
  }
  else if(!drawPropertiesAreMerged(guide1) && drawPropertiesAreMerged(guide2)) {
    GeometryDrawProps* properties = &mergedGeometryDrawProperties(guide2);
//     properties->merge(rangeDrawProperties(guide1));
//     properties->resetCutoffs();
//     properties->classify();
    _data->_mergedGeometryDrawProperties[guide1] = properties;
  }
  else {
    GeometryDrawProps* properties =
         new GeometryDrawProps(geometryDrawProperties(guide2));
//     properties->merge(rangeDrawProperties(guide1));
//     properties->resetCutoffs();
//     properties->classify();
    _data->_mergedGeometryDrawProperties[guide1] = properties;
    _data->_mergedGeometryDrawProperties[guide2] = properties;
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      What about the classifiers which are layered in the properties,
             delete them too, when the properties are deleted (see oldProperties
             below).
*/
void DataProperties::mergeRangeDataProperties(
         DataGuide const& guide1,
         DataGuide const& guide2)
{
  // Determine whether guide1 or guide2 already merged with some other guide(s).
  // If both of them already merged, merge the two sets.
  // If one of them already merged, merge the other with that set.
  // If non of them already merged, create a new set of merged properties.
  if(drawPropertiesAreMerged(guide1) && drawPropertiesAreMerged(guide2)) {
    // Not triggered yet so no way to test code.
    assert(false);

    // RangeDrawProps* newProperties = &mergedRangeDrawProperties(guide2);
    // RangeDrawProps* oldProperties = &mergedRangeDrawProperties(guide1);

    // if(newProperties != oldProperties) {
    //   newProperties->merge(*oldProperties);
    //   newProperties->resetCutoffs();
    //   newProperties->classify();

    //   for(std::map<DataGuide, RangeDrawProps*, ltDataGuide>::iterator it =
    //        _data->_mergedRangeDrawProperties.begin();
    //        it != _data->_mergedRangeDrawProperties.end(); ++it) {
    //     if((*it).second == oldProperties) {
    //       (*it).second = newProperties;
    //     }
    //   }

    //   assert(false);
    //   // Haal de classifiers uit de collecties!
    //   eraseRangeClassifier(oldProperties->rawValueClassifier());
    //   if(oldProperties->displayValueClassifier()) {
    //     eraseRangeClassifier(oldProperties->displayValueClassifier());
    //   }

    //   delete oldProperties;
    // }
  }
  else if(drawPropertiesAreMerged(guide1) && !drawPropertiesAreMerged(guide2)) {
    // Not triggered yet so no way to test code.
    assert(false);

    /*
    RangeDrawProps* properties =
         new RangeDrawProps(rangeDrawProperties(guide2));
    properties->merge(mergedRangeDrawProperties(guide1));
    properties->resetCutoffs();
    properties->classify();

    // delete originele properties van guide1 en vriendjes en laat ze allemaal
    // wijzen naar de nieuwe properties van guide2.
    // ...

    if(newProperties != oldProperties) {
      newProperties->merge(oldProperties);
      newProperties->resetCutoffs();
      newProperties->classify();

      for(std::map<DataGuide, RangeDrawProps*, ltDataGuide>::iterator it =
           _data->_mergedRangeDrawProperties.begin();
           it != _data->_mergedRangeDrawProperties.end(); ++it) {
        if((*it).second == oldProperties) {
          (*it).second = newProperties;
        }
      }

      delete oldProperties;
    }
    */
  }
  else if(!drawPropertiesAreMerged(guide1) && drawPropertiesAreMerged(guide2)) {
    RangeDrawProps* properties = &mergedRangeDrawProperties(guide2);
    properties->merge(rangeDrawProperties(guide1));
    properties->resetCutoffs();
    properties->classify();
    _data->_mergedRangeDrawProperties[guide1] = properties;
  }
  else {
    RangeDrawProps* properties =
         new RangeDrawProps(rangeDrawProperties(guide2));
    properties->merge(rangeDrawProperties(guide1));
    properties->resetCutoffs();
    properties->classify();
    _data->_mergedRangeDrawProperties[guide1] = properties;
    _data->_mergedRangeDrawProperties[guide2] = properties;
  }
}



bool DataProperties::drawPropertiesAreMerged(
         DataGuide const& guide) const
{
  return _data->_mergedRangeDrawProperties.find(guide) !=
         _data->_mergedRangeDrawProperties.end() ||
         _data->_mergedGeometryDrawProperties.find(guide) !=
         _data->_mergedGeometryDrawProperties.end();
}



RangeDrawProps const& DataProperties::mergedRangeDrawProperties(
         DataGuide const& guide) const
{
  return *(*_data->_mergedRangeDrawProperties.find(guide)).second;
}



RangeDrawProps& DataProperties::mergedRangeDrawProperties(
         DataGuide const& guide)
{
  return *(*_data->_mergedRangeDrawProperties.find(guide)).second;
}



GeometryDrawProps const& DataProperties::mergedGeometryDrawProperties(
         DataGuide const& guide) const
{
  return *(*_data->_mergedGeometryDrawProperties.find(guide)).second;
}



GeometryDrawProps& DataProperties::mergedGeometryDrawProperties(
         DataGuide const& guide)
{
  return *(*_data->_mergedGeometryDrawProperties.find(guide)).second;
}



DataProperties::iterator DataProperties::begin()
{
  return _data->_guides.begin();
}



DataProperties::const_iterator DataProperties::begin() const
{
  return _data->_guides.begin();
}



DataProperties::const_reverse_iterator
                   DataProperties::rbegin() const
{
  return _data->_guides.rbegin();
}



DataProperties::iterator DataProperties::end()
{
  return _data->_guides.end();
}



DataProperties::const_iterator DataProperties::end() const
{
  return _data->_guides.end();
}



DataProperties::const_reverse_iterator
         DataProperties::rend() const
{
  return _data->_guides.rend();
}



bool DataProperties::hasCommonDataPropertiesFor(
         DataGuide const& guide) const
{
  return dev::hasElement(*this, guide);
}



bool DataProperties::hasNominalDrawPropertiesFor(
         DataGuide const& guide) const
{
  return _data->_nominalDrawProperties.find(guide) !=
         _data->_nominalDrawProperties.end();
}



bool DataProperties::hasRangeDrawPropertiesFor(
         DataGuide const& guide) const
{
  return _data->_rangeDrawProperties.find(guide) !=
         _data->_rangeDrawProperties.end();
}



DataProperty& DataProperties::commonDataProperties(
                   const DataGuide& guide)
{
  return *_data->_dataProperties[index(guide)];
}



const DataProperty& DataProperties::commonDataProperties(
                   const DataGuide& guide) const
{
  return *_data->_dataProperties[index(guide)];
}



bool DataProperties::isEnabled(const DataGuide& guide) const
{
  return commonDataProperties(guide).isEnabled();
}



bool DataProperties::isSelected(const DataGuide& guide) const
{
  return commonDataProperties(guide).isSelected();
}



bool DataProperties::hasSelectedData() const
{
  bool result = false;

  for(const_iterator it = begin(); it != end(); ++it) {
    if(isSelected(*it)) {
      result = true;
      break;
    }
  }

  return result;
}



std::vector<DataGuide> DataProperties::selectedData() const
{
  std::vector<DataGuide> selectedData;

  for(const_iterator it = begin(); it != end(); ++it) {
    if(isSelected(*it)) {
      selectedData.push_back(*it);
    }
  }

  return selectedData;
}



std::string DataProperties::title(
         DataGuide const& guide) const
{
  std::string title;

  switch(guide.type()) {
    case geo::TIMESERIES: {
      title = nominalDrawProperties(guide).title();
      break;
    }
    case geo::VECTOR: {
      title = rangeDrawProperties(guide).title();
      break;
    }
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          title = booleanDrawProperties(guide).title();
          break;
        }
        case VS_NOMINAL: {
          title = nominalDrawProperties(guide).title();
          break;
        }
        case VS_ORDINAL: {
          title = ordinalDrawProperties(guide).title();
          break;
        }
        case VS_SCALAR: {
          title = rangeDrawProperties(guide).title();
          break;
        }
        case VS_DIRECTION: {
          title = rangeDrawProperties(guide).title();
          break;
        }
        case VS_LDD: {
          title = lddDrawProperties(guide).title();
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
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          title = booleanDrawProperties(guide).title();
          break;
        }
        case VS_NOMINAL: {
          title = nominalDrawProperties(guide).title();
          break;
        }
        case VS_ORDINAL: {
          title = ordinalDrawProperties(guide).title();
          break;
        }
        case VS_SCALAR: {
          title = rangeDrawProperties(guide).title();
          break;
        }
        case VS_UNDEFINED: {
          if(guide.type() == geo::FEATURE) {
            title = geometryDrawProperties(guide).title();
          }

          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return title;
}



const com::RawPalette* DataProperties::palette(
         DataGuide const& guide) const
{
  const com::RawPalette* palette(0);

  switch(guide.type()) {

    case geo::TIMESERIES: {
      assert(guide.valueScale() == VS_SCALAR);
      palette = nominalDrawProperties(guide).palette();
      break;
    }
    case geo::VECTOR: {
      assert(guide.valueScale() == VS_SCALAR);
      palette = rangeDrawProperties(guide).palette();
      break;
    }
    case geo::STACK: {
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          palette = booleanDrawProperties(guide).palette();
          break;
        }
        case VS_NOMINAL: {
          palette = nominalDrawProperties(guide).palette();
          break;
        }
        case VS_ORDINAL: {
          palette = ordinalDrawProperties(guide).palette();
          break;
        }
        case VS_LDD: {
          palette = lddDrawProperties(guide).palette();
          break;
        }
        case VS_SCALAR: {
          palette = rangeDrawProperties(guide).palette();
          break;
        }
        case VS_DIRECTION: {
          palette = rangeDrawProperties(guide).palette();
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
      switch(guide.valueScale()) {
        case VS_BOOLEAN: {
          palette = booleanDrawProperties(guide).palette();
          break;
        }
        case VS_NOMINAL: {
          palette = nominalDrawProperties(guide).palette();
          break;
        }
        case VS_ORDINAL: {
          palette = ordinalDrawProperties(guide).palette();
          break;
        }
        case VS_SCALAR: {
          palette = rangeDrawProperties(guide).palette();
          break;
        }
        case VS_UNDEFINED: {
          if(guide.type() == geo::FEATURE) {
            palette = geometryDrawProperties(guide).palette();
          }
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  assert(palette);

  return palette;
}



std::vector<const com::RawPalette*> DataProperties::palettes(
         CSF_VS valueScale) const
{
  std::vector<const com::RawPalette*> palettes;

  switch(valueScale) {

    case VS_BOOLEAN:
    case VS_NOMINAL:
    case VS_LDD: {
      palettes = com::classPalettes();
      break;
    }

    case VS_ORDINAL:
    case VS_SCALAR:
    case VS_DIRECTION: {
      palettes = com::rangePalettes();
      break;
    }

    default: {
      assert(false);
      break;
    }
  }

  assert(!palettes.empty());

  return palettes;
}



DrawProps& DataProperties::geometryDrawProperties(
         DataGuide const& guide)
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_geometryDrawProperties.find(guide) !=
         _data->_geometryDrawProperties.end());

  if(drawPropertiesAreMerged(guide)) {
    return mergedGeometryDrawProperties(guide);
  }
  else {
    return *(*_data->_geometryDrawProperties.find(guide)).second;
  }
}



DrawProps const& DataProperties::geometryDrawProperties(
         DataGuide const& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_geometryDrawProperties.find(guide) !=
         _data->_geometryDrawProperties.end());

  if(drawPropertiesAreMerged(guide)) {
    return mergedGeometryDrawProperties(guide);
  }
  else {
    return *(*_data->_geometryDrawProperties.find(guide)).second;
  }
}



BooleanDrawProps& DataProperties::booleanDrawProperties(
                   const DataGuide& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_booleanDrawProperties.find(guide) !=
                   _data->_booleanDrawProperties.end());

  return *(*_data->_booleanDrawProperties.find(guide)).second;
}



NominalDrawProps& DataProperties::nominalDrawProperties(
                   const DataGuide& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_nominalDrawProperties.find(guide) !=
                   _data->_nominalDrawProperties.end());

  return *(*_data->_nominalDrawProperties.find(guide)).second;
}



OrdinalDrawProps& DataProperties::ordinalDrawProperties(
                   const DataGuide& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_ordinalDrawProperties.find(guide) !=
                   _data->_ordinalDrawProperties.end());

  return *(*_data->_ordinalDrawProperties.find(guide)).second;
}



LddDrawProps& DataProperties::lddDrawProperties(
         const DataGuide& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_lddDrawProperties.find(guide) !=
                   _data->_lddDrawProperties.end());

  return *(*_data->_lddDrawProperties.find(guide)).second;
}



ClassDrawProps& DataProperties::classDrawProperties(
         const DataGuide& guide) const
{
  ClassDrawProps* result = 0;

  switch(guide.valueScale()) {
    case VS_BOOLEAN: { result = &booleanDrawProperties(guide); break; }
    case VS_NOMINAL: { result = &nominalDrawProperties(guide); break; }
    case VS_ORDINAL: { result = &ordinalDrawProperties(guide); break; }
    case VS_LDD:     { result = &lddDrawProperties(guide); break; }
    case VS_NOTDETERMINED:
    case VS_CLASSIFIED:
    case VS_CONTINUOUS:
    case VS_SCALAR:
    case VS_DIRECTION:
    case VS_UNDEFINED: {
      result = 0;
      break;
    }
  }

  assert(result);

  return *result;
}



RangeDrawProps const& DataProperties::rangeDrawProperties(
         const DataGuide& guide) const
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_rangeDrawProperties.find(guide) !=
                   _data->_rangeDrawProperties.end());

  if(drawPropertiesAreMerged(guide)) {
    return mergedRangeDrawProperties(guide);
  }
  else {
    return *(*_data->_rangeDrawProperties.find(guide)).second;
  }
}



RangeDrawProps& DataProperties::rangeDrawProperties(
         const DataGuide& guide)
{
  assert(hasCommonDataPropertiesFor(guide));
  assert(_data->_rangeDrawProperties.find(guide) !=
                   _data->_rangeDrawProperties.end());

  if(drawPropertiesAreMerged(guide)) {
    return mergedRangeDrawProperties(guide);
  }
  else {
    return *(*_data->_rangeDrawProperties.find(guide)).second;
  }
}



DrawProps& DataProperties::drawProperties(
         DataGuide const& guide)
{
  DrawProps* result = 0;

  switch(guide.valueScale()) {
    case VS_BOOLEAN:   { result = &booleanDrawProperties(guide); break; }
    case VS_NOMINAL:   { result = &nominalDrawProperties(guide); break; }
    case VS_ORDINAL:   { result = &ordinalDrawProperties(guide); break; }
    case VS_LDD:       { result = &lddDrawProperties(guide); break; }
    case VS_SCALAR:    { result = &rangeDrawProperties(guide); break; }
    case VS_DIRECTION: { result = &rangeDrawProperties(guide); break; }
    case VS_UNDEFINED: {
      if(guide.type() == geo::FEATURE) {
        result = &geometryDrawProperties(guide);
      }

      break;
    }
    case VS_NOTDETERMINED:
    case VS_CLASSIFIED:
    case VS_CONTINUOUS: {
      break;
    }
  }

  assert(result);

  return *result;
}



DrawProps const& DataProperties::drawProperties(
         DataGuide const& guide) const
{
  return const_cast<DataProperties&>(*this).drawProperties(guide);
}



QColor const& DataProperties::colour(DataGuide const& guide) const
{
  return drawProperties(guide).colour();
}



template<>
std::string DataProperties::label<UINT1>(
         DataGuide const& guide,
         UINT1 const& value) const
{
  std::string result;

  switch(guide.valueScale()) {
    case VS_BOOLEAN: {
      result = booleanDrawProperties(guide).label(value);
      break;
    }
    case VS_LDD: {
      result = lddDrawProperties(guide).label(value);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



template<>
std::string DataProperties::label<INT4>(
         DataGuide const& guide,
         INT4 const& value) const
{
  std::string result;

  switch(guide.valueScale()) {
    case VS_NOMINAL: {
      result = nominalDrawProperties(guide).label(value);
      break;
    }
    case VS_ORDINAL: {
      result = ordinalDrawProperties(guide).label(value);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



template<>
std::string DataProperties::label<REAL4>(
         DataGuide const& guide,
         REAL4 const& value) const
{
  std::string result;

  switch(guide.valueScale()) {
    case VS_SCALAR:
    case VS_DIRECTION: {
      result = rangeDrawProperties(guide).label(value);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



void DataProperties::replaceClassifier(
         DataGuide const& guide,
         com::Classifier const& classifier)
{
  popClassifier(guide);
  pushClassifier(guide, classifier);
}



void DataProperties::pushClassifier(
         DataGuide const& guide,
         com::Classifier const& classifier)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  RangeDrawProps& properties(rangeDrawProperties(guide));

  com::Classifier* raw = new com::Classifier(classifier);
  _data->_rangeClassifiers.push_back(raw);
  com::Classifier* display = 0;

  RangeDrawProps::ClassifierTuple tuple(raw, display);
  properties.classifiers().push_back(tuple);
  properties.classify();
}



void DataProperties::popClassifier(
         DataGuide const& guide)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  RangeDrawProps& properties(rangeDrawProperties(guide));
  assert(properties.classifiers().size() > 1);

  RangeDrawProps::ClassifierTuple& tuple(properties.classifiers().back());
  com::Classifier* raw(boost::get<0>(tuple));
  com::Classifier* display(boost::get<1>(tuple));

  assert(raw);
  eraseRangeClassifier(raw);

  if(display) {
    eraseRangeClassifier(display);
  }

  properties.classifiers().pop_back();
  properties.classify();
}



void DataProperties::popClassifiers(
         DataGuide const& guide)
{
  assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
  assert(guide.valueScale() == VS_SCALAR);

  RangeDrawProps& properties(rangeDrawProperties(guide));
  // Some properties have been merged (scenarios). This results in the assertion
  // failing for the properties of the all-but-the-first scenario. So, accept
  // properties with only 1 classifier too.
  // assert(properties.classifiers().size() > 1);

  while(properties.classifiers().size() > 1) {
    popClassifier(guide);
  }
}



void DataProperties::eraseRangeClassifier(com::Classifier* classifier)
{
  assert(classifier);
  std::vector<com::Classifier*>::iterator it =
         std::find(_data->_rangeClassifiers.begin(),
              _data->_rangeClassifiers.end(), classifier);
  assert(it != _data->_rangeClassifiers.end());
  delete *it;
  _data->_rangeClassifiers.erase(it);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

