#ifndef INCLUDED_AG_DATAOBJECT
#define INCLUDED_AG_DATAOBJECT



// Library headers.
#include <cassert>
#include <vector>
#include <QObject>
#ifndef Q_MOC_RUN
#include <boost/noncopyable.hpp>

// PCRaster library headers.
#include "dal_MapperUtils.h"
#include "dal_Utils.h"
#endif

// Module headers.
#include "ag_Configure.h"
#include "ag_DataGuide.h"
#include "ag_DataProperties.h"
#ifndef Q_MOC_RUN
#include "ag_FeatureDataSources.h"
#endif
#include "ag_RangeDrawProps.h"
#ifndef Q_MOC_RUN
#include "ag_RasterDataSources.h"
#endif
#ifndef Q_MOC_RUN
#include "ag_TableDataSources.h"
#include "ag_VectorDataSources.h"
#endif
#include "ag_Types.h"
#include "ag_VisSubject.h"



class QPointF;
namespace dal {
  class DataSpace;
  class DataSpaceAddress;
  class DataSpaceAddressMapper;
  class Raster;
}
namespace com {
  class RawPalette;
}
namespace pcrxml {
  class DrawProperties;
  class DateMapper;
}
namespace qt {
  class Animation;
}
namespace ag {
  // DataObject declarations.
  class DataObjectPrivate;
  class FeatureDataSources;
  class TableDataSources;
  class RasterDataSources;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo This is a God class
        (http://en.wikipedia.org/wiki/God_object), refactor.
*/
class PCR_AG_DECL DataObject: public QObject,
                              private boost::noncopyable,
                              public ag::VisSubject
{

private:

  Q_OBJECT

  DataObjectPrivate* d_data;

  std::vector<com::RawPalette*> d_palettesFromXML;

  size_t           nrDataSets          () const;

  size_t           nrSpatialDataSets   () const;

  void             postNotify          ();

  bool             notifyNeeded        () const;

  DataGuide        addStack            (std::string const& name,
                                        dal::DataSpace const& space);

  DataGuide        addFeatureLayer     (std::string const& name,
                                        dal::DataSpace const& space);

  DataGuide        addTimeSeries       (std::string const& name,
                                        dal::DataSpace const& space);

  DataGuide        addVector           (std::string const& name,
                                        dal::DataSpace const& space);

  void             localStepMappings   (
                   std::vector<dal::DimensionTimeStepMapping>& timeMappings,
                   std::vector<dal::DimensionSpaceStepMapping>& spaceMappings);

  void             globalStepMappings  (
              std::vector<dal::DimensionTimeStepMapping> const& timeMappings,
              std::vector<dal::DimensionSpaceStepMapping> const& spaceMappings,
              std::vector<dal::StepMapper>& timeStepMappers,
              std::vector<dal::StepMapper>& spaceStepMappers,
              dal::TimeStepMapper& timeStepMapper,
              dal::SpaceStepMapper& spaceStepMapper);

  void             setGlobalToWorldMappers(
                        std::vector<dal::StepMapper> const& timeStepMappers,
                        std::vector<dal::StepMapper> const& spaceStepMappers);

  void             setGlobalToWorldMapper(
                                  dal::TimeStepMapper const& timeStepMapper,
                                  dal::SpaceStepMapper const& spaceStepMapper);

  void             reconfigureAnimationManager();

  void             reconfigureDataSpaceAndMappers();

  dal::RasterDimensions const& rasterDimensions() const;

  dal::SpaceDimensions const& featureDimensions() const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataObject          ();

  /* virtual */    ~DataObject         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             notify              ();

  void             setDataSpaceAddress (dal::DataSpaceAddress const& address,
                                        bool notify=true);

  void             setMap2DZoom        (double zoom,
                                        bool notify=true);

  void             map2DZoomBy         (double factor,
                                        bool notify=true);

  void             setMap2DScale       (double scale,
                                        bool notify=true);

  void             map2DMoveBy         (double dx,
                                        double dy,
                                        bool notify=true);

  void             map2DMoveBy         (QPointF const& offset,
                                        bool notify=true);

  void             setQuadLength       (size_t quadLength,
                                        bool notify=true);

  void             setMap3DScale       (double scale,
                                        bool notify=true);

  void             read                ();

  template<class T>
  bool             compatibleData      (T const& dataset,
                                        dal::DataSpace const& space);

  bool             compatibleData      (std::string const& name,
                                        dal::DataSpace const& space);

  DataGuide        add                 (std::string const& name,
                                        dal::DataSpace const& space);

  void             clear               ();

  void             remove              (DataGuide const& guide);

  void             setEnabled          (DataGuide const& guide,
                                        bool enabled);

  void             setSelected         (bool selected,
                                        bool notify=true);

  void             setSelected         (DataGuide const& dataGuide,
                                        bool selected,
                                        bool notify=true);

  void             setSelected         (std::vector<DataGuide> const& guides,
                                        bool selected);

  void             setXML              (DataGuide const& guide,
                                        pcrxml::DrawProperties const& dp,
                                        bool notify=true);

  void             setDateMapper       (DataGuide const& guide,
                                        pcrxml::DateMapper const& dm,
                                        bool notify=true);


  void             setPalette          (DataGuide const& guide,
                                        com::RawPalette const* palette,
                                        bool notify=true);

  void             setNrClasses        (DataGuide const& guide,
                                        size_t nrClasses,
                                        bool notify=true);

  void             setMaxCutoff        (DataGuide const& guide,
                                        double maxCutoff,
                                        bool notify=true);

  void             setMinCutoff        (DataGuide const& guide,
                                        double minCutoff,
                                        bool notify=true);

  void             setCutoffs          (DataGuide const& guide,
                                        double minCutoff,
                                        double maxCutoff,
                                        bool notify=true);

  void             setClassificationMode(DataGuide const& guide,
                                        RangeDrawProps::Mode mode,
                                        bool notify=true);

  void             setProbabilityScale (DataGuide const& guide,
                                        RangeDrawProps::ProbabilityScale scale,
                                        bool notify=true);

  void             setClassificationAlgorithm(
                                        DataGuide const& guide,
                                        RangeDrawProps::Algorithm algorithm,
                                        bool notify=true);

  void             setClassificationProperties(
                                        DataGuide const& guide,
                                        RangeDrawProps::Algorithm algorithm,
                                        double minCutoff,
                                        double maxCutoff,
                                        bool notify=true);

  void             setDrawerType       (DataGuide const& guide,
                                        DrawerType type,
                                        bool notify=true);

  void             mergeDataProperties (DataGuide const& guide1,
                                        DataGuide const& guide2);

  void             setSelectedValue    (REAL4 value,
                                        bool notify=true);

  void             unsetSelectedValue  (bool notify=true);

  // template<class T>
  // void             setSelectedValue    (DataGuide const& guide,
  //                                       T const& value,
  //                                       bool notify=true);

  // void             unsetSelectedValue  (DataGuide const& guide,
  //                                       bool notify=true);

  void             replaceClassifier   (DataGuide const& guide,
                                        com::Classifier const& classifier,
                                        bool notify=true);

  void             pushClassifier      (DataGuide const& guide,
                                        com::Classifier const& classifier,
                                        bool notify=true);

  void             popClassifier       (DataGuide const& guide,
                                        bool notify=true);

  void             popClassifiers      (DataGuide const& guide,
                                        bool notify=true);

  void             setBackgroundColour (QColor const& color,
                                        bool notify=true);

  void             setXY               (double x,
                                        double y,
                                        bool notify=true);

  void             unsetCoordinates    (dal::Meaning meaning,
                                        bool notify=true);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void             setNotifyNeeded     (bool needed);

  bool             isValid             (DataGuide const& guide) const;

  int              firstTimeStep       () const;

  int              lastTimeStep        () const;

  size_t           timeSpan            () const;

  std::string      name                (DataGuide const& guide) const;

  std::string      description         (DataGuide const& guide) const;

  std::string      title               (DataGuide const& guide) const;

  std::string      label               (DataGuide const& guide) const;

  template<typename T>
  bool             value               (T& result,
                                        DataGuide const& guide) const;

  dal::DataSpace const& dataSpace      () const;

  dal::DataSpace   dataSpace           (DataGuide const& guide) const;

  dal::DataSpace   dataSpace           (std::vector<DataGuide> const& guides) const;

  dal::DataSpaceAddress const& dataSpaceAddress() const;

  double           map2DZoom           () const;

  double           map2DScale          () const;

  QPointF const&   map2DOffset         () const;

  size_t           quadLength          () const;

  double           map3DScale          () const;

  DataGuide const& dataGuide           (geo::DataGuide const& guide) const;

  std::vector<DataGuide> dataGuides    () const;

  TableDataSources& tableDataSources   ();

  TableDataSources const& tableDataSources() const;

  RasterDataSources& rasterDataSources ();

  RasterDataSources const& rasterDataSources() const;

  FeatureDataSources& featureDataSources();

  FeatureDataSources const& featureDataSources() const;

  VectorDataSources& vectorDataSources ();

  VectorDataSources const& vectorDataSources() const;

  DataProperties&  properties          ();

  Dataset&         dataset             (DataGuide const& guide);

  DataProperties const& properties     () const;

  bool             isAvailable         (DataGuide const& guide) const;

  bool             isEnabled           (DataGuide const& guide) const;

  bool             isSelected          (DataGuide const& guide) const;

  bool             hasSelectedData     () const;

  std::vector<DataGuide> selectedData  () const;

  bool             hasSelectedValue    () const;

  // bool             hasSelectedValue    (DataGuide const& guide) const;

  REAL4            selectedValue       () const;

  // template<class T>
  // T                selectedValue       (DataGuide const& guide) const;

  qt::Animation&   animationManager    () const;

  size_t           nrClasses           (DataGuide const& guide) const;

  double           maxCutoff           (DataGuide const& guide) const;

  double           minCutoff           (DataGuide const& guide) const;

  bool             hasSpace            (DataGuide const& guide) const;

  bool             hasTimeSeries       (DataGuide const& guide) const;

  bool             hasCumProbabilities (DataGuide const& guide) const;

  dal::DataSpaceAddressMapper const& globalToWorldMapper() const;

  dal::DataSpaceAddressMapper& localToWorldMapper(
                                        DataGuide const& guide) const;

  dal::DataSpaceAddressMapper& globalToLocalMapper(
                                        DataGuide const& guide) const;

  dal::SpaceDimensions envelope        () const;

  dal::SpatialCoordinate const& spatialAddress() const;

public Q_SLOTS:

  void             setTimeStep         (size_t time);

  void             setQuantile         (float quantile,
                                        bool notify=true);

  QColor const&    backgroundColour    () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

// template<class T>
// inline T ag::DataObject::selectedValue(
//          DataGuide const& guide) const
// {
//   assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
//   assert(guide.valueScale() == VS_SCALAR);
//
//   T result;
//
//   switch(guide.type()) {
//     case geo::STACK: {
//       result = rasterDataSources().data(guide).selectedValue<T>();
//       break;
//     }
//     case geo::FEATURE: {
//       result = featureDataSources().data(guide).selectedValue<T>();
//       break;
//     }
//     default: {
//       assert(false);
//       break;
//     }
//   }
//
//   return result;
// }

// template<class T>
// inline void ag::DataObject::setSelectedValue(
//          DataGuide const& guide,
//          T const& value,
//          bool notify)
// {
//   assert(guide.type() == geo::STACK || guide.type() == geo::FEATURE);
//   assert(guide.valueScale() == VS_SCALAR);
//
//   if(!hasSelectedValue(guide) ||
//          !dal::comparable<T>(selectedValue<T>(guide), value)) {
//     switch(guide.type()) {
//       case geo::STACK: {
//         rasterDataSources().data(guide).setSelectedValue<T>(value);
//         break;
//       }
//       case geo::FEATURE: {
//         featureDataSources().data(guide).setSelectedValue<T>(value);
//         break;
//       }
//       default: {
//         assert(false);
//         break;
//       }
//     }
//
//     setNotifyNeeded(true);
//   }
//
//   if(notify) {
//     this->notify();
//   }
// }

template<typename T>
inline bool DataObject::value(
         T& result,
         DataGuide const& guide) const
{
  bool isValid = false; // Shut up compiler.

  switch(guide.type()) {
    case geo::STACK: {
      isValid = rasterDataSources().data(guide).value<T>(result,
         dataSpace(), dataSpaceAddress());

      // <hack>
      if(guide.valueScale() == VS_SCALAR && isValid && hasSelectedValue() &&
         dataSpace(guide).hasCumProbabilities() &&
         properties().rangeDrawProperties(guide).probabilityScale() ==
              RangeDrawProps::ExceedanceProbabilities) {
        result = REAL4(1.0) - result;
      }
      // </hack>

      break;
    }
    case geo::FEATURE: {
      isValid = featureDataSources().data(guide).value<T>(result,
         dataSpace(), dataSpaceAddress());

      // <hack>
      if(guide.valueScale() == VS_SCALAR && isValid && hasSelectedValue() &&
         dataSpace(guide).hasCumProbabilities() &&
         properties().rangeDrawProperties(guide).probabilityScale() ==
              RangeDrawProps::ExceedanceProbabilities) {
        result = REAL4(1.0) - result;
      }
      // </hack>

      break;
    }
    case geo::VECTOR: {
      isValid = vectorDataSources().data(guide).value<T>(result,
         dataSpace(), dataSpaceAddress());
      break;
    }
    case geo::TIMESERIES: {
      isValid = tableDataSources().data(guide).value<T>(result,
         dataSpace(), dataSpaceAddress());
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return isValid;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
