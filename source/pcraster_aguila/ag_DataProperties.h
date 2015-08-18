#ifndef INCLUDED_AG_DATAPROPERTIES
#define INCLUDED_AG_DATAPROPERTIES



// Library headers.
#include <memory>
#include <vector>
#include <boost/noncopyable.hpp>
#include <QColor>

// PCRaster library headers.
#include "csftypes.h"

// Module headers.



namespace com {
  class BasicTable;
  class Classifier;
  class RawPalette;
}
namespace geo {
  template<class T> class SimpleRaster;
}
namespace ag {
  // DataProperties declarations.
  class BooleanDrawProps;
  class ClassDrawProps;
  class DataGuide;
  class DataObject;
  class DataPropertiesPrivate;
  class DataProperty;
  class DrawProps;
  class LddDrawProps;
  class NominalDrawProps;
  class OrdinalDrawProps;
  class RangeDrawProps;
}



namespace ag {

typedef DrawProps GeometryDrawProps;



//! This class keeps track of the properties of data.
/*!
  Here data is coupled to visualisation properties.

  \todo The non-const begin(), end(), properties() are public but should be
        private. Gcc 2.96 doesn't seem to compile if they are private. It
        wants to link all const calls to those functions to the non-const and
        private versions instead of the const and public versions. Be sure not
        to use the non-const versions outside the scope of this class. Move
        functions back to private after a compiler upgrade.

  \todo Move general stuff like title from vs specific property object to
        general property object.

  \todo Refactor this class!
*/
class DataProperties: private boost::noncopyable
{

private:

  typedef std::vector<DataGuide>::iterator iterator;

  std::auto_ptr<DataPropertiesPrivate> _data;

  void             assertIntegrity     ();

  size_t           index               (const DataGuide& guide) const;

  void             add                 (const DataGuide& guide);

  void             add                 (const DataGuide& guide,
                                        const DataProperty& dataProperty);

  void             copy                (DataGuide const& guide,
                                        DataProperty const& properties);

  bool             hasCommonDataPropertiesFor(
                                        const DataGuide& guide) const;

  bool             hasNominalDrawPropertiesFor(
                                        DataGuide const& guide) const;

  bool             hasRangeDrawPropertiesFor(
                                        DataGuide const& guide) const;

  void             removeBooleanDrawProperties(
                                        const DataGuide& guide);

  void             removeNominalDrawProperties(
                                        const DataGuide& guide);

  void             removeOrdinalDrawProperties(
                                        const DataGuide& guide);

  void             removeLddDrawProperties(
                                        const DataGuide& guide);

  void             removeRangeDrawProperties(
                                        const DataGuide& guide);

  void             removeScalarTimeSeriesProperties(
                                        const DataGuide& guide);

  void             removeVectorPropertiesProperties(
                                        DataGuide const& guide);

  void             removeBooleanStackProperties(
                                        const DataGuide& guide);

  void             removeNominalStackProperties(
                                        const DataGuide& guide);

  void             removeOrdinalStackProperties(
                                        const DataGuide& guide);

  void             removeScalarStackProperties(
                                        const DataGuide& guide);

  void             removeDirectionalStackProperties(
                                        const DataGuide& guide);

  void             removeLddStackProperties(
                                        const DataGuide& guide);

  void             removeBooleanFeatureProperties(
                                        DataGuide const& guide);

  void             removeNominalFeatureProperties(
                                        DataGuide const& guide);

  void             removeOrdinalFeatureProperties(
                                        DataGuide const& guide);

  void             removeScalarFeatureProperties(
                                        DataGuide const& guide);

  void             addNominalDrawProperties(
                                        DataGuide const& guide,
                                        const DataProperties& properties);

  void             addRangeDrawProperties(
                                        DataGuide const& guide,
                                        const DataProperties& properties);

  void             copyNominalDrawProperties(
                                        DataGuide const& guide,
                                        const DataProperties& properties);

  void             copyRangeDrawProperties(
                                        DataGuide const& guide,
                                        const DataProperties& properties);

  void             copyScalarTimeSeriesProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyVectorProperties(const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyGeometryProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyBooleanStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyNominalStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyOrdinalStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyLddStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyScalarStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  void             copyDirectionalStackProperties(
                                        const DataGuide& guide,
                                        const DataProperties& properties);

  bool             drawPropertiesAreMerged(DataGuide const& guide) const;

  RangeDrawProps&  mergedRangeDrawProperties(DataGuide const& guide);

  RangeDrawProps const& mergedRangeDrawProperties(
                                        DataGuide const& guide) const;

  GeometryDrawProps&  mergedGeometryDrawProperties(
                                        DataGuide const& guide);

  GeometryDrawProps const& mergedGeometryDrawProperties(DataGuide const& guide) const;

  void             eraseRangeClassifier(com::Classifier* classifier);

public:

  typedef std::vector<DataGuide>::const_iterator const_iterator;

  typedef std::vector<DataGuide>::const_reverse_iterator const_reverse_iterator;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataProperties      ();

  /* virtual */    ~DataProperties     ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addGeometryDataProperties(
                                        DataObject const& dataObject,
                                        DataGuide const& guide);

  void             addScalarTimeSeriesProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addBooleanStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addNominalStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addOrdinalStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addScalarStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addDirectionalStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addLddStackProperties(
                                        const DataObject& dataObject,
                                        const DataGuide& guide);

  void             addBooleanFeatureProperties(
                                        DataObject const& dataObject,
                                        DataGuide const& guide);

  void             addNominalFeatureProperties(
                                        DataObject const& dataObject,
                                        DataGuide const& guide);

  void             addOrdinalFeatureProperties(
                                        DataObject const& dataObject,
                                        DataGuide const& guide);

  void             addScalarFeatureProperties(
                                        DataObject const& dataObject,
                                        DataGuide const& guide);

  void             addVectorProperties (DataObject const& dataObject,
                                        DataGuide const& guide);

  void             remove              (const DataGuide& guide);

  void             copy                (const DataGuide& guide,
                                        const DataProperties& properties);

  void             setEnabled          (const DataGuide& guide,
                                        bool enabled);

  void             setSelected         (bool selected);

  void             setSelected         (const DataGuide& guide,
                                        bool selected);

  void             setSelected    (const std::vector<DataGuide>& guides,
                                   bool selected);

  void             setPalette          (const DataGuide& guide,
                                        const com::RawPalette* palette);

  void             mergeDataProperties (DataGuide const& guide1,
                                        DataGuide const& guide2);

  void             mergeRangeDataProperties(DataGuide const& guide1,
                                            DataGuide const& guide2);

  void             mergeGeometryDataProperties(
                                        DataGuide const& guide1,
                                        DataGuide const& guide2);

  void             replaceClassifier   (DataGuide const& guide,
                                        com::Classifier const& classifier);

  void             pushClassifier      (DataGuide const& guide,
                                        com::Classifier const& classifier);

  void             popClassifier       (DataGuide const& guide);

  void             popClassifiers      (DataGuide const& guide);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  iterator         begin               ();

  const_iterator   begin               () const;

  const_reverse_iterator rbegin        () const;

  iterator         end                 ();

  const_iterator   end                 () const;

  const_reverse_iterator rend          () const;

  DataProperty&    commonDataProperties    (const DataGuide& guide);

  const DataProperty& commonDataProperties (const DataGuide& guide) const;

  bool             isEnabled           (const DataGuide& guide) const;

  bool             isSelected          (const DataGuide& guide) const;

  bool             hasSelectedData     () const;

  std::vector<DataGuide> selectedData  () const;

  std::string      title               (DataGuide const& guide) const;

  const com::RawPalette* palette       (const DataGuide& guide) const;

  std::vector<const com::RawPalette*> palettes(CSF_VS valueScale) const;

  DrawProps&       drawProperties      (DataGuide const& guide);

  DrawProps const& drawProperties      (DataGuide const& guide) const;

  DrawProps&       geometryDrawProperties(
                                        DataGuide const& guide);

  DrawProps const& geometryDrawProperties(
                                        DataGuide const& guide) const;

  ag::BooleanDrawProps& booleanDrawProperties(
                                        const DataGuide& guide) const;

  ag::NominalDrawProps& nominalDrawProperties(
                                        const DataGuide& guide) const;

  ag::OrdinalDrawProps& ordinalDrawProperties(
                                        const DataGuide& guide) const;

  LddDrawProps&    lddDrawProperties   (const DataGuide& guide) const;

  ClassDrawProps&  classDrawProperties (const DataGuide& guide) const;

  RangeDrawProps&  rangeDrawProperties (DataGuide const& guide);

  RangeDrawProps const& rangeDrawProperties(DataGuide const& guide) const;

  QColor const&    colour              (DataGuide const& guide) const;

  template<typename T>
  std::string      label               (DataGuide const& guide,
                                        T const& value) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
