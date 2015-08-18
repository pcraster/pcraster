#ifndef INCLUDED_DAL_DATASPACE
#define INCLUDED_DAL_DATASPACE



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DATASPACEITERATOR
#include "dal_DataSpaceIterator.h"
#define INCLUDED_DAL_DATASPACEITERATOR
#endif

#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif



namespace dal {
  // DataSpace declarations.
  class DataSpaceAddress;
}



namespace dal {



//! Class for describing the dimensional properties of a dataset.
/*!
  The data space contains one or more dimensions which together describe the
  dimensionality of the dataset.

  In case of a spatial dataset, the first spatial dimension is the dimension
  in the y direction (rows), the second is the dimension in the x direction
  (columns) and the optional third is the dimension is the z direction.

  There is a fixed number of combinations of dimensions which can be
  combined to a valid data space. For example, it is valid to have a data
  space with one scenario dimension folowed by two space dimensions. This
  is a static raster. It is not valid to have a data space with a space
  dimension folowed by a scenario dimension. This would be a raster with
  per cell different values per scenario.

  The order of dimensions matters when iterating over a data space. The
  order of dimensions determines how iteration takes place. The
  last dimension in the data space is iterated over first, then the
  one before that, etc. Some functions assume that dimensions are
  ordered in a predictable way, for example DataSource::read(Raster&,
  DataSpaceAddress).

  Currently we support this set of valid combinations of dimensions in
  a data space:

  Without Scenarios, Samples and CumulativeProbabilities.
  - Time (time series)
  - Space(/Space) (raster and/or feature layer)
  - Time/Space(/Space) (dynamic raster or feature layer: stack)

  With Scenarios.
  - Scenarios/Space(/Space) (for each scenario a raster or feature layer)
  - Scenarios/Time (for each scenario a time series)
  - Scenarios/Time/Space(/Space) (for each scenario a stack)

  With Samples.
  - Scenarios/Samples/Space(/Space)
  - Scenarios/Samples/Time
  - Scenarios/Samples/Time/Space(/Space)

  With CumulativeProbabilities.
  - Scenarios/CumulativeProbabilities/Space(/Space)
  - Scenarios/Time/CumulativeProbabilities
  - Scenarios/Time/CumulativeProbabilities/Space(/Space)

  The folowing holds for a valid data space:
  - Scenarios always at the beginning
  - Space always at the end
  - Samples OR CumulativeProbabilities dimension
  - Time BEFORE CumulativeProbabilities
  - Time AFTER Samples

  A data space can contain information about a raster space and a vector space.
  These are defined by 4 space dimensions. The raster dimensions should always
  come before the vector dimensions.

  \todo      The way the spatial dimensions are handled might not suffice in
             all use cases. What about a cross section data set with one
             horizontal dimension and one vertical, for example. Maybe explicit
             spatial dimensions must be added: XSpace, YSpace, ZSpace or
             something similar, for example.
  \todo      Rethink names of members dealing with the amount of dimensions
             in the space (rank(), size()) and with the amount of coordinates
             in the space (isEmpty()). size() and isEmpty() should mean the
             same thing (dealing both with coordinates or with dimensions).
*/
class PCR_DAL_DECL DataSpace
{

  friend class DataSpaceTest;

private:

  static Meaning _meanings[];

  static size_t _nrMeanings;

  //! Dimensions of the data space.
  std::vector<Dimension> _dimensions;

#ifdef DEBUG_DEVELOP
  void             checkConsistency    ();
#endif

  DataSpace&       merge               (DataSpace const& space,
                                        Meaning const* meanings,
                                        size_t nrMeanings);

  void             insertDimension     (size_t index,
                                        Dimension const& dimension);

  void             appendDimension     (Dimension const& dimension);

public:

  enum IntersectionOptions {
    //! Keep the coordinates of the shared dimensions as they are.
    DontIntersectCoordinates = 0x00000001,

    //! Keep the non shared dimensions (but they will be cleared of coord's).
    KeepNonSharedDimensions = 0x00000002
  };

  typedef size_t IntersectionFlags;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DataSpace           ();

                   DataSpace           (Dimension const& dimension);

                   DataSpace           (DataSpace const& space,
                                        DataSpaceAddress address);

                   DataSpace           (DataSpace const& space,
                                        std::vector<DataSpaceAddress> const& addresses);

  /* virtual */    ~DataSpace          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  DataSpace&       operator+=          (DataSpace const& space);

  DataSpace&       operator|=          (DataSpace const& space);

  DataSpace&       operator&=          (DataSpace const& space);

  DataSpace&       intersect           (DataSpace const& space,
                                        IntersectionFlags options = 0);

  void             addDimension        (Dimension const& dimension);

  void             eraseDimension      (size_t index);

  void             eraseDimension      (Meaning meaning);

  DataSpaceAddress eraseCoordinates    (DataSpaceAddress const& address,
                                        Meaning meaning) const;

  DataSpaceAddress unsetCoordinates    (DataSpaceAddress const& address,
                                        Meaning meaning) const;

  void             clear               ();

  void             replaceDimension    (size_t index,
                                        Dimension const& dimension);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (DataSpace const& rhs) const;

  size_t           rank                () const;

  size_t           size                () const;

  bool             isEmpty             () const;

  size_t           nrWideDimensions    () const;

  size_t           indexOfWideDimension() const;

  bool             isCompatible        (DataSpace const& space) const;

  DataSpaceIterator begin              () const;

  DataSpaceIterator rbegin             () const;

  DataSpaceIterator end                () const;

  DataSpaceIterator rend               () const;

  Dimension const& dimension           (size_t index) const;

  Dimension&       dimension           (size_t index);

  Dimension const& dimension           (Meaning meaning) const;

  size_t           indexOf             (Meaning meaning) const;

  size_t           indexOf             (Dimension const* dimension) const;

  size_t           indexOf             (Dimension const& dimension) const;

  bool             isSpatial           () const;

  bool             hasScenarios        () const;

  bool             hasSamples          () const;

  bool             hasTime             () const;

  bool             hasCumProbabilities () const;

  bool             hasRaster           () const;

  bool             hasFeatures         () const;

  bool             hasSpace            () const;

  bool             equal               (DataSpaceAddress const& lhs,
                                        DataSpaceAddress const& rhs) const;

  DataSpaceAddress trim                (DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  DataSpaceAddress initialiseInvalidCoordinates(
                                        DataSpaceAddress const& address) const;

  DataSpaceAddress address             () const;

  bool             contains            (DataSpaceAddress const& address) const;

  bool             isValid             (DataSpaceAddress const& address) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (DataSpace const& lhs,
                                        DataSpace const& rhs);

PCR_DAL_DECL bool  operator!=          (DataSpace const& lhs,
                                        DataSpace const& rhs);

DataSpace          operator|           (DataSpace const& lhs,
                                        DataSpace const& rhs);

DataSpace          operator&           (DataSpace const& lhs,
                                        DataSpace const& rhs);

DataSpace          operator+           (DataSpace const& lhs,
                                        DataSpace const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
