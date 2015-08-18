#ifndef INCLUDED_DAL_DEF
#define INCLUDED_DAL_DEF



// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.



#if defined(_MSC_VER)
  #define IS_INFINITE(value) (_finite(value) == 0)
  #define IS_FINITE(value)   (_finite(value) != 0)
  // #define IS_NAN(value)      static_cast<bool>( _isnan (value))
#else
  #define IS_INFINITE(value)  std::isinf(value)
  #define IS_FINITE(value)   !std::isinf(value)
  // #define IS_NAN(value)       std::isnan(value)
#endif

#define IS_NAN(value) (value != value)



namespace dal {

//! Dataset types.
enum DatasetType {

  //! Spatial raster data. Each cell has the same type.
  RASTER,

  //! Spatial vector data.
  FEATURE,

  //! 3D spatial regular or irregular (ragged) block data.
  BLOCK,

  //! Tabular data. Each column has its own type.
  TABLE,

  //! Non spatial data matrix. Each cell has the same type.
  MATRIX,

  //! Image, drawing, graphics.
  GRAPHIC,

  //! Single value.
  CONSTANT,

  //! Vector data (direction, magnitude).
  VECTOR,

  //! Total number of dataset types.
  NR_DATASET_TYPES

};

/*
//! Projection for spatial datasets.
enum Projection {

  //! Y increases from north to south.
  YINCRN2S,

  //! Y increases from south to north.
  YINCRS2N,

  //! Total number of projections.
  NR_PROJECTIONS
};
*/

//! Value scales for values.
/*
enum ValueScale {
  BOOLEAN,
  ORDINAL,
  NOMINAL,
  SCALAR,
  DIRECTIONAL,
  LDD,
  NR_VALUE_SCALES
};
*/

/*
//! Dataset properties.
enum DatasetProperties {

  //! Dataset supports temporal data.
  TEMPORAL,

  //! Total number of dataset properties.
  NR_DATASET_PROPERTIES
};
*/

//! Id's for elementary types for individual dateset values.
/*!
 * all integer and float types map to typedef's in csftypes.h (e.g. TI_INT1 to INT1)
 */
enum TypeId {
  //! Signed 1 byte integer.
  TI_INT1,

  //! Signed 2 byte integer.
  TI_INT2,

  //! Signed 4 byte integer.
  TI_INT4,

  //! Unsigned 1 byte integer.
  TI_UINT1,

  //! Unsigned 2 byte integer.
  TI_UINT2,

  //! Unsigned 4 byte integer.
  TI_UINT4,

  //! Signed 4 byte floating point.
  TI_REAL4,

  //! Signed 8 byte floating point.
  TI_REAL8,

  //! String. maps to std::string
  TI_STRING,

  //! Vector of UINT1's.
  TI_UINT1_VECTOR,

  //! Vector of INT4's.
  TI_INT4_VECTOR,

  //! Vector of REAL4's.
  TI_REAL4_VECTOR,

  //! Number of type id's.
  TI_NR_TYPES
};



typedef std::vector<UINT1> UINT1_VECTOR;
typedef std::vector<INT4> INT4_VECTOR;
typedef std::vector<REAL4> REAL4_VECTOR;



//! Some files have headers, some don't, some sometimes.
enum HeaderType {

  //! Read header if it is present.
  AUTO_HEADER,

  //! Read header.
  HEADER,

  //! Do no support a header.
  NO_HEADER
};

//! Type of coordinates.
enum CoordinateType {

  //! Dimension has reference to some numeric range.
  NumericalCoordinates,

  //! Dimension has reference to some textual range.
  TextualCoordinates,

  //! Invalid.
  NrCoordinateTypes
};

//! High level meaning of dimension.
enum Meaning {

  //! Model scenarios.
  Scenarios,

  //! Set of distribution values.
  CumulativeProbabilities,

  //! Possible solutions.
  Samples,

  //! Time.
  Time,

  //! Space.
  Space,

  //! Invalid / unknown meaning.
  NrMeanings
};

enum DiscretisationType {

  //! Dimension is defined at locations at regular intervals. Dimension is 'binned'.
  RegularDiscretisation,

  //! Dimension is defined at specific (possibly, but not probably, all) locations along the dimension.
  ExactDiscretisation,

  //! Dimension is defined at all coordinates within and including two limits.
  BorderedDiscretisation,

  //! Discretisation type is not relevant, invalid.
  NrDiscretisationTypes
};

enum FilenameConvention {

  //! Use PCRaster convention for naming dynamic stacks (name0000.001, etc).
  PCRConvention=0x00000001,

  //! Use the DAL convention for naming datasets.
  DALConvention=0x00000002,

  //! Unknown file naming convention.
  UnknownFilenameConvention=0

};

typedef size_t FilenameConventions;



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
enum DriverProperty {

  //! Reads and writes discretisation info and attribute values in one go.
  CombinesDiscretisationAndData = 0x00000001,

  //! Supports reading a dataset.
  Reader = 0x00000002,

  //! Support writing a dataset.
  Writer = 0x00000004,

  //! Support deleting a dataset.
  Deleter = 0x00000008

  // Supports updating a dataset.
  // Updater = ,

};

typedef size_t DriverProperties;

//! Properties to tune the data sources.
/*!
*/
enum DataSourceProperty {
  //! Write data that is read to the cache for later reference.
  CacheDataOnRead = 0x00000001,

  //! Write data that is written to the cache for later reference.
  CacheDataOnWrite = 0x00000002
};

typedef size_t DataSourceProperties;

enum MissingDataStrategy {
  SetToMissingValue,
  Interpolate,
  UsePrevious,
  UseNext,
  UseNearest
};

//! Determines how to search through the data space for data items
/*!
*/
enum SearchMethod {
  //! Depth-first. Start with the data space and continue the search in sub-spaces with dimensions removed, until data items are found.
  NarrowSpaceWhenNeeded,

  //! Only search the data spacefor data items.
  SearchThisSpaceOnly
};

//! Determines when the search for data items should stop.
/*!
*/
enum SearchHaltCondition {
  //! Stop when the first data item is found.
  HaltOnFirstItemFound,

  //! Stop when all data items are found.
  SearchForAllItems
};

} // namespace dal

#endif
