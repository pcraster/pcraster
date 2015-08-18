#ifndef INCLUDED_DAL_RESAMPLE
#define INCLUDED_DAL_RESAMPLE



// External headers.
#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif



namespace dal {
namespace resample {

enum CellSelectionType {
  //! Whenever a source cell lies within the destination cell.
  WhateverContribution,

  //! When the source cell contributes most to the destination cell.
  LargestContribution
};

template<typename T>
class Average
{
public:

  static CellSelectionType const cellSelectionType=WhateverContribution;

  void             init                (size_t size);

  void             operator+=          (T argument);

  T                operator()          ();
};

template<typename T,
         class Operation,
         bool proportionalSourceCells>
class Aggregator
{
private:

public:

  void             aggregate           (Raster& result,
                                        Raster const& argument,
                                        RasterDimensions const& argumentArea,
                                        RasterDimensions const& resultArea);

};

template<typename T,
         class Operation>
class Aggregator<T, Operation, WhateverContribution>
{
private:

public:

  void aggregate(
         Raster& result,
         Raster const& argument,
         RasterDimensions const& argumentArea,
         RasterDimensions const& resultArea)
  {
    // Iterate over resultArea while selecting cells from argumentArea.
    // Use operation to calculate values based on cells from argument raster.

    // For each destination cell, select contributing source cells and
    // percentage or the area of the cell that contributes.

    // size_t sourceRow = size_t(0);
    // size_t sourceCol = size_t(0);

    // size_t destinationRow = size_t(0);
    // size_t destinationCol = size_t(0);


  }
};

template<typename T,
         class Operation>
class Aggregator<T, Operation, LargestContribution>
{
private:

public:

  void aggregate(
         Raster& result,
         Raster const& argument,
         RasterDimensions const& argumentArea,
         RasterDimensions const& resultArea)
  {
    // Iterate over resultArea while selecting cells from argumentArea.
    // Use operation to calculate values based on cells from argument raster.

    // For each destination cell, select contributing source cells. Select
    // cells based on contributing area.


  }
};

#define AGGREGATE_CASE(type)                                                   \
Aggregator<type, Operation, Operation::cellSelectionType>().aggregate(result,  \
         argument, sourceOverlap, destinationOverlap);                         \
break;

template<template<typename, class, CellSelectionType> class Aggregator,
         class Operation>
class Resampler
{
private:

public:

  void calculate(
         Raster& result,
         Raster const& argument)
  {
    assert(result.typeId() == argument.typeId());
    assert(result.typeId() == TI_REAL4 || result.typeId() == TI_REAL8);

    // Initialize result.
    result.setAllMV();

    // Handle overlapping cells.
    RasterDimensions sourceOverlap, destinationOverlap;
    boost::tie(sourceOverlap, destinationOverlap) =
         RasterDimensions::overlap(argument.dimensions(), result.dimensions());

    switch(result.typeId()) {
      case TI_REAL4: {
        AGGREGATE_CASE(REAL4)
      }
      case TI_REAL8: {
        AGGREGATE_CASE(REAL8)
      }
      default: {
        assert(false);
      }
    }
  }
};

#undef AGGREGATE_CASE

} // namespace resample
} // namespace dal

#endif
