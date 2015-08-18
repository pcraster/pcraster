#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif

#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#include "geo_circularneighbourhood.h"
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#endif

#ifndef INCLUDED_GEO_FILTERENGINE
#include "geo_filterengine.h"
#define INCLUDED_GEO_FILTERENGINE
#endif

#ifndef INCLUDED_GEO_FRACTIONFILTER
#include "geo_fractionfilter.h"
#define INCLUDED_GEO_FRACTIONFILTER
#endif

#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#include "geo_mooreneighbourhood.h"
#define INCLUDED_GEO_MOORENEIGHBOURHOOD
#endif

#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOOD
#include "geo_riksneighbourhood.h"
#define INCLUDED_GEO_RIKSNEIGHBOURHOOD
#endif



// namespace calc {

//! Calculates the fraction of value cells within a neighbourhood.
/*!
  \param     outputMap Output raster with fractions.
  \param     inputMap Input raster with cells to count.
  \param     innerRadiusMap Inner radius of the neighbourhood to use.
  \param     outerRadiusMap Output radius of neighbourhood to use.
  \return    0 in case of success, 1 otherwise.
  \warning   .
  \sa        .
*/
template<class Neighbourhood>
int fraction(
         MAP_REAL8* outputMap,
         MAP_UINT1 const* inputMap,
         MAP_REAL8 const* innerRadiusMap,
         MAP_REAL8 const* outerRadiusMap)
{
  PRECOND(outputMap->spatial);
  PRECOND(inputMap->spatial);
  PRECOND(!innerRadiusMap->spatial && !outerRadiusMap->spatial);

  int result = 0;
  int nrRows, nrCols;

  nrRows = outputMap->NrRows(outputMap);
  nrCols = outputMap->NrCols(outputMap);

  geo::SimpleRaster<UINT1> inputRaster(nrRows, nrCols);
  geo::SimpleRaster<REAL8> outputRaster(nrRows, nrCols);

  // Copy values from inputMap-arg to inputRaster-var
  for(int row = 0; row < nrRows; ++row) {
    for(int col = 0; col < nrCols; ++col) {
      if(!inputMap->Get(&inputRaster.cell(row, col), row, col, inputMap)) {
        inputRaster.setMV(row, col);
      }
    }
  }

  // Configure neighbourhood with filter weights.
  REAL8 innerRadius, outerRadius;
  innerRadiusMap->Get(&innerRadius, 0, 0, innerRadiusMap);
  outerRadiusMap->Get(&outerRadius, 0, 0, outerRadiusMap);
  Neighbourhood weights(innerRadius, outerRadius);

  // Create filter and filter engine.
  UINT1 value = 1;
  geo::FractionFilter<UINT1> filter(weights, value);
  geo::FilterEngine<UINT1, REAL8> engine(inputRaster, filter, outputRaster);

  try {
    engine.calc();

    // Copy values of outputRaster-var to outputMap-arg
    for(int row = 0; row < nrRows; ++row) {
      for(int col = 0; col < nrCols; ++col) {
        if(!outputRaster.isMV(row, col)) {
          outputMap->Put(outputRaster.cell(row, col), row, col, outputMap);
        }
        else {
          outputMap->PutMV(row, col, outputMap);
        }
      }
    }
  }
  catch(...) {
    result = 1;
  }

  return result;
}



extern "C" int squareFraction(
         MAP_REAL8* outputMap,
         MAP_UINT1 const* inputMap,
         MAP_REAL8 const* innerRadiusMap,
         MAP_REAL8 const* outerRadiusMap)
{
  return fraction<geo::SquareNeighbourhood>(outputMap, inputMap,
         innerRadiusMap, outerRadiusMap);
}



// int circularFraction(
//          MAP_REAL8* outputMap,
//          MAP_UINT1 const* inputMap,
//          MAP_REAL8 const* innerRadiusMap,
//          MAP_REAL8 const* outerRadiusMap)
// {
//   return fraction<geo::CircularNeighbourhood>(outputMap, inputMap,
//          innerRadiusMap, outerRadiusMap);
// }



extern "C" int riksFraction(
         MAP_REAL8* outputMap,
         MAP_UINT1 const* inputMap,
         MAP_REAL8 const* innerRadiusMap,
         MAP_REAL8 const* outerRadiusMap)
{
  return fraction<geo::RiksNeighbourhood>(outputMap, inputMap,
         innerRadiusMap, outerRadiusMap);
}

// } // namespace calc
