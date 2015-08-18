#ifndef INCLUDED_GEO_UTIL
#define INCLUDED_GEO_UTIL



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_GEO_DEF
#include "geo_def.h"
#define INCLUDED_GEO_DEF
#endif

#ifndef INCLUDED_GEO_RASTER
#include "geo_raster.h"
#define INCLUDED_GEO_RASTER
#endif

#ifndef INCLUDED_GEO_RASTERBOUNDARIES
#include "geo_rasterboundaries.h"
#define INCLUDED_GEO_RASTERBOUNDARIES
#endif



namespace com {
  class PathName;
}

namespace geo {

  //! Checks if \a s contains a geo-eas formatted table.
  bool             geoEasFormat        (std::istream &s);

  bool             isCSFStack          (const com::PathName& pn);

  bool             isBlock             (const com::PathName& pn);

  bool             isTimeSeriesFile    (const com::PathName& pathName);

  bool             isColumnFile        (const com::PathName& pn);

  bool             isModelScriptFile   (const com::PathName& pathName);

  // geo::DataType    dataType            (const com::PathName& pathName);

  // bool             isSpatial           (DataType dataType);

  // bool             isTimeSeries        (DataType dataType);

  // bool             isModelScript       (DataType dataType);

  //! Converts a CSF projection type to a geo projection type.
  Projection       csfProjToGeo        (CSF_PT p);

  //! Converts a geo projection type to a CSF projection type.
  CSF_PT           geoProjToCsf        (Projection p);

  //! Converts projection code to string.
  std::string      projToStr           (Projection p);

  //! Converts projection string to code.
  Projection       strToProj           (const std::string &s);

  CSF_VS           strToValueScale     (const std::string &str);

  std::string      valueScaleToStr     (const CSF_VS& vs);

  // std::string      dataTypeToStr       (const DataType& dataType);

  //! get cell representation belongin to a value scale
  class ValueScale2CellRepr {
    private:
    CSF_VS d_vs;
  public:
      ValueScale2CellRepr(CSF_VS vs): d_vs(vs) {};
    CSF_CR defaultCR() const;
    CSF_CR smallCR()   const;
    CSF_CR largeCR()   const;
  };

/*
  template<class T>
  void             nonMVRowSection     (const geo::Raster<T>& raster,
                                        size_t r,
                                        size_t c,
                                        size_t& start,
                                        size_t& n);
*/

  template<class T>
  T                average             (const Raster<T>& raster,
                                        size_t r,
                                        size_t c,
                                        size_t l);

  template<class T>
  T                average             (T const* cells,
                                        geo::RasterDim const& dim,
                                        size_t row,
                                        size_t col,
                                        size_t length);

  template<class T>
  void             raster2Boundaries   (const SimpleRaster<T>& raster,
                                        RasterBoundaries<T>& boundaries);

  template<class T>
  void             magnitude           (const RasterBoundaries<T>& xVector,
                                        const RasterBoundaries<T>& yVector,
                                        RasterBoundaries<T>& result);

} // namespace geo

#endif
