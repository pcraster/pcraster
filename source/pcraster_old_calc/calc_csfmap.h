#ifndef INCLUDED_CALC_CSFMAP
#define INCLUDED_CALC_CSFMAP

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include"geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

namespace calc {

//! csf input map
class CsfMap : public GridMap {
 private:
  geo::CSFMap d_map;
 public:
  // CONSTRUCTORS

  //! open map, PRECOND:file must exist
  CsfMap(const std::string& fileName); // throw (geo::NotA_PCRasterMap);

  //! create a map
  CsfMap(const std::string&   fileName,
    const geo::RasterSpace& rs, VS vs);

  //! close map
    ~CsfMap();

  bool getMinMax(double& min, double& max) const;

  void readInBuffer(VS readAs, void *val);

  void writeData(const void *allValues);

  geo::RasterSpace rasterSpace         () const;
};

}

#endif
