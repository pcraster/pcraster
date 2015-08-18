#ifndef INCLUDED_CALC_BANDMAP
#define INCLUDED_CALC_BANDMAP

#ifndef INCLUDED_CALC_GRIDMAP
#include "calc_gridmap.h"
#define INCLUDED_CALC_GRIDMAP
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_GEO_BANDMAP
#include"geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif

namespace calc {

//! Band input map
class BandMap : public GridMap {
 private:
  geo::BandMap d_map;
 public:
  // CONSTRUCTORS

  //! open map, PRECOND:file must exist
  BandMap(const std::string& fileName);

  //! create a map
  BandMap(const std::string&   fileName,
    const geo::RasterSpace& rs, VS vs);

  //! close map
    ~BandMap();

  bool getMinMax(double& min, double& max) const;

  void readInBuffer(VS readAs, void *val);

  void writeData(const void *allValues);
};

}

#endif
