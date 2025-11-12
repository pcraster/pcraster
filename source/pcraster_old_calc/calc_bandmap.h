#ifndef INCLUDED_CALC_BANDMAP
#define INCLUDED_CALC_BANDMAP

#include "calc_gridmap.h"
#include"geo_bandmap.h"

#include <string>


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
    ~BandMap() override;

  bool getMinMax(double& min, double& max) const override;

  void readInBuffer(VS readAs, void *val) override;

  void writeData(const void *allValues) override;
};

}

#endif
