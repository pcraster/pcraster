#ifndef INCLUDED_OLDCALC_CSFMAP
#define INCLUDED_OLDCALC_CSFMAP

#include "calc_gridmap.h"
#include "geo_csfmap.h"

#include <string>


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
    ~CsfMap() override;

  bool getMinMax(double& min, double& max) const override;

  void readInBuffer(VS readAs, void *val) override;

  void writeData(const void *allValues) override;

  geo::RasterSpace rasterSpace         () const;
};

}

#endif
