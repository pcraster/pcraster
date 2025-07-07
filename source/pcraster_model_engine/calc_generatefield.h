#ifndef INCLUDED_CALC_GENERATEFIELD
#define INCLUDED_CALC_GENERATEFIELD

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

namespace geo {
  class RasterSpace;
}

namespace calc {

class  SpatialPacking;
class  Field;

class GenerateSpatial {

  //! boolean mask if  true (1), generate value
  const UINT1 *d_mask;

  //! nr of cells in mask
  size_t d_nrMask;

  //! needed for index translation
  const  SpatialPacking&       d_spatialpacking;

  //! needed for index translation
  const  geo::RasterSpace&     d_rasterSpace;

  UINT1 maskAt(size_t pos) const;
  bool  maskTrue(size_t pos) const;

public:
  GenerateSpatial(
    const Field            &mask,
    const SpatialPacking   &c,
    const geo::RasterSpace &rs);

  GenerateSpatial(const GenerateSpatial& other) = delete;

  GenerateSpatial& operator=(const GenerateSpatial& other) = delete;

  ~GenerateSpatial();

  void xcoordinate(REAL4 *res) const;
  void ycoordinate(REAL4 *res) const;
  void uniqueid   (REAL4 *res) const;
  void uniform    (REAL4 *res) const;
  void normal     (REAL4 *res) const;

};

class GenerateNonSpatial {
  //! needed for index translation
  const  geo::RasterSpace&     d_rasterSpace;
 public:
   GenerateNonSpatial(const geo::RasterSpace &rs);
   GenerateNonSpatial(const GenerateNonSpatial& other) = delete;
   GenerateNonSpatial& operator=(const GenerateNonSpatial& other) = delete;
  ~GenerateNonSpatial();

  void celllength (REAL4 *res) const;
  void cellarea   (REAL4 *res) const;
  void mapuniform (REAL4 *res) const;
  void mapnormal  (REAL4 *res) const;

};

}
#endif
