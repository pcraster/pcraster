#ifndef INCLUDED_CALC_GENERATESPATIALFUNC
#define INCLUDED_CALC_GENERATESPATIALFUNC

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

namespace calc {
}

namespace calc {

class  Compressor;

class GenerateSpatialFunc {

  //! boolean mask if  true (1), generate value 
  const UINT1 *d_mask;

  //! nr of cells in mask 
  size_t d_nrMask;

  //! needed for index translation
  const  Compressor&       d_compressor;

  UINT1 maskAt(size_t pos) const;

public:
  GenerateSpatialFunc(
    const UINT1 *mask,
    size_t nrMask,
    const Compressor &c);

  void xcoordinate(REAL4 *res) const;
  void ycoordinate(REAL4 *res) const;
  void uniqueid(REAL4 *res) const;
};

}

#endif
