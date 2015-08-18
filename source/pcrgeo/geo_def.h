#ifndef INCLUDED_GEO_DEF
#define INCLUDED_GEO_DEF

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// #ifndef INCLUDED_CSF
// #include "csf.h"
// #define INCLUDED_CSF
// #endif

namespace geo
{

  //! Projection types.
  enum Projection
  {
    IllegalProjection=0, /*!< not initialized, undefined */

    YIncrB2T=1,          /*!< see also CSF docs
                              <pre>
                               highest y value
                               ^
                               |___
                               |_|_|
                               |_|_|_
                               ^ lowest y value
                              </pre>
                        */
    YIncrT2B=2           /*!< specified in CSF docs
                              <pre>
                               lowest y value
                               v
                               |___
                               |_|_|
                               |_|_|_
                               v highest y value
                              </pre>
                        */
  };

  //! Quadrants.
  enum Quadrant
  {
    NorthWest,
    NorthEast,
    SouthEast,
    SouthWest
  };

// typedef UINT1                BoolType;
// typedef INT4                 NomType;
// typedef INT4                 OrdType;
// typedef REAL4                ScalType;
// typedef REAL4                DirectType;
// typedef UINT1                LddType;

// typedef UINT1                Boolean;
// typedef INT4                 Nominal;
// typedef INT4                 Ordinal;
// typedef REAL4                Scalar;
// typedef REAL4                Directional;
// typedef UINT1                Ldd;

// typedef enum DataType { DT_INVALID, STACK, BLOCK, TIMESERIES, POINTS, MODELSCRIPT } DataType;

} // namespace geo

#endif

