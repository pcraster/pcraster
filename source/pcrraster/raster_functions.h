#ifndef INCLUDED_RASTER_FUNCTIONS
#define INCLUDED_RASTER_FUNCTIONS

#include "stddefx.h"



namespace discr {
  class Raster;
}



namespace raster {

discr::Raster*     create              (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north);

template<typename ValueType>
void               setMV               (discr::RasterData<ValueType>& data,
                                        size_t index);

template<typename T>
void               writeBinary         (discr::RasterData<T> const& data,
                                        std::string const& name);

}


#endif
