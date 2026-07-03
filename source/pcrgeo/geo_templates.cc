#include "csftypes.h"
#include "geo_csfraster.cc"
#include "geo_csfraster.h"
#include "geo_griddedpoints.cc"
#include "geo_griddedpoints.h"
#include "geo_point.h"
#include "geo_raster.cc"
#include "geo_raster.h"
#include "geo_voxelstack.h"

template class geo::Raster<geo::VoxelStack>;

template class geo::Raster<UINT1>;
template class geo::Raster<INT4>;
template class geo::Raster<REAL4>;
template class geo::Raster<REAL8>;
template class geo::CSFRaster<UINT1>;
template class geo::CSFRaster<INT4>;
template class geo::CSFRaster<REAL4>;
template class geo::CSFRaster<REAL8>;

template class geo::GriddedPoints<geo::Point<double, 2>>;
