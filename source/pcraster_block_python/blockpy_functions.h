#ifndef INCLUDED_BLOCKPY_FUNCTIONS
#define INCLUDED_BLOCKPY_FUNCTIONS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRDATATYPE
#include "pcrdatatype.h"
#define INCLUDED_PCRDATATYPE
#endif

#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.


namespace geo {
  class RasterSpace;
}

namespace calc {
  class Field;
}

namespace discr {
  class Block;
  template<typename T>
    class BlockData;
  class Raster;
  template<typename T>
    class RasterData;
  class VoxelStack;
}



namespace blockpy {

template<PCR_VS valueScale>
struct ValueScaleTraits {};

template<>
struct ValueScaleTraits<VS_B>
{
  typedef UINT1 ValueType;
};

template<>
struct ValueScaleTraits<VS_L>
{
  typedef UINT1 ValueType;
};

template<>
struct ValueScaleTraits<VS_N>
{
  typedef INT4 ValueType;
};

template<>
struct ValueScaleTraits<VS_O>
{
  typedef INT4 ValueType;
};

template<>
struct ValueScaleTraits<VS_S>
{
  typedef REAL4 ValueType;
};

template<>
struct ValueScaleTraits<VS_D>
{
  typedef REAL4 ValueType;
};

//template<TypeId T>
// struct DefaultType {};
//
// template<>
// struct DefaultType<TI_UINT1> {
//    typedef UINT1 Type;
//    };
//

#ifdef WIN32
calc::Field*       booleanField        (discr::RasterData<UINT1> const*data);
calc::Field*       nominalField        (discr::RasterData<INT4> const*data);
calc::Field*       ordinalField        (discr::RasterData<INT4> const*data);
calc::Field*       scalarField         (discr::RasterData<REAL4> const*data);
calc::Field*       directionalField    (discr::RasterData<REAL4> const*data);
calc::Field*       lddField            (discr::RasterData<UINT1> const*data);
#else
// How it should work:
template<PCR_VS valueScale>
calc::Field*       field               (
         discr::RasterData<typename ValueScaleTraits<valueScale>::ValueType> const* data);
#endif

discr::Raster*     createRaster        (geo::RasterSpace const& space);

// discr::Raster*     createRaster        (size_t nrRows,
//                                         size_t nrCols,
//                                         double cellSize,
//                                         double west,
//                                         double north);

template<typename T>
discr::RasterData<T>* rasterData       (calc::Field const* field,
                                        discr::Raster* raster);

template<typename T>
discr::RasterData<T>* createRasterData (discr::Raster* raster,
                                        T defaultValue);

template<typename T>
discr::BlockData<T>* createBlockData   (discr::Block* block,
                                        T defaultValue);

discr::RasterData<REAL4>* baseElevation(discr::Block const& block);

discr::RasterData<REAL4>* surfaceElevation(
                                        discr::Block const& block);

discr::VoxelStack* voxelStack          (discr::Block const& block,
                                        size_t row,
                                        size_t col);

template<typename T>
std::vector<T>*    voxelStackData      (discr::BlockData<T> const& data,
                                        size_t row,
                                        size_t col);

template<typename T>
discr::BlockData<UINT1>* equals        (discr::BlockData<T> const& lhs,
                                        T rhs);

discr::Block*      resample            (discr::Block const& block,
                                        REAL4 thickness);

template<typename T>
discr::BlockData<T>* resample          (discr::BlockData<T> const& data,
                                        discr::Block* block);

template<typename T>
discr::RasterData<T>* profile          (discr::BlockData<T> const& data,
                                        REAL4 height);

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
template<typename T>                                                           \
discr::BlockData<REAL4>* name          (discr::BlockData<REAL4> const& lhs,    \
                                        T rhs);                                \
                                                                               \
template<typename T>                                                           \
discr::BlockData<REAL4>* name          (discr::BlockData<REAL4> const& lhs,    \
                                        discr::BlockData<T> const& rhs);

PCR_OPERATOR_TEMPLATES(add)
PCR_OPERATOR_TEMPLATES(substract)
PCR_OPERATOR_TEMPLATES(multiply)
PCR_OPERATOR_TEMPLATES(divide)

template<typename DestinationType, typename SourceType>
discr::BlockData<DestinationType>* cast(discr::BlockData<SourceType> const& rhs);

} // namespace blockpy

#endif
