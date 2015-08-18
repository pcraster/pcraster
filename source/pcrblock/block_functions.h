#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

#ifndef INCLUDED_BLOCK_COMPACTORS
#include "block_compactors.h"
#define INCLUDED_BLOCK_COMPACTORS
#endif

#ifndef INCLUDED_BLOCK_DEHAANCOMPACTOR
#include "block_dehaancompactor.h"
#define INCLUDED_BLOCK_DEHAANCOMPACTOR
#endif



namespace discr {
  class Block;
  template<typename T>
    class BlockData;
  template<typename T>
    class RasterData;
}



namespace block {

discr::Block*      create              (
                             discr::RasterData<REAL4> const* baseElevation);

template<typename T>
void               equals              (discr::BlockData<UINT1>& result,
                                        discr::BlockData<T> const& data,
                                        T value);

void               noCompactionAdd     (discr::Block& block,
                                        size_t nrVoxels,
                                        REAL4 thickness);

void               noCompactionAdd     (
                             discr::Block& block,
                             discr::RasterData<REAL4> const& thickness);

void               mackeyBridgeAdd     (
                             discr::Block& block,
                             discr::BlockData<REAL4>& originalThickness,
                             discr::BlockData<INT4>& sediment,
                             discr::RasterData<REAL4> const& thickness,
                             REAL4 maxVoxelThickness,
                             Compactors<MackeyBridgeCompactor> const& compactors);

void               deHaanAdd           (
                             discr::Block& block,
                             discr::BlockData<INT4>& sediment,
                             discr::BlockData<REAL4>& initialThickness,
                             discr::BlockData<REAL4>& cummulativeLoad,
                             discr::BlockData<REAL4>& cummulativeDuration,
                             discr::RasterData<REAL4> const& thickness,
                             Compactors<DeHaanCompactor> const& compactors);

void               remove              (
                                  discr::Block& block,
                                  discr::RasterData<REAL4> const& thickness);

// void               minimumElevation    (REAL4& result,
//                                         discr::Block const& block);

// void               maximumElevation    (REAL4& result,
//                                         discr::Block const& block);

void               baseElevation       (discr::RasterData<REAL4>& result,
                                        discr::Block const& block);

void               surfaceElevation    (discr::RasterData<REAL4>& result,
                                        discr::Block const& block);

template<typename ValueType>
void               profile             (discr::RasterData<ValueType>& result,
                                        discr::BlockData<ValueType> const& data,
                                        REAL4 height);

void               resample            (discr::Block& result,
                                        discr::Block const& block,
                                        REAL4 thickness);

template<typename ValueType>
void               resample            (
                             discr::BlockData<ValueType>& result,
                             discr::BlockData<ValueType> const& data);

discr::Block*      read                (std::string const& name);

template<typename ValueType>
discr::BlockData<ValueType>* read      (std::string const& name,
                                        discr::Block* block);

void               write               (discr::Block const& block,
                                        std::string const& name);

void               writeVTK            (discr::Block const& block,
                                        std::string const& name);

template<typename ValueType>
void               write               (discr::BlockData<ValueType> const& data,
                                        std::string const& name);

template<typename ValueType>
void               writeVTK            (discr::BlockData<ValueType> const& data,
                                        std::string const& name);

template<typename ValueType>
void               writeGSLIB          (discr::BlockData<ValueType> const& data,
                                        std::string const& name);

/*
template<typename ValueType>
void               write               (discr::BlockData<ValueType> const& data,
                                        REAL4 height,
                                        std::string const& name);
                                        */

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
template<typename T>                                                           \
void               name                (discr::BlockData<REAL4>& lhs,          \
                                        T rhs);                                \
                                                                               \
template<typename T>                                                           \
void               name                (discr::BlockData<REAL4>& lhs,          \
                                        discr::BlockData<T> const& rhs);

PCR_OPERATOR_TEMPLATES(add)
PCR_OPERATOR_TEMPLATES(substract)
PCR_OPERATOR_TEMPLATES(divide)
PCR_OPERATOR_TEMPLATES(multiply)

template<typename DestinationType, typename SourceType>
void               cast                (discr::BlockData<DestinationType>& lhs,
                                        discr::BlockData<SourceType> const& rhs);

template<typename T>
void               setDefaultValue     (discr::BlockData<T>& result,
                                        discr::RasterData<T> const& value);

} // namespace block
