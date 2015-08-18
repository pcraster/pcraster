#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

// Module headers.
#ifndef INCLUDED_BLOCK_VOXELATHEIGHT
#include "block_voxelatheight.h"
#define INCLUDED_BLOCK_VOXELATHEIGHT
#endif



namespace block {

template<typename ValueType>
static void profile(
         ValueType& result,
         std::vector<ValueType> const& data,
         discr::VoxelStack const& stack,
         REAL4 height)
{
  if(stack.baseElevation() > height) {
    pcr::setMV(result);
  }
  else {
    typename std::vector<REAL4>::const_iterator it = std::find_if(
           stack.begin(), stack.end(),
           VoxelAtHeight(stack.baseElevation(), height));

    if(it == stack.end()) {
      pcr::setMV(result);
    }
    else {
      size_t index = it - stack.begin();

      if(pcr::isMV(data[index])) {
        pcr::setMV(result);
      }
      else {
        result = data[index];
      }
    }
  }
}

template
void profile<UINT1>(
         UINT1&,
         std::vector<UINT1> const&,
         discr::VoxelStack const&,
         REAL4);
template
void profile<INT4>(
         INT4&,
         std::vector<INT4> const&,
         discr::VoxelStack const&,
         REAL4);
template
void profile<REAL4>(
         REAL4&,
         std::vector<REAL4> const&,
         discr::VoxelStack const&,
         REAL4);



template<typename ValueType>
void profile(
         discr::RasterData<ValueType>& result,
         discr::BlockData<ValueType> const& data,
         REAL4 height)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  for(size_t i = 0; i < data.block()->nrCells(); ++i) {
    if(data.block()->cell(i).isMV()) {
      pcr::setMV(result.cell(i));
    }
    else {
      profile(result.cell(i), data.cell(i), data.block()->cell(i), height);
    }
  }
}

template
void profile<UINT1>(
         discr::RasterData<UINT1>&,
         discr::BlockData<UINT1> const&,
         REAL4);
template
void profile<INT4>(
         discr::RasterData<INT4>&,
         discr::BlockData<INT4> const&,
         REAL4);
template
void profile<REAL4>(
         discr::RasterData<REAL4>&,
         discr::BlockData<REAL4> const&,
         REAL4);

} // namespace block

