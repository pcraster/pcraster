#include "stddefx.h"
#include "discr_blockdata.h"
#include "discr_rasterdata.h"
#include "block_voxelatheight.h"



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
    auto it = std::find_if(
           stack.begin(), stack.end(),
           VoxelAtHeight(stack.baseElevation(), height));

    if(it == stack.end()) {
      pcr::setMV(result);
    }
    else {
      size_t const index = it - stack.begin();

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

