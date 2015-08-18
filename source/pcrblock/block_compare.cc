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

// Module headers.



namespace block {

template<typename T>
static void equals(
         UINT1* result,
         T const* data,
         T value,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    if(pcr::isMV(data[i])) {
      pcr::setMV(result[i]);
    }
    else {
      result[i] = data[i] == value;
    }
  }
}



template<typename T>
static void equals(
         std::vector<UINT1>& result,
         std::vector<T> const& data,
         T value)
{
  PRECOND(result.size() == data.size());
  equals(&(*result.begin()), &(*data.begin()), value, result.size());
}



template<typename T>
void equals(
         discr::BlockData<UINT1>& result,
         discr::BlockData<T> const& data,
         T value)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(result);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  discr::Block const& block(*data.block());

  for(size_t i = 0; i < block.nrCells(); ++i) {
    equals(result.cell(i), data.cell(i), value);
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(result);
}



template
void equals<UINT1>(
         discr::BlockData<UINT1>&,
         discr::BlockData<UINT1> const&,
         UINT1);
template
void equals<INT4>(
         discr::BlockData<UINT1>&,
         discr::BlockData<INT4> const&,
         INT4);
template
void equals<REAL4>(
         discr::BlockData<UINT1>&,
         discr::BlockData<REAL4> const&,
         REAL4);

} // namespace block

