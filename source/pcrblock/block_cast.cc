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



/*!
  \file
  This file contains the implementation of the Cast class.
*/



namespace block {

template<class Operator, typename T>
inline void unaryOperator(
         Operator& op,
         REAL4* lhs,
         T const* rhs,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    if(pcr::isMV(rhs[i])) {
      pcr::setMV(lhs[i]);
    }
    else {
      op(lhs[i], rhs[i]);
    }
  }
}



template<class Operator, typename T>
inline void unaryOperator(
         Operator& op,
         std::vector<REAL4>& lhs,
         std::vector<T> const& rhs)
{
  DEVELOP_PRECOND(lhs.size() == rhs.size());
  unaryOperator(op, &(*lhs.begin()), &(*rhs.begin()), lhs.size());
}



template<class Operator, typename DestinationType, typename SourceType>
inline void unaryOperator(
         Operator& op,
         discr::BlockData<DestinationType>& lhs,
         discr::BlockData<SourceType> const& rhs)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(rhs);

  discr::Block const& block(*lhs.block());

  for(size_t i = 0; i < block.nrCells(); ++i) {
    unaryOperator(op, lhs.cell(i), rhs.cell(i));
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);
}



#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
template<typename DestinationType, typename SourceType>                        \
inline void name(                                                              \
         discr::BlockData<DestinationType>& lhs,                               \
         discr::BlockData<SourceType> const& rhs)                              \
{                                                                              \
  void (*op)(DestinationType&, SourceType) = name;                             \
  unaryOperator(op, lhs, rhs);                                                 \
}                                                                              \
                                                                               \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         discr::BlockData<UINT1> const&);                                      \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         discr::BlockData<INT4> const&);                                       \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         discr::BlockData<REAL4> const&);                                      \

template<typename DestinationType, typename SourceType>
inline void cast(DestinationType& lhs, SourceType rhs)
{ lhs = DestinationType(rhs); }

PCR_OPERATOR_TEMPLATES(cast)

} // namespace block

