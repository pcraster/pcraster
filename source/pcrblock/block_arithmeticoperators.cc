#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif



namespace block {

//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Array of first arguments.
  \param     rhs Second argument.
  \param     size Length of \a lhs.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         REAL4* lhs,
         T rhs,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    if(!pcr::isMV(rhs)) {
      if(!pcr::isMV(lhs[i])) {
        op(lhs[i], rhs);
      }
    }
  }
}



//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Array of first arguments.
  \param     rhs Array of second arguments.
  \param     size Length of \a lhs and \a rhs.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         REAL4* lhs,
         T const* rhs,
         size_t size)
{
  for(size_t i = 0; i < size; ++i) {
    if(!pcr::isMV(lhs[i])) {
      if(pcr::isMV(rhs[i])) {
        pcr::setMV(lhs[i]);
      }
      else {
        op(lhs[i], rhs[i]);
      }
    }
  }
}



//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Vector of first arguments.
  \param     rhs Second argument.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         std::vector<REAL4>& lhs,
         T rhs)
{
  arithmeticOperator(op, &(*lhs.begin()), rhs, lhs.size());
}



//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Vector of first arguments.
  \param     rhs Vector of second arguments.
  \warning   The length of \a lhs must be equal to the lengt of \a rhs.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         std::vector<REAL4>& lhs,
         std::vector<T> const& rhs)
{
  DEVELOP_PRECOND(lhs.size() == rhs.size());
  arithmeticOperator(op, &(*lhs.begin()), &(*rhs.begin()), lhs.size());
}



//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Data to use as first argument.
  \param     rhs Second argument.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         discr::BlockData<REAL4>& lhs,
         T rhs)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);

  discr::Block const& block(*lhs.block());

  for(size_t i = 0; i < block.nrCells(); ++i) {
    arithmeticOperator(op, lhs.cell(i), rhs);
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);
}



//! Applies \a op to \a lhs and \a rhs.
/*!
  \param     op Binary operator to apply.
  \param     lhs Data to use as first argument.
  \param     rhs Data to use as second argument.

  The result of applying \a op is stored in \a lhs. \a op is not applied when
  \a lhs and/or \a rhs contains a missing value.
*/
template<class Operator, typename T>
inline void arithmeticOperator(
         Operator& op,
         discr::BlockData<REAL4>& lhs,
         discr::BlockData<T> const& rhs)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(rhs);

  discr::Block const& block(*lhs.block());

  for(size_t i = 0; i < block.nrCells(); ++i) {
    arithmeticOperator(op, lhs.cell(i), rhs.cell(i));
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(lhs);
}



#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
template<typename T>                                                           \
inline void name(                                                              \
         discr::BlockData<REAL4>& lhs,                                         \
         discr::BlockData<T> const& rhs)                                       \
{                                                                              \
  void (*op)(REAL4&, T) = name;                                                \
  arithmeticOperator(op, lhs, rhs);                                            \
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
                                                                               \
template<typename T>                                                           \
inline void name(                                                              \
         discr::BlockData<REAL4>& lhs,                                         \
         T rhs)                                                                \
{                                                                              \
  void (*op)(REAL4&, T) = name;                                                \
  arithmeticOperator(op, lhs, rhs);                                            \
}                                                                              \
                                                                               \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         UINT1);                                                               \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         INT4);                                                                \
template void name(                                                            \
         discr::BlockData<REAL4>&,                                             \
         REAL4);                                                               \

// -----------------------------------------------------------------------------

template<typename T>
inline void add(REAL4& lhs, T rhs)
{ lhs += rhs; }

template<typename T>
inline void substract(REAL4& lhs, T rhs)
{ lhs -= rhs; }

template<typename T>
inline void divide(REAL4& lhs, T rhs)
{ lhs /= rhs; }

template<typename T>
inline void multiply(REAL4& lhs, T rhs)
{ lhs *= rhs; }

PCR_OPERATOR_TEMPLATES(add)
PCR_OPERATOR_TEMPLATES(substract)
PCR_OPERATOR_TEMPLATES(divide)
PCR_OPERATOR_TEMPLATES(multiply)

/*
*   **
* Calculates the nth power of the first expression, where n is the value on a second expression and sends it to the result, on a cell-by-cell basis.

* abs
* Calculates the absolute value of an expression, on a cell-by-cell basis.

* acos
* Calculates the inverse cosine value of an expression, on a cell-by-cell basis.

* asin
* Calculates the inverse sine value of an expression, on a cell-by-cell basis.

* atan
* Calculates the inverse tangent value of an expression, on a cell-by-cell basis.

* cos
* Calculates the cosine of an expression, on a cell-by-cell basis.

* exp
* Calculates the basee exponential of an expression, on a cell-by-cell basis.

* idiv
* Divides (integer division) the values on a first expression by the values on a second expression and assigns this quotient to the result, on a cell-by-cell basis.

* ln
* Calculates the natural logarithm (basee) exponential of an expression, on a cell-by-cell basis.

* log10
* Calculates the (basee) logarithm of an expression, on a cell-by-cell basis.

* mod
* Divides (integer division) the values on a first expression by the values on a second expression and assigns the remainder to the result, on a cell-by-cell basis.

* sin
* Calculates the sine of an expression, on a cell-by-cell basis.

* sqr
* Calculates the square of an expression, on a cell-by-cell basis.

* sqrt
* Calculates the square root of an expression, on a cell-by-cell basis.

* tan
* Calculates the tangent of an expression, on a cell-by-cell basis.
*/



} // namespace block

