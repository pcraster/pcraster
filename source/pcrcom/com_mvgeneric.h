#ifndef INCLUDED_COM_MVGENERIC
#define INCLUDED_COM_MVGENERIC



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif


namespace com {
  // mvgeneric declarations.
}



namespace com {

/*! \file generic algorithms with MV handling

   \todo
     wildgroei dingen die iets met de specieke PCRaster/MV tests doen:
      com_binaryoperators.h
      com_functions.h
      com_csfcell.h

    en ook geo::SimpleRaster

    wrsl. beter om iets met typetraits/policies te doen, uitdaging maak
     generic algorithms die met MV's kunnen werken:
      templates met hard ingebakken MV-waarden zoals in PCRaster.
                met runtime bekende MV-waarden zoals BIL-formaat ESRI
                 zie GDAL.
     input alleen in termen van iterators (geo::SimpleRaster heeft al meer;
      de notie van een grid):
       generic algoritm types:
       - for_each/transform etc. die MV skippen
       - zoiets als minimum in com_functions.h die eerst een geldige waarde zoekt.
     begin in com_mvgeneric gemaakt.

    tricky issue is implicit cast zie:
    void com::MVGenericTest::testVisitNonMV()/ std::accumulate(d,d+3,0)
    en 
    void com::BinaryOperatorsTest::testMVCast()
 */


/*! as std::for_each, but skip missing value
 *  apply function object \a op to all non MV cells
 */
template<class InputIterator,class UnaryProc>
UnaryProc forEachNonMV(
         InputIterator begin,
         InputIterator end,
         UnaryProc     op)
{
  while(begin != end) {
    if(!pcr::isMV(*begin))
      op(*begin);
    ++begin;
  }
  return op;
}

//! in support of NonSpatialContainer
/*! looks most like an Input Iterator
 */
template<typename T>
 class NonSpatialIterator {
   T       d_value;
   size_t  d_pos;

   typedef NonSpatialIterator<T> NSI;
  public:
   // default Copy ctor
   NonSpatialIterator(
       const   T& value,
       size_t  pos):
    d_value(value),
    d_pos(pos)
   {
   }
   const T& operator*() const {
     return d_value;
   }
   NonSpatialIterator<T>
   operator++(int) {
     d_pos++;
     // postfix
     return NonSpatialIterator<T>(d_value,d_pos-1);
   }
   NonSpatialIterator<T>
   operator++() {
     // prefix
     d_pos++;
     return NonSpatialIterator<T>(d_value,d_pos);
   }
   bool operator==(const NSI& e2) const {
     return d_pos == e2.d_pos;
   }
   bool operator!=(const NSI& e2) const {
     return d_pos != e2.d_pos;
   }
 };

//! emulate an array of \a n elements having identical \a value
/*! this container can be used in template algorithms instead of an
 *  stl::container or a POD-C-array.
 *  see forEachNonMV2 for an example where both a POD-C-array or
 *  a ptr to a single value are handled.
 */
template<typename T>
 class NonSpatialContainer {
   T       d_value;
   size_t  d_n;
 public:
   typedef NonSpatialIterator<T> const_iterator;

   NonSpatialContainer(
       const   T& value,
       size_t  n=1):
     d_value(value),
     d_n(n)
     {};

   const_iterator begin() const {
     return const_iterator(d_value,0);
   }

   const_iterator end() const {
     return const_iterator(d_value,d_n);
   }
   const T& operator[](size_t ) {
     return d_value;
   }
 };


/*!
 * \todo
 *   specialize for ar1 or ar2 being non-spatial so the isMV test can
 *   be skipped.
 */
template<
     typename A1,
     typename A2,
     class    BinaryProc>
   /*!
    * \param ar1  having operator[]
    * \param ar2  having operator[]
    */
   BinaryProc forEachNonMV(
         A1 ar1,
         A2 ar2,
         size_t n,
         BinaryProc     op)
     {
       for(size_t i=0; i < n; ++i) {
         if (!(pcr::isMV(ar1[i])|pcr::isMV(ar2[i])))
           op(ar1[i],ar2[i]);
       }
       return op;
     }

//! first-cut non-spatial - spatial handler for a 2 input function
/*!
 * \todo
 *   NonSpatialContainer allows MV, pcrcalc does not allow NonSpatial MV's
 *   No distinction between NonSpatial and a Spatial with 1 cell.
 *   hence can not yet use it for the (calc) point operations.
 *
 * \todo
 *   tag op for being commutative, saves 1 forEachNonMV instantion
 */
template<typename T1,
         typename T2,
         typename BinaryOp>
  BinaryOp iterateNonMV2(
         const T1   *v1,
         size_t      len1,
         const T2   *v2,
         size_t      len2,
         BinaryOp    op)
{
  PRECOND(len1 > 0 && len2 > 0);
  // equal length or one of them "non-spatial"
  PRECOND(len1 == len2 || len1 == 1 || len2 == 1);

  if (len1 == len2)
    return forEachNonMV(v1,v2,len1,op);

  if (len1 == 1) {
    NonSpatialContainer<T1> nsc1(*v1);
    return forEachNonMV(nsc1,v2,len2,op);
  }
  POSTCOND(len2 == 1);
  NonSpatialContainer<T2> nsc2(*v2);
  return forEachNonMV(v1,nsc2,len1,op);
}

/*! \brief
 *   (may) alter each element of v1 with an
 *    BinaryOp op(T1 &v1Element, const T2& v2Element)
 *  \pre
 *    Since v1 is the destination it can not have a
 *    single element while v2 is a real array:
 *    <br> len1 == len2 || (len1 > 1 && len2 == 1)
 */
template<typename T1,
         typename T2,
         typename BinaryOp>
  BinaryOp forEachNonMV2(
         T1         *v1,
         size_t      len1,
         const T2   *v2,
         size_t      len2,
         BinaryOp    op)
{
  PRECOND(len1 > 0 && len2 > 0);
  // equal length or only v2 "non-spatial"
  PRECOND(len1 == len2 || (len1 > 1 && len2 == 1));

  if (len1 == len2)
    return forEachNonMV(v1,v2,len1,op);

  POSTCOND(len2 == 1);
  NonSpatialContainer<T2> nsc2(*v2);
  return forEachNonMV(v1,nsc2,len1,op);
}


//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
