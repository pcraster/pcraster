#ifndef INCLUDED_CALC_STACKCR
#define INCLUDED_CALC_STACKCR

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif


namespace calc {

//! cell representation index
typedef enum CRIndex {
 CRI_1=0,   /* UINT1 */
 CRI_4=1,   /* INT4 */
 CRI_f=2,   /* REAL4 */
 CRI_X=4    /* UNKNOWN */
} CRIndex;

//! CSF_CR constant value from type deduced
template<typename CR>
 CSF_CR crCode();

template<>
 inline CSF_CR crCode<UINT1>() {
   return CR_UINT1;
 }
template<>
 inline CSF_CR crCode<INT4>() {
   return CR_INT4;
 }
template<>
 inline CSF_CR crCode<REAL4>() {
   return CR_REAL4;
 }

//! CRIndex constant value from type deduced
template<typename CR>
 CRIndex crIndex();

template<>
 inline CRIndex crIndex<UINT1>() {
   return CRI_1;
 }
template<>
 inline CRIndex crIndex<INT4>() {
   return CRI_4;
 }
template<>
 inline CRIndex crIndex<REAL4>() {
   return CRI_f;
 }

CSF_CR  cr(const CRIndex& cri);
size_t  size(const CRIndex& cri);
CRIndex allFitCRIndex(VS vsSet);

}
#endif
