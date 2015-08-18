#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_AREAOPERATIONS
#include "calc_areaoperations.h"
#define INCLUDED_CALC_AREAOPERATIONS
#endif

// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_MVGENERIC
#include "com_mvgeneric.h"
#define INCLUDED_COM_MVGENERIC
#endif
// Module headers.
#ifndef INCLUDED_CALC_AVERAGEMAP
#include "calc_averagemap.h"
#define INCLUDED_CALC_AVERAGEMAP
#endif


/*!
  \file
  This file contains the implementation of the AreaOperations class.
*/



//------------------------------------------------------------------------------


namespace calc {
namespace detail {
 } // namespace detail
} // namespace calc




//------------------------------------------------------------------------------
// DEFINITION OF STATIC AREAOPERATIONS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF AREAOPERATIONS MEMBERS
//------------------------------------------------------------------------------

calc::AreaOperations::AreaOperations()
{
}


calc::AreaOperations::~AreaOperations()
{
}

template<typename IDF>
void calc::AreaOperations::apply(
                    REAL4 *val,
              const IDF   *id,
                    size_t len)
{
  AverageMap am;
  am.apply(id,len,val,len);

  d_map=am.areaAverageMap();

  for(size_t i=0; i < len; ++i) {
    if (pcr::isMV(id[i]) || !d_map.count(id[i]))
      pcr::setMV(val[i]);
    else {
     assert(d_map.count(id[i]));
     val[i] = (REAL4)statistic(id[i]);
    }
  }
}

template void calc::AreaOperations::apply<UINT1>(
     REAL4 *val, const UINT1    *id,  size_t idLen);
template void  calc::AreaOperations::apply<INT4>(
     REAL4 *val, const INT4    *id,  size_t idLen);

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
