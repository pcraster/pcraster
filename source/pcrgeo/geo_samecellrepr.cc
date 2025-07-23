#include "stddefx.h"
#include "geo_samecellrepr.h"


/*!
  \file
  This file contains the implementation of the SameCellRepr class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC SAMECELLREPR MEMBERS
//------------------------------------------------------------------------------

// The SameCellRepr class is limited to instantiations for the common CSF cell
// representations.
namespace geo {
 template<> const CSF_CR SameCellRepr<REAL8>::cr(CR_REAL8);
 template<> const CSF_CR SameCellRepr<REAL4>::cr(CR_REAL4);
 template<> const CSF_CR SameCellRepr<UINT1>::cr(CR_UINT1);
 template<> const CSF_CR SameCellRepr<INT4>::cr(CR_INT4);
}


