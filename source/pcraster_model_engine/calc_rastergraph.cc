#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RASTERGRAPH
#include "calc_rastergraph.h"
#define INCLUDED_CALC_RASTERGRAPH
#endif

// Library headers.
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_VFIELD
#include "calc_vfield.h"
#define INCLUDED_CALC_VFIELD
#endif


/*!
  \file
  This file contains the implementation of the RasterGraph class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class RasterGraphPrivate
{
public:

  RasterGraphPrivate()
  {
  }

  ~RasterGraphPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERGRAPH MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERGRAPH MEMBERS
//------------------------------------------------------------------------------

calc::RasterGraph::RasterGraph(size_t nrVertices):
   d_nrVertices(nrVertices)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::RasterGraph::RasterGraph(RasterGraph const& rhs)

  : Base(rhs)

{
}
*/



calc::RasterGraph::~RasterGraph()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::RasterGraph& calc::RasterGraph::operator=(RasterGraph const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! equals nr of distinct FieldId's
size_t calc::RasterGraph::nrVertices() const
{
  return d_nrVertices;
}

//! unset bits in mvField that are MV in the graph
/*!
 * as a result \a mvField will only bits set for "new" MV's
 * not already known in d_mv
 */
void calc::RasterGraph::unsetMVField(BitField& mvField) const
{
  PRECOND(mvField.size()==nrVertices());
  for(size_t i=0;i<d_mv.size(); ++i)
    mvField[d_mv[i]]=0;
}

/*!
 * \brief initialize \a f to \a value inside graph and to MV outside
 * \todo
 *   if d_mv is runLength (as in MaskPacking) this can go much faster
 */
template<typename D,typename S>
void calc::RasterGraph::initField(D *f, const S& value) const
{
  for(size_t i=0;i<d_nrVertices; ++i)
    f[i]=(D)value;
  for(size_t i=0;i<d_mv.size(); ++i)
    pcr::setMV(f[d_mv[i]]);
}

/*!
 * \brief initialize \a dest to \a src value's inside graph and to MV outside
 * The templates instances created are those needed, allowing float <-> double
 * conversions but no others with loss of significance
 */
template<typename D, typename S>
void calc::RasterGraph::copyField(D *dest, const S* src) const
{
  for(size_t i=0;i<d_nrVertices; ++i)
    dest[i]=(D)src[i];
  for(size_t i=0;i<d_mv.size(); ++i)
    pcr::setMV(dest[d_mv[i]]);
}

/*
 * \brief initialize \a dest to \a src value's inside graph and to MV outside
 * The templates instance created are those needed.
 */
template<typename D, typename S>
void calc::RasterGraph::copyField(D *dest, const VField<S>& src) const
{
  if (src.spatial())
    copyField(dest,&(src[0]));
  else
    initField(dest,  src[0] );
}

namespace calc {
template void RasterGraph::initField<>(float *f, const float& value) const;
template void RasterGraph::initField<>(float *f, const double& value) const;
template void RasterGraph::initField<>(INT4  *f, const INT4& value) const;
template void RasterGraph::initField<>(UINT1 *f, const UINT1& value) const;
template void RasterGraph::copyField<>(float *d, const float *s) const;
template void RasterGraph::copyField<>(float *d, const double*s) const;
template void RasterGraph::copyField<>(double *d, const float*s) const;
template void RasterGraph::copyField<>(INT4  *d, const INT4  *s) const;
template void RasterGraph::copyField<>(UINT1 *d, const UINT1 *s) const;
template void RasterGraph::copyField<>(float *d, const VField<float>&s) const;
template void RasterGraph::copyField<>(double *d, const VField<float>&s) const;
template void RasterGraph::copyField<>(INT4  *d, const VField<INT4 >&s) const;
template void RasterGraph::copyField<>(UINT1 *d, const VField<UINT1>&s) const;
}

const calc::IFieldRDConversion&
  calc::RasterGraph::iFieldRDConversion() const
{
  PRECOND(d_frc);
  return *d_frc;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



