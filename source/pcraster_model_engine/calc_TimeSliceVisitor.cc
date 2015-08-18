#ifndef INCLUDED_CALC_TIMESLICEVISITOR
#include "calc_TimeSliceVisitor.h"
#define INCLUDED_CALC_TIMESLICEVISITOR
#endif

// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_CALC_DOMAINERROR
#include "calc_domainerror.h"
#define INCLUDED_CALC_DOMAINERROR
#endif



/*!
  \file
  This file contains the implementation of the TimeSliceVisitor class.
*/

namespace {

} // Anonymous namespace



namespace calc {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMESLICEVISITOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMESLICEVISITOR MEMBERS
//------------------------------------------------------------------------------

TimeSliceVisitor::TimeSliceVisitor(
      LddGraph const& lg,
      VField<INT4> const&  nrTimeSlices,
      Field        const&  timestepInSecs):
     DownstreamVisitor(lg),
     d_nrTimeSlicesField(nrTimeSlices)
{
  PRECOND(!timestepInSecs.isSpatial());
  d_timestepInSecs= timestepInSecs.src_f()[0];
  if (d_timestepInSecs <= 0)
    throw DomainError("timestepInSecs must be > 0");
}


TimeSliceVisitor::~TimeSliceVisitor()
{
}

void TimeSliceVisitor::visitPerCachmentSlice()
{
    CurrentSliceInfo csi;
    csi.timestepInSecs = d_timestepInSecs;

    for(LddGraph::Catchments::const_iterator c= graph().catchmentsBegin();
        c != graph().catchmentsEnd(); ++c)
    {
      // set the per pit timeslice
      size_t fieldId  =c->d_pitId;
      csi.nrTimeSlices =1; // keep 1 on no pit (unsound ldd)
      if (!graph().invalid(fieldId)) {
         csi.nrTimeSlices =d_nrTimeSlicesField[fieldId];
         if (csi.nrTimeSlices == MV_INT4 || csi.nrTimeSlices <  1)
             csi.nrTimeSlices=1; // supress domain error
      }
      csi.sliceInSecs = d_timestepInSecs/(double)csi.nrTimeSlices;
      initPerCatchmentSlice(csi);

      for(LddGraph::DownConstIterator i=c->downBegin(); i != c->downEnd(); ++i)
          finishVertexBeforeAllSlices(i->up());
      finishVertexBeforeAllSlices(c->d_pitId);


      for(INT4 s=0;s < csi.nrTimeSlices; ++s) {
        // std::cerr << "CW Iteration " << s << std::endl;
        csi.currentTimeSlice=s;
        initPerCatchmentSlice(csi);
        visitCatchmentOfPit(c);
      }
     for(LddGraph::DownConstIterator i=c->downBegin(); i != c->downEnd(); ++i) {
       finishVertexAfterAllSlices(i->up());
     }
     finishVertexAfterAllSlices(c->d_pitId);
    }
}

//! visit catchment in downstream order and call initVertexBeforeSlice() upfront for each vertex
void calc::TimeSliceVisitor::visitCatchmentOfPit(
    LddGraph::Catchments::const_iterator c)
{
  POSTCOND(c < graph().catchmentsEnd());

  for(LddGraph::DownConstIterator i=c->downBegin();
         i != c->downEnd(); ++i) {
       initVertexBeforeSlice(i->up());
  }
  initVertexBeforeSlice(c->d_pitId);

  visitCatchment(*c);

  for(LddGraph::DownConstIterator i=c->downBegin();
         i != c->downEnd(); ++i) {
       finishVertexAfterSlice(i->up());
  }
  finishVertexAfterSlice(c->d_pitId);
}

void TimeSliceVisitor::initPerCatchmentSlice(CurrentSliceInfo const& )
{
}

//! called in visitCatchmentOfPit
void calc::TimeSliceVisitor::initVertexBeforeSlice(size_t /* v */)
{
}

//! called in visitCatchmentOfPit
void calc::TimeSliceVisitor::finishVertexAfterSlice(size_t /* v */)
{
}

void calc::TimeSliceVisitor::finishVertexAfterAllSlices(size_t /* v */)
{
}

void calc::TimeSliceVisitor::finishVertexBeforeAllSlices(size_t /* v */)
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

