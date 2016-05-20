#ifndef INCLUDED_CALC_TIMESLICEVISITOR
#define INCLUDED_CALC_TIMESLICEVISITOR



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_CALC_DOWNSTREAMVISITOR
#include "calc_downstreamvisitor.h"
#define INCLUDED_CALC_DOWNSTREAMVISITOR
#endif



namespace calc {
  // TimeSliceVisitor declarations.
}



namespace calc {

//! Visit each catchment a nr of nrTimeSlices times.
/*!
  visitEdge(), finishVertex(), initVertexBeforeSlice
  are called for each catchment for each timeslice.
*/
class TimeSliceVisitor: public DownstreamVisitor
{

private:

    VField<INT4> const&     d_nrTimeSlicesField;
    double                  d_timestepInSecs;

                   TimeSliceVisitor               ();
  void              visitCatchmentOfPit(
    LddGraph::Catchments::const_iterator c);

protected:
                  TimeSliceVisitor(LddGraph const& lg,
                                   VField<INT4> const&  nrTimeSlices,
                                   Field        const&  timestepInSecs);
  struct CurrentSliceInfo {
    INT4    nrTimeSlices;

    //! [0,nrTimeSlices)
    INT4    currentTimeSlice;

    //! equals nrTimeSlices * sliceInSecs
    double  timestepInSecs;

    //! equals  timestepInSecs / nrTimeSlices
    double  sliceInSecs;
  };

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~TimeSliceVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              visitPerCachmentSlice         ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  //! called once before all slice then each time before each slice in each catchment
  virtual void      initPerCatchmentSlice         (CurrentSliceInfo const& csi);

  virtual void      initVertexBeforeSlice         (size_t /* v */);
  virtual void      finishVertexAfterSlice        (size_t /* v */);
  virtual void      finishVertexAfterAllSlices    (size_t /* v */);
  virtual void      finishVertexBeforeAllSlices   (size_t /* v */);

};

//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc

#endif
