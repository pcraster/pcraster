#ifndef INCLUDED_CALC_SCOPEDLDDGRAPH
#define INCLUDED_CALC_SCOPEDLDDGRAPH



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_LDDGRAPH
#include "calc_lddgraph.h"
#define INCLUDED_CALC_LDDGRAPH
#endif
#ifndef INCLUDED_CALC_SCOPEDCACHEDOBJECT
#include "calc_scopedcachedobject.h"
#define INCLUDED_CALC_SCOPEDCACHEDOBJECT
#endif



namespace calc {
  // ScopedLddGraph declarations.
}



namespace calc {

//! fetch ldd from rte and get/compute its graph
class ScopedLddGraph
{

private:
  ScopedCachedObject<LddGraph> d_lg;
  LddGraph*                    d_modifiedLg;

  //! Assignment operator. NOT IMPLEMENTED.
  ScopedLddGraph&           operator=           (ScopedLddGraph const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ScopedLddGraph               (ScopedLddGraph const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                    ScopedLddGraph              (RunTimeEnv* rte,
                                                 const Field& ldd);

  /* virtual */    ~ScopedLddGraph              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void               setMVInput                 (BitField& mvAtInput);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const LddGraph& current() const;

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
