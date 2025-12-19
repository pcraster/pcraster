#ifndef INCLUDED_CALC_SCOPEDLDDGRAPH
#define INCLUDED_CALC_SCOPEDLDDGRAPH

#include "stddefx.h"
#include "calc_lddgraph.h"
#include "calc_scopedcachedobject.h"



namespace calc {
  // ScopedLddGraph declarations.
}



namespace calc {

//! fetch ldd from rte and get/compute its graph
class ScopedLddGraph
{

private:
  ScopedCachedObject<LddGraph> d_lg;
  LddGraph*                    d_modifiedLg{nullptr};

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
