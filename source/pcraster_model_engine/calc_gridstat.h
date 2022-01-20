#ifndef INCLUDED_CALC_GRIDSTAT
#define INCLUDED_CALC_GRIDSTAT



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace calc {
  // GridStat declarations.
}



namespace calc {



//! info on a written map
struct GridStat
{

private:

  //  Assignment operator. DEFAULT
  // GridStat&           operator=           (const GridStat& rhs);

  //  Copy constructor. DEFAULT
  //               GridStat               (const GridStat& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GridStat               ();

     virtual       ~GridStat              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool   d_minMaxSet{false};
  double d_min{0},d_max{0};

  void   merge(const GridStat& s);

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
