#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#define INCLUDED_GEO_MOORENEIGHBOURHOOD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_NEIGHBOURHOOD
#include "geo_neighbourhood.h"
#define INCLUDED_GEO_NEIGHBOURHOOD
#endif



namespace geo {
  // MooreNeighbourhood declarations.
}



namespace geo {



//! Implemenation of the extended Moore neighbourhood.
/*!
  This class implements the extended Moore Neighbourhood: radius can be
  larger than 1.

  As an extension to the already extended Moore neighbourhood this class
  supports donut shaped neighbourhoods.

  \todo Rename to SquareNeighbourhood.
*/
class MooreNeighbourhood: public Neighbourhood
{

private:

  void             init                ();

  void             addRing             (size_t radius);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   MooreNeighbourhood  (double toRadius);

                   MooreNeighbourhood  (double fromRadius,
                                        double toRadius);

  /* virtual */    ~MooreNeighbourhood ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};

//! Untill this class is renamed this typedef.
typedef MooreNeighbourhood SquareNeighbourhood;



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
