#ifndef INCLUDED_GEO_MOORENEIGHBOURHOOD
#define INCLUDED_GEO_MOORENEIGHBOURHOOD

#include "stddefx.h"
#include "geo_neighbourhood.h"



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

  /* virtual */    ~MooreNeighbourhood () override;

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
