#ifndef INCLUDED_GEO_CIRCULARNEIGHBOURHOOD
#define INCLUDED_GEO_CIRCULARNEIGHBOURHOOD



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
  // CircularNeighbourhood declarations.
}



namespace geo {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class CircularNeighbourhood: public Neighbourhood
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  CircularNeighbourhood& operator=     (const CircularNeighbourhood& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   CircularNeighbourhood(const CircularNeighbourhood& rhs);

  void             init                ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   CircularNeighbourhood(
                                        double toRadius);

                   CircularNeighbourhood(
                                        double fromRadius,
                                        double toRadius);

  /* virtual */    ~CircularNeighbourhood();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace geo

#endif
