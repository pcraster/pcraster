#ifndef INCLUDED_GEO_RIKSNEIGHBOURHOOD
#define INCLUDED_GEO_RIKSNEIGHBOURHOOD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_TUPLE_TUPLE
#include <boost/tuple/tuple.hpp>
#define INCLUDED_BOOST_TUPLE_TUPLE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_NEIGHBOURHOOD
#include "geo_neighbourhood.h"
#define INCLUDED_GEO_NEIGHBOURHOOD
#endif



namespace geo {
  // RiksNeighbourhood declarations.
}



namespace geo {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo Handle floating point radiusses.
*/
class RiksNeighbourhood: public Neighbourhood
{

  friend class RiksNeighbourhoodTest;

private:

  static boost::tuple<size_t, size_t> circleCell(double radius);

  void             init                ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RiksNeighbourhood   (double toRadius);

                   RiksNeighbourhood   (double fromRadius,
                                        double toRadius);

  /* virtual */    ~RiksNeighbourhood  ();

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
