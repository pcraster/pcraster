#ifndef INCLUDED_GEO_NEIGHBOURHOOD
#define INCLUDED_GEO_NEIGHBOURHOOD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_GEO_SIMPLERASTER
#include "geo_simpleraster.h"
#define INCLUDED_GEO_SIMPLERASTER
#endif



namespace geo {
  // Neighbourhood declarations.
}



namespace geo {



//! This class is for neighbourhood objects to be used in filter operations.
/*!
  A neighbourhood is a (small) raster with weighting factors. It can be used
  as the weights raster for filter operations (see geo::Filter). A weighting
  factor of 0 means the cell at that location doesn't contribute to the
  resulting value.

  Specialized neighbourhoods implement different neighbourhood
  types/forms/shapes, for example Moore and Von Neumann neighbourhoods.

  The radius of the neighbourhood is the radius of the the raster containing
  the weighting factors. The 'from radius' is the inner radius of the shape
  of the neighbourhood while the 'to radius' is the outer radius of the shape.

  The to- and from- radiusses are floating point values which have to be
  approximated by discrete cells by specialized classes (for example the
  Circular neighbourhood). The neighbourhood radius might not correspond to
  the shape radius.
*/
class Neighbourhood: public geo::SimpleRaster<double>
{

  friend class NeighbourhoodTest;

private:

  //! Radius of the neighbourhood.
  size_t           d_radius;

  //! Radius of the inner shape of a donut shaped neighbourhood.
  double           d_fromRadius;

  //! Radius of the outer shape of the neighbourhood.
  double           d_toRadius;

  //! Assignment operator. NOT IMPLEMENTED.
  Neighbourhood&   operator=           (const Neighbourhood& rhs);

protected:

                   Neighbourhood       (size_t radius);

                   Neighbourhood       (double toRadius);

                   Neighbourhood       (double fromRadius,
                                        double toRadius);

  double           fromRadius          () const;

  double           toRadius            () const;

  bool             hasDonutShape       () const;

  bool             isOutline           () const;

  double           sum                 () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Neighbourhood       (size_t radius,
                                        double* cells);

  virtual          ~Neighbourhood      ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  size_t           radius              () const;

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

void               randomCellLocations (std::vector<LinearLoc>& locations,
                                        size_t nrCells,
                                        RasterDim const& space,
                                        Neighbourhood const& neighbourhood,
                                        CellLoc const& cell);

template<typename T>
void               randomCellLocations (std::vector<LinearLoc>& locations,
                                        size_t nrCells,
                                        SimpleRaster<T> const& raster,
                                        Neighbourhood const& neighbourhood,
                                        CellLoc const& cell);

} // namespace geo

#endif
