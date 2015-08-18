#ifndef INCLUDED_DAL_SPACEDIMENSIONS
#define INCLUDED_DAL_SPACEDIMENSIONS



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif



namespace dal {
  // SpaceDimensions declarations.
}



namespace dal {

//! Class for storing the general properties of a spatial area.
/*!
  Properties stored are the coordinates of the bounding rectangle around the
  area.
*/
class PCR_DAL_DECL SpaceDimensions
{

  friend class SpaceDimensionsTest;
  friend PCR_DAL_DECL bool operator==(
         SpaceDimensions const&, SpaceDimensions const&);
  friend PCR_DAL_DECL bool operator!=(
         SpaceDimensions const&, SpaceDimensions const&);

private:

  //! North-west coordinate.
  SpatialCoordinate _northWest;

  //! South-east coordinate.
  SpatialCoordinate _southEast;

  bool             equals              (SpaceDimensions const& rhs) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpaceDimensions     (double west=0.0,
                                        double north=0.0,
                                        double east=1.0,
                                        double south=-1.0);

                   SpaceDimensions     (SpatialCoordinate const& northWest,
                                        SpatialCoordinate const& southEast);

                   SpaceDimensions     (SpaceDimensions const& rhs);

  virtual          ~SpaceDimensions    ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  SpaceDimensions const& operator=     (SpaceDimensions const& rhs);

  SpaceDimensions const& operator|=    (SpaceDimensions const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           north               () const;

  double           west                () const;

  double           south               () const;

  double           east                () const;

  double           longitudinalExtent  () const;

  double           latitudinalExtent   () const;

  double           area                () const;

  bool             contains            (double x,
                                        double y) const;

  bool             contains            (SpatialCoordinate const& coordinate) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (SpaceDimensions const& lhs,
                                        SpaceDimensions const& rhs);

PCR_DAL_DECL bool  operator!=          (SpaceDimensions const& lhs,
                                        SpaceDimensions const& rhs);

PCR_DAL_DECL SpaceDimensions operator| (SpaceDimensions const& lhs,
                                        SpaceDimensions const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
