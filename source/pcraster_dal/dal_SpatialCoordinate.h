#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#define INCLUDED_DAL_SPATIALCOORDINATE



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif



namespace dal {
  // SpatialCoordinate declarations.
}



namespace dal {

//! Class for spatial addresses.
/*!
  Just a simple class for storing and x- and y-coordinate.

  \todo      Rename to SpatialAddress.
*/
class PCR_DAL_DECL SpatialCoordinate
{

  friend class SpatialCoordinateTest;
  friend PCR_DAL_DECL bool operator==(
         SpatialCoordinate const&, SpatialCoordinate const&);
  friend PCR_DAL_DECL bool operator!=(
         SpatialCoordinate const&, SpatialCoordinate const&);

private:

  //! X-coordinate.
  double           _x;

  //! Y-coordinate.
  double           _y;

  bool             equals              (SpatialCoordinate const& rhs) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SpatialCoordinate   ();

                   SpatialCoordinate   (double x,
                                        double y);

                   SpatialCoordinate   (SpatialCoordinate const& rhs);

  /* virtual */    ~SpatialCoordinate  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  SpatialCoordinate& operator=         (SpatialCoordinate const& rhs);

  void             setX                (double x);

  void             setY                (double y);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           x                   () const;

  double           y                   () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (SpatialCoordinate const& lhs,
                                        SpatialCoordinate const& rhs);

PCR_DAL_DECL bool  operator!=          (SpatialCoordinate const& lhs,
                                        SpatialCoordinate const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
