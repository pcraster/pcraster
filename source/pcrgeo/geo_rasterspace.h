#ifndef INCLUDED_GEO_RASTERSPACE
#define INCLUDED_GEO_RASTERSPACE



#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_GEO_DEF
#include "geo_def.h"
#define INCLUDED_GEO_DEF
#endif

#ifndef INCLUDED_GEO_RASTERDIM
#include "geo_rasterdim.h"
#define INCLUDED_GEO_RASTERDIM
#endif



namespace dal {
  class Raster;
}
namespace geo {
  class CellLoc;
}



namespace geo {

//! Divides a rectangular space in a grid.
/*!
   Implements a rectangular raster divided in square cells, the raster is
   referenced in a projection.

   It implements the simple projection scheme of the PCRaster CSF version 2
   library; the projection only discerns in which direction the y-axis
   increases. Coordinate transformation code is copied from the Ansi C CSF
   library, please keep in sync.

   layout of data members sorted for alignment, first doubles then 4 byte
   quantities.
*/
class RasterSpace: public RasterDim
{
  // default copy and assignment constructor are ok

private:
  //! set up after initialization or alteration.
  void setup();

  //! Cellsize.
  double d_cellSize;

  //! Left coordinate of left of raster.
  double d_left;

  //! Top coordinate of raster.
  double d_top;

  //! Angle.
  /*! PCRaster CSF has an angle, most other formats do not. See CSF docs.
   */
  double d_angle;

  //! expensive derivatives.
  double d_angleCos;
  double d_angleSin;

  //! Projection, can never be IllegalProjection
  Projection d_projection;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Constructor.
                   RasterSpace         ();

  //! Constructor.
                   RasterSpace         (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize=1,
                                        double leftWest=0,
                                        double topNorth=0,
                                        Projection proj=YIncrB2T,
                                        double angle=0);

                   RasterSpace         (dal::Raster const& raster);

  //! Destructor.
  virtual          ~RasterSpace        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Sets the cellsize of the the raster to \a s.
  void             setCellSize         (double s);

  //! Sets the upper left coordinates of the raster.
  void             setUpperLeft        (double x,
                                        double y);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the projection of the raster.
  Projection       projection          () const;

  //! Returns the angle of the raster.
  double           angle               () const;

  //! Returns the cellsize of the raster.
  double           cellSize            () const;

  //! Returns the left coordinate of the raster.
  double           west                () const;

  //! Returns the right coordinate of the raster.
  double           east                () const;

  //! Returns the top coordinate of the raster.
  double           north               () const;

  //! Returns the bottom coordinate of the raster.
  double           south               () const;

  //! Returns the left coordinate of the raster. DEPRICATED
  double           left                () const;

  //! Returns the right coordinate of the raster. DEPRICATED
  double           right               () const;

  //! Returns the top coordinate of the raster. DEPRICATED
  double           top                 () const;

  //! Returns the bottom coordinate of the raster. DEPRICATED
  double           bottom              () const;

  //! Returns the back coordinate of the raster. DEPRICATED
  double           back                () const;

  //! Returns the front coordinate of the raster. DEPRICATED
  double           front               () const;

  //! Returns the width of the raster.
  double           width               () const;

  //! Returns the height of the raster.
  double           height              () const;

  //! Assigns the center coordinates of cell \a r, \a c to \a x, \a y.
  void             center              (size_t r,
                                        size_t c,
                                        double &x,
                                        double &y) const;

  void             center              (CellLoc const& loc,
                                        double &x,
                                        double &y) const;

  void             upperLeft           (size_t r,
                                        size_t c,
                                        double &x,
                                        double &y) const;

  void             lowerRight          (size_t r,
                                        size_t c,
                                        double &x,
                                        double &y) const;

  bool             equals              (const RasterSpace& rs) const;

  // bool operator==(const RasterSpace& cmpTo) const;

 void              rowCol2Coords       (double row,
                                        double col,
                                        double &x,
                                        double &y) const;

  void             loc2Coords          (const CellLoc& loc,
                                        double& x,
                                        double& y) const;

  void             coords2Loc          (double x,
                                        double y,
                                        CellLoc& loc) const;

  void             coords2RowCol       (double x,
                                        double y,
                                        double& row,
                                        double& col) const;

  void             coordinates         (double& x,
                                        double& y,
                                        CellLoc const& cell) const;

  void             coordinates         (double& x,
                                        double& y,
                                        LinearLoc const& cell) const;


  Quadrant         quadrant            (double x,
                                        double y) const;

  friend std::ostream& operator<<      (std::ostream& s,
                                        const RasterSpace& rs);

  friend std::istream& operator>>      (std::istream& s,
                                        RasterSpace& rs);

};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

inline double geo::RasterSpace::cellSize() const
{ return d_cellSize; }

inline double geo::RasterSpace::west() const
{ return d_left; }

inline double geo::RasterSpace::east() const
{ return d_left + nrCols() * d_cellSize; }

inline double geo::RasterSpace::north() const
{ return d_top; }

inline double geo::RasterSpace::south() const
{
  return d_projection == YIncrT2B ? d_top + nrRows() * d_cellSize
                                  : d_top - nrRows() * d_cellSize;
}

inline double geo::RasterSpace::left() const
{ return west(); }

inline double geo::RasterSpace::right() const
{ return east(); }

inline double geo::RasterSpace::top() const
{ return north(); }

inline double geo::RasterSpace::bottom() const
{ return south(); }

inline double geo::RasterSpace::back() const
{ return north(); }

inline double geo::RasterSpace::front() const
{ return south(); }

inline double geo::RasterSpace::width() const
{ return nrCols() * d_cellSize; }

inline double geo::RasterSpace::height() const
{ return nrRows() * d_cellSize; }

inline geo::Projection geo::RasterSpace::projection() const
{ return d_projection; }

inline double geo::RasterSpace::angle() const
{ return d_angle; }


//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (const RasterSpace& lhs,
                                        const RasterSpace& rhs);

bool               operator!=          (const RasterSpace& lhs,
                                        const RasterSpace& rhs);

std::ostream&      operator<<          (std::ostream& s,
                                        const RasterSpace& rs);

std::istream&      operator>>          (std::istream& s,
                                        RasterSpace& rs);



} // namespace geo

#endif
