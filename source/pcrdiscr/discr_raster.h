#ifndef INCLUDED_DISCR_RASTER
#define INCLUDED_DISCR_RASTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace discr {
  // Raster declarations.
}



namespace discr {



//! Class for discretisation information of 2D raster data.
/*!
  A raster is defined by its number of rows and cols, the cell size and the
  most x-coordinate of the left side of the western most column and the
  most y-coordinate of the top side of the northern most row.

  Raster objects can be used to supply discretisation information to
  RasterData objects.
*/
class Raster
{

  friend class RasterTest;

private:

  //! Number of rows.
  size_t d_nrRows;

  //! Number of columns.
  size_t d_nrCols;

  //! Cell size.
  double d_cellSize;

  //! X-coordinate of left side of western most column.
  double d_west;

  //! Y-coordinate of top side of northern most row.
  double d_north;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Raster              (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize = 1.0,
                                        double west = 0.0,
                                        double north = 0.0);

  /* virtual */    ~Raster             ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             equals              (Raster const& rhs) const;

  size_t           nrRows              () const;

  size_t           nrCols              () const;

  size_t           nrCells             () const;

  double           cellSize            () const;

  double           west                () const;

  double           north               () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (Raster const& lhs,
                                        Raster const& rhs);

bool               operator!=          (Raster const& lhs,
                                        Raster const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace discr

#endif
