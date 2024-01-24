#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#define INCLUDED_DAL_RASTERDIMENSIONS



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_MATRIXDIMENSIONS
#include "dal_MatrixDimensions.h"
#define INCLUDED_DAL_MATRIXDIMENSIONS
#endif

#ifndef INCLUDED_DAL_SPACEDIMENSIONS
#include "dal_SpaceDimensions.h"
#define INCLUDED_DAL_SPACEDIMENSIONS
#endif

#include <tuple>


namespace dal {
  // RasterDimensions declarations.
}



namespace dal {

//! Class for objects representing the dimensions of a raster.
/*!
  The dimensionality of a raster is defined by its MatrixDimensions, its
  SpaceDimensions, and its cell size.
*/
class PCR_DAL_DECL RasterDimensions: public MatrixDimensions,
                                     public SpaceDimensions
{

  friend class RasterDimensionsTest;
  friend PCR_DAL_DECL bool operator==(
         RasterDimensions const&, RasterDimensions const&);
  friend PCR_DAL_DECL bool operator!=(
         RasterDimensions const&, RasterDimensions const&);

private:

  //! Size of individual cells.
  double           _cellSize{1.0};

  bool             equals              (RasterDimensions const& rhs) const;

protected:

public:

  static std::tuple<RasterDimensions, RasterDimensions> overlap(
                                        RasterDimensions const& dimensions1,
                                        RasterDimensions const& dimensions2);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RasterDimensions    ();

                   RasterDimensions    (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize=1.0,
                                        double west=0.0,
                                        double north=0.0);

                   RasterDimensions    (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        SpatialCoordinate const& northWest);

                   RasterDimensions    (RasterDimensions const& rhs);

  /* virtual */    ~RasterDimensions   () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  RasterDimensions const& operator=    (RasterDimensions const& rhs);

  RasterDimensions const& operator|=   (RasterDimensions const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  double           cellSize            () const;

  size_t           index               (size_t row,
                                        size_t col) const;

  size_t           index               (double x,
                                        double y) const;

  void             indices             (double x,
                                        double y,
                                        double& row,
                                        double& col) const;

  void             indices             (SpatialCoordinate const& address,
                                        double& row,
                                        double& col) const;

  RasterDimensions areaDimensions      (double west,
                                        double north,
                                        double east,
                                        double south) const;

  bool             containsCell        (double row,
                                        double col) const;

  void             coordinates         (size_t index,
                                        double& x,
                                        double& y) const;

  void             coordinates         (double row,
                                        double col,
                                        double& x,
                                        double& y) const;

  bool             compatible          (RasterDimensions const& rhs) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

PCR_DAL_DECL bool  operator==          (RasterDimensions const& lhs,
                                        RasterDimensions const& rhs);

PCR_DAL_DECL bool  operator!=          (RasterDimensions const& lhs,
                                        RasterDimensions const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

#endif
