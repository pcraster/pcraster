#ifndef INCLUDED_DISCR_BLOCK
#define INCLUDED_DISCR_BLOCK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SIGNALS2_SIGNAL
#include <boost/signals2/signal.hpp>
#define INCLUDED_BOOST_SIGNALS2_SIGNAL
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DISCR_RASTER
#include "discr_raster.h"
#define INCLUDED_DISCR_RASTER
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

#ifndef INCLUDED_DISCR_VOXELSTACK
#include "discr_voxelstack.h"
#define INCLUDED_DISCR_VOXELSTACK
#endif



namespace discr {
  // Block declarations.
}



namespace discr {



//! Class for discretisation information of 3D block data.
/*!
  Block is a Raster with information about voxels at each cell.

  This class contains information about the discretisation of space using
  voxels. This is done using a raster with stacks of voxels. Each stack
  contains information about the elevation of the bottom voxel (base elevation)
  and the thicknesses of each voxel in the stack.

  Block objects can be used to supply discretisation information to
  BlockData objects.

  Inheriting from Raster is for convenience only, its members
  are handy to have close by. But the information in Raster is also available
  from the RasterData interface. This scheme makes it possible to do
  block.nrRows() which is more intuitive than doing block.raster().nrRows().
*/
class Block: public Raster,
             public RasterData<VoxelStack>
{

  friend class BlockTest;

private:

  //! Signal emitted when one or more voxels have been added.
  boost::signals2::signal<void(size_t, size_t)> d_addVoxelsSignal;

  //! Signal emitted when one or more voxels have been removed.
  boost::signals2::signal<void(size_t, size_t)> d_removeVoxelsSignal;

  // //! Signal emitted when a voxel has been cut.
  // boost::signals2::signal<void(size_t, REAL4)> d_cutVoxelSignal;

  //! Assignment operator. NOT IMPLEMENTED.
  Block&           operator=           (Block const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Block               (Block const& rhs);

protected:

public:

  typedef VoxelStack::value_type ThicknessType;

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Block               (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north);

                   Block               (Raster const& raster);

                   Block               (Raster const& raster,
                                        REAL4 baseElevation);

                   Block               (RasterData<REAL4> const& baseElevation);

  /* virtual */    ~Block              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             addVoxel            (size_t index,
                                        REAL4 thickness);

  void             addVoxels           (size_t index,
                                        size_t nr,
                                        REAL4 thickness);

  void             addVoxels           (size_t nr,
                                        REAL4 thickness);

  void             removeVoxels        (size_t index,
                                        size_t nr);

  void             cutVoxel            (size_t index,
                                        REAL4 fraction);

  void             setMV               (size_t index);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  boost::signals2::signal<void(size_t, size_t)>& addVoxelsSignal();

  boost::signals2::signal<void(size_t, size_t)>& removeVoxelsSignal();

  // boost::signals2::signal<void(size_t, REAL4)>& cutVoxelSignal();

  bool             equals              (Block const& rhs) const;

  bool             isEmpty             () const;

  bool             isRegular           () const;

  size_t           nrVoxels            () const;

  bool             bottomElevation     (REAL4& elevation) const;

  bool             topElevation        (REAL4& elevation) const;

  bool             extremeElevations   (REAL4& bottomElevation,
                                        REAL4& topElevation) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (Block const& lhs,
                                        Block const& rhs);

bool               operator!=          (Block const& lhs,
                                        Block const& rhs);

//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace discr

#endif
