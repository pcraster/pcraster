#ifndef INCLUDED_DAL_BLOCK
#define INCLUDED_DAL_BLOCK



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif



namespace dal {
  // Block declarations.
}



namespace dal {



//! Class for block objects to read or write.
/*!
  A block can contain discretisation information, data or both.

  Discretisation information is information about which part of the
  world is represented and how it is done. In the case of a block the
  discretisation information consists of a raster with base elevations
  and a raster with stacks of voxel thicknesses.

  Data are the attribute values. They are stored in a raster of stacks of
  values.

  Block objects can be configured in three ways: to contain discretisation
  information, data or both. For each of these cases the member variables
  are configured as folows:

  A block with only discretisation information:
  - base elevation: set
  - voxels: not set
  - attribute values: represent the voxel thicknesses

  A block with only data:
  - base elevation: not set
  - voxels: not set
  - attribute values: represent the attribute values

  A block with both discretisation information and data:
  - base elevation: not set
  - voxels: set, configured as a block with only discretisation information
  - attribute values: represent the attribute values

  Base elevation is only set for blocks which contain only discretisation
  information. Such a block might ofcourse be layered in a block which
  contains data.
*/
class PCR_DAL_DECL Block: public Raster
{

  friend class BlockTest;

private:

  //! Base elevation in case this block contains discretisation information.
  Raster*          d_baseElevation;

  //! Optional discretisation information in case this block contains data.
  Block*           d_voxels;

  //! Whether the block is regular or not.
  bool             d_isRegular;

  //! Assignment operator. NOT IMPLEMENTED.
  Block&           operator=           (Block const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Block               (Block const& rhs);

#ifdef DEBUG_DEVELOP
  void             checkIntegrity      () const;
#endif

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Block               (size_t nrRows,
                                        size_t nrCols,
                                        TypeId typeId);

                   Block               (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north);

                   Block               (size_t nrRows,
                                        size_t nrCols,
                                        double cellSize,
                                        double west,
                                        double north,
                                        Raster* baseElevation);

  /* virtual */    ~Block              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setBaseElevation    (Raster* elevation);

  void             setVoxels           (Block* voxels);

  void             setIsRegular        (bool isRegular);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             containsDiscretisationInfo() const;

  bool             containsData        () const;

  bool             containsDiscretisationInfoAndData() const;

  Raster const*    baseElevation       () const;

  Raster*          baseElevation       ();

  Block const*     voxels              () const;

  bool             isRegular           () const;

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



} // namespace dal

#endif
