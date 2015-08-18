#ifndef INCLUDED_GEO_BLOCK
#define INCLUDED_GEO_BLOCK



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_CSF
#include "csf.h"
#define INCLUDED_CSF
#endif

#ifndef INCLUDED_GEO_DEF
#include "geo_def.h"
#define INCLUDED_GEO_DEF
#endif

#ifndef INCLUDED_GEO_RASTER
#include "geo_raster.h"
#define INCLUDED_GEO_RASTER
#endif

#ifndef INCLUDED_GEO_VOXEL
#include "geo_voxel.h"
#define INCLUDED_GEO_VOXEL
#endif

#ifndef INCLUDED_GEO_VOXELSTACK
#include "geo_voxelstack.h"
#define INCLUDED_GEO_VOXELSTACK
#endif



namespace geo {



/*!
  \class Block
  \brief short_description

  longer_description
*/
//       1         2         3         4         5         6         7         8
class Block: public Raster<VoxelStack>
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Block &          operator=           (const Block &);

  //! Copy constructor. NOT IMPLEMENTED.
                   Block               (const Block &);

  //! Frees dynamically allocated memory.
  void             clean               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

/*
                   Block               ();
*/

  //! Constructor.
                   Block               (size_t nr,
                                        size_t nc,
                                        REAL8 cellSize,
                                        REAL8 left,
                                        REAL8 top,
                                        Projection proj = YIncrB2T);

  //! Destructor.
  /* virtual */    ~Block              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Adds layer of \a n voxels w sed type \a s, thickness \a t on top of block.
  void             addLayer            (size_t n,
                                        INT4 s,
                                        REAL8 t);

  friend std::istream &operator>>      (std::istream &s,
                                        Block &b);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  //! Returns the thickness of the block.
  REAL8            thickness           () const;

  //! Returns the lowest coordinate of the block.
  REAL8            low                 () const;

  //! Returns the highest coordinate of the block.
  REAL8            high                () const;

  size_t           nrVoxels            () const;


  friend std::ostream &operator<<      (std::ostream &s,
                                        const Block &b);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



} // namespace geo

#endif
