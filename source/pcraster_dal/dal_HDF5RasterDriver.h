#ifndef INCLUDED_DAL_HDF5RASTERDRIVER
#define INCLUDED_DAL_HDF5RASTERDRIVER



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTERDRIVER
#include "dal_RasterDriver.h"
#define INCLUDED_DAL_RASTERDRIVER
#endif



namespace dal {
  // HDF5RasterDriver declarations.
}



namespace dal {

//! Raster driver for rasters in hdf5 format.
/*!
  This driver implements a convention for storing rasters in the various
  possible data spaces. One attribute is stored in one file or set of files.

  Requirements:
  - One dataset per attribute.
  - Once a dataset is created for a specific dataspace, only slices that fit
    in the dataspace can be written to it / read from it.
  - Upon creation the dataset is layed out to contain the full amount of data
    that can fit in it.
  - Upon writing only one slice of data is written to the dataset,
    possibly overwriting existing data. To fill it the write method can
    be called multiple times.
  - Writing to an existing dataset only succeeds if the dataspace in the
    dataset equals the dataspace passed in.
  - Initialising the dataset / removing an existing one is the responsibility
    of the application.

  Roadmap:
  - Create use cases, implement these as tests.
  - Implement and test the driver for single rasters, empty dataspaces, for
    all possible value types.
  - Handle extensions (see CSFRasterDriver).
  - Handle metadata (value scale, ...).
  - Add driver to the RasterDal and Dal lists of drivers.
  - Handle temporal data.
  - Handle scenarios.
  - Handle quantiles.
  - Document specifics of the dataset layout so other can read and write it
    too. Conventions used.
  - Create a convertion tool to convert rasters in other formats to HDF5 files.
  - Rethink the design, is it good for all uses (large files, ...)? Optional
    multi file strategy?

  \todo Design convention for storing rasters given various possible
        dataspaces.
  \todo Describe convention used.
  \todo Let the extension of the hdf file be optional and apply strategy
        for handling extensions similar to CSFRasterDriver.
  \todo Annotate exception: HDF5 raster driver could not write the raster, etc.
  \todo Convert assertions to exceptions? Other driver too?
*/
class HDF5RasterDriver: public RasterDriver
{

  friend class HDF5RasterDriverTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  HDF5RasterDriver& operator=          (HDF5RasterDriver const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   HDF5RasterDriver    (HDF5RasterDriver const& rhs);

  DataSpace        dataSpace           (std::string const& name) const;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   HDF5RasterDriver    ();

  /* virtual */    ~HDF5RasterDriver   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  bool             exists              (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          open                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  DataSpace        dataSpace           (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  Raster*          read                (std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        TypeId typeId) const;

  void             read                (Raster& raster,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             read                (void* cell,
                                        TypeId typeId,
                                        std::string const& name,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address) const;

  void             write               (Raster const& raster,
                                        DataSpace const& space,
                                        DataSpaceAddress const& address,
                                        std::string const& name) const;

  void             remove              (std::string const& name,
                                        DataSpace space) const;

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
