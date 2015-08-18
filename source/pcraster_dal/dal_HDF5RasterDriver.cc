#ifndef INCLUDED_DAL_HDF5RASTERDRIVER
#include "dal_HDF5RasterDriver.h"
#define INCLUDED_DAL_HDF5RASTERDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_H5CPP
#include <H5Cpp.h>
#define INCLUDED_H5CPP
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the HDF5RasterDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------

/*
class HDF5RasterDriverPrivate
{
public:

  HDF5RasterDriverPrivate()
  {
  }

  ~HDF5RasterDriverPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC HDF5RASTERDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF HDF5RASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

HDF5RasterDriver::HDF5RasterDriver()

  : RasterDriver(Format("HDF5", "HDF5 raster file format",
         RASTER, Format::File, Format::Attribute))

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".h5");
  extensions.push_back(".hdf5");
  extensions.push_back(".hdf");
  format().setExtensions(extensions);

  // Turn off the auto-printing when failure occurs so that we can handle
  // the errors appropriately.
  H5::Exception::dontPrint();
}



/* NOT IMPLEMENTED
//! Copy constructor.
HDF5RasterDriver::HDF5RasterDriver(
         HDF5RasterDriver const& rhs)

  : Base(rhs)

{
}
*/



HDF5RasterDriver::~HDF5RasterDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
HDF5RasterDriver& HDF5RasterDriver::operator=(
         HDF5RasterDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



DataSpace HDF5RasterDriver::dataSpace(
         std::string const& /* name */) const
{
  DataSpace result;

  // Open the file and see if it contains something useful.
  // Determine dimensionality of the data and configure DataSpace object.
  assert(false);

  return result;
}



bool HDF5RasterDriver::exists(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  // See if the file with name exists. If so, check whether a raster at
  // the specified address is present.
  return false;
}



Raster* HDF5RasterDriver::open(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         TypeId /* typeId */) const
{
  return 0;
}



/*!
  \overload
  \warning   Since datasets writting by this driver contain all data from a
             dataspace, this function only returns something useful when
             \a space is empty. If not, an empty dataspace is returned.
*/
DataSpace HDF5RasterDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  DataSpace result;

  if(space.isEmpty()) {
    assert(address.size() == 0);

    result = dataSpace(name);
  }

  return result;
}



Raster* HDF5RasterDriver::read(
         std::string const& /* name */,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId /* typeId */) const
{
  assert(space.isEmpty() && address.size() == 0);

  return 0;
}



void HDF5RasterDriver::read(
         Raster& /* raster */,
         std::string const& /* name */,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(false);

  assert(space.isEmpty() && address.size() == 0);

  try {

    // const int    NX_SUB = 3;        // hyperslab dimensions
    // const int    NY_SUB = 4;
    // const int    NX = 7;            // output buffer dimensions
    // const int    NY = 7;
    // const int    NZ = 3;
    // const int    RANK_OUT = 3;
    // 
    // int main (void)
    // {
    //    /*
    //     * Output buffer initialization.
    //     */
    //    int i, j, k;
    //    int         data_out[NX][NY][NZ ]; /* output buffer */
    //    for (j = 0; j < NX; j++)
    //    {
    //       for (i = 0; i < NY; i++)
    //       {
    //          for (k = 0; k < NZ ; k++)
    //             data_out[j][i][k] = 0;
    //       }
    //    }
    // 



    // /*
    //  * Open the specified file and the specified dataset in the file.
    //  */
    // H5File file( name, H5F_ACC_RDONLY );
    // DataSet dataset = file.openDataSet( "raster" );
    // 
    // /*
    //  * Get the class of the datatype that is used by the dataset.
    //  */
    // H5T_class_t type_class = dataset.getTypeClass();
    // 
    // /*
    //  * Get class of datatype and print message if it's an integer.
    //  */
    // if( type_class == H5T_INTEGER )
    // {
    //    cout << "Data set has INTEGER type" << endl;
    // 
    //    /*
    //     * Get the integer datatype
    //     */
    //    IntType intype = dataset.getIntType();
    // 
    //    /*
    //     * Get order of datatype and print message if it's a little endian.
    //     */
    //    string order_string;
    //    H5T_order_t order = intype.getOrder( order_string );
    //    cout << order_string << endl;
    // 
    //    /*
    //     * Get size of the data element stored in file and print it.
    //     */
    //    size_t size = intype.getSize();
    //    cout << "Data size is " << size << endl;
    // }
    // 
    // /*
    //  * Get dataspace of the dataset.
    //  */
    // DataSpace dataspace = dataset.getSpace();
    // 
    // /*
    //  * Get the number of dimensions in the dataspace.
    //  */
    // int rank = dataspace.getSimpleExtentNdims();
    // 
    // /*
    //  * Get the dimension size of each dimension in the dataspace and
    //  * display them.
    //  */
    // hsize_t dims_out[2];
    // int ndims = dataspace.getSimpleExtentDims( dims_out, NULL);
    // cout << "rank " << rank << ", dimensions " <<
    //         (unsigned long)(dims_out[0]) << " x " <<
    //         (unsigned long)(dims_out[1]) << endl;
    // 
    // /*
    //  * Define hyperslab in the dataset; implicitly giving strike and
    //  * block NULL.
    //  */
    // hsize_t      offset[2];   // hyperslab offset in the file
    // hsize_t      count[2];    // size of the hyperslab in the file
    // offset[0] = 1;
    // offset[1] = 2;
    // count[0]  = NX_SUB;
    // count[1]  = NY_SUB;
    // dataspace.selectHyperslab( H5S_SELECT_SET, count, offset );
    // 
    // /*
    //  * Define the memory dataspace.
    //  */
    // hsize_t     dimsm[3];              /* memory space dimensions */
    // dimsm[0] = NX;
    // dimsm[1] = NY;
    // dimsm[2] = NZ ;
    // DataSpace memspace( RANK_OUT, dimsm );
    // 
    // /*
    //  * Define memory hyperslab.
    //  */
    // hsize_t      offset_out[3];       // hyperslab offset in memory
    // hsize_t      count_out[3];        // size of the hyperslab in memory
    // offset_out[0] = 3;
    // offset_out[1] = 0;
    // offset_out[2] = 0;
    // count_out[0]  = NX_SUB;
    // count_out[1]  = NY_SUB;
    // count_out[2]  = 1;
    // memspace.selectHyperslab( H5S_SELECT_SET, count_out, offset_out );
    // 
    // /*
    //  * Read data from hyperslab in the file into the hyperslab in
    //  * memory and display the data.
    //  */
    // dataset.read( data_out, PredType::NATIVE_INT, memspace, dataspace );
    // 
    // for (j = 0; j < NX; j++)
    // {
    //   for (i = 0; i < NY; i++)
    //      cout << data_out[j][i][0] << " ";
    //   cout << endl;
    // }
    // /*
    //  * 0 0 0 0 0 0 0
    //  * 0 0 0 0 0 0 0
    //  * 0 0 0 0 0 0 0
    //  * 3 4 5 6 0 0 0
    //  * 4 5 6 7 0 0 0
    //  * 5 6 7 8 0 0 0
    //  * 0 0 0 0 0 0 0
    //  */
  }
  catch(H5::Exception const& exception) {
    throw Exception(exception.getDetailMsg());
  }
}



void HDF5RasterDriver::read(
         void* /* cell */,
         TypeId /* typeId */,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
}



void HDF5RasterDriver::write(
         Raster const& raster,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  assert(false);

  // TODO In the real case the file should only be created when it does not
  // TODO already exists or, when it exists but is not compatible with the
  // TODO file format and space of the dataset. If it is compatible then
  // TODO the raster should be put at the specified address in the dataset,
  // TODO overwriting existing data.

  assert(space.isEmpty() && address.size() == 0);    // Location to write to.
  assert(space.rank() == 2);          // Only 2 space for now.
  assert(raster.typeId() == TI_INT4);

  try {
    // Create a new file using H5F_ACC_TRUNC access, default file creation
    // properties, and default file access properties.
    H5::H5File file(name, H5F_ACC_TRUNC);

    // Define the size of the array and create the data space for fixed
    // size dataset.
    int rank = 2;
    hsize_t dimsf[2];                  // Dataset dimensions.
    dimsf[0] = raster.nrCols();
    dimsf[1] = raster.nrRows();
    H5::DataSpace dataspace(rank, dimsf);

    // Define datatype for the data in the file.
    // We will store little endian INT numbers.
    H5::IntType datatype(H5::PredType::NATIVE_INT);
    datatype.setOrder(H5T_ORDER_LE);

    // Create a new dataset within the file using defined dataspace and
    // datatype and default dataset creation properties.
    H5::DataSet dataset = file.createDataSet("raster", datatype, dataspace);

    // Write the data to the dataset using default memory space, file space,
    // and transfer properties.
    dataset.write(raster.cells(), H5::PredType::NATIVE_INT);
  }
  catch(H5::Exception const& exception) {
    throw Exception(exception.getDetailMsg());
  }
}



void HDF5RasterDriver::remove(
         std::string const& name,
         DataSpace space) const
{
  if(space.rank() == 0) {
    boost::filesystem::path path(pathFor(name));

    if(dal::exists(path)) {
      dal::remove(path);
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

