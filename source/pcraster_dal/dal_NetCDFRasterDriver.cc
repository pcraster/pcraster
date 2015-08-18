#ifndef INCLUDED_DAL_NETCDFRASTERDRIVER
#include "dal_NetCDFRasterDriver.h"
#define INCLUDED_DAL_NETCDFRASTERDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_NETCDFCPP
#include <netcdfcpp.h>
#define INCLUDED_NETCDFCPP
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the NetCDFRasterDriver class.
*/



namespace dal {

static TypeId typeId(
         NcType type)
{
  TypeId typeId = TI_NR_TYPES;

  switch(type) {
    case ncByte: {
      typeId = TI_INT1;
      break;
    }
    case ncShort: {
      typeId = TI_INT2;
      break;
    }
    case ncInt: {
      typeId = TI_INT4;
      break;
    }
    case ncChar: {
      typeId = TI_UINT1;
      break;
    }
    case ncFloat: {
      typeId = TI_REAL4;
      break;
    }
    case ncDouble: {
      typeId = TI_REAL8;
      break;
    }
    default: {
      break;
    }
  }

  return typeId;
}



static NcType netCDFType(
         TypeId typeId)
{
  NcType netCDFType = ncNoType;

  switch(typeId) {
    case TI_INT1: {
      netCDFType = ncByte;
      break;
    }
    case TI_INT2: {
      netCDFType = ncShort;
      break;
    }
    case TI_INT4: {
      netCDFType = ncInt;
      break;
    }
    case TI_UINT1: {
      netCDFType = ncChar;
      break;
    }
    case TI_UINT2: {
      // TODO Handle situation.
      assert(false);
      break;
    }
    case TI_UINT4: {
      // TODO Handle situation.
      assert(false);
      break;
    }
    case TI_REAL4: {
      netCDFType = ncFloat;
      break;
    }
    case TI_REAL8: {
      netCDFType = ncDouble;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return netCDFType;
}



//------------------------------------------------------------------------------
// DEFINITION OF STATIC NETCDFRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF NETCDFRASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

NetCDFRasterDriver::NetCDFRasterDriver()

  : RasterDriver(Format("NetCDF", "NetCDF raster file format",
         RASTER, Format::File, Format::Attribute))

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= Reader;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".nc"); // According to the CF convention.
  format().setExtensions(extensions);

  // Supported value types:
  // INT1, UINT1, INT2, INT4, REAL4, REAL8
}



/* NOT IMPLEMENTED
//! Copy constructor.
NetCDFRasterDriver::NetCDFRasterDriver(
         NetCDFRasterDriver const& rhs)

  : Base(rhs)

{
}
*/



NetCDFRasterDriver::~NetCDFRasterDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
NetCDFRasterDriver& NetCDFRasterDriver::operator=(
         NetCDFRasterDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



DataSpace NetCDFRasterDriver::dataSpace(
         std::string const& /* name */) const
{
  DataSpace result;

  // Open the file and see if it contains something useful.
  // Determine dimensionality of the data and configure DataSpace object.
  assert(false);

  return result;
}



bool NetCDFRasterDriver::exists(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  // See if the file with name exists. If so, check whether a raster at
  // the specified address is present.
  return false;
}



Raster* NetCDFRasterDriver::open(
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
DataSpace NetCDFRasterDriver::dataSpace(
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



Raster* NetCDFRasterDriver::read(
         std::string const& /* name */,
         DataSpace const& space,
         DataSpaceAddress const& address,
         TypeId /* typeId */) const
{
  assert(space.isEmpty() && address.size() == 0);

  return 0;
/*
#include <iostream>
#include <netcdfcpp.h>

using namespace std;

// We are reading 2D data, a 6 x 12 grid. 
static const int NDIMS = 2;
static const int NX = 6;
static const int NY = 12;

// Return this in event of a problem.
static const int NC_ERR = 2;

int main(void)
{
   // This is the array we will read.
   int dataIn[NX][NY]; 

   // Open the file. The ReadOnly parameter tells netCDF we want
   // read-only access to the file.
   NcFile dataFile("simple_xy.nc", NcFile::ReadOnly);

   // You should always check whether a netCDF file open or creation
   // constructor succeeded.
   if (!dataFile.is_valid())
   {
      cout << "Couldn't open file!\n";
      return NC_ERR;
   }
  
   // For other method calls, the default behavior of the C++ API is
   // to exit with a message if there is an error.  If that behavior
   // is OK, there is no need to check return values in simple cases
   // like the following.
      
   // Retrieve the variable named "data"
   NcVar *data = dataFile.get_var("data");

   // Read all the values from the "data" variable into memory. 
   data->get(&dataIn[0][0], NX, NY);

   // Check the values. 
   for (int i = 0; i < NX; i++)
      for (int j = 0; j < NY; j++)
	 if (dataIn[i][j] != i * NY + j)
	    return NC_ERR;
    
   // The netCDF file is automatically closed by the NcFile destructor
   cout << "*** SUCCESS reading example file simple_xy.nc!" << endl;

   return 0;
}
*/
}



void NetCDFRasterDriver::read(
         Raster& /* raster */,
         std::string const& /* name */,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(false);

  assert(space.isEmpty() && address.size() == 0);
}



void NetCDFRasterDriver::read(
         void* /* cell */,
         TypeId /* typeId */,
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
}




template<typename T>
void NetCDFRasterDriver::write(
         Raster const& raster,
         NcVar* data) const
{
  NcBool result = data->put(raster.cells<T>(),
         raster.nrRows(), raster.nrCols());
  assert(result == TRUE);
}



void NetCDFRasterDriver::write(
         Raster const& raster,
         NcVar* data) const
{
  switch(raster.typeId()) {
    // TODO
    // case TI_INT1: {
    //   write<INT1>(raster, data);
    //   break;
    // }
    case TI_INT2: {
      write<INT2>(raster, data);
      break;
    }
    case TI_INT4: {
      write<INT4>(raster, data);
      break;
    }
    case TI_UINT1: {
      write<UINT1>(raster, data);
      break;
    }
    case TI_REAL4: {
      write<REAL4>(raster, data);
      break;
    }
    case TI_REAL8: {
      write<REAL8>(raster, data);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }
}



void NetCDFRasterDriver::write(
         Raster const& raster,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  // TODO In the real case the file should only be created when it does not
  // TODO already exists or, when it exists but is not compatible with the
  // TODO file format and space of the dataset. If it is compatible then
  // TODO the raster should be put at the specified address in the dataset,
  // TODO overwriting existing data.

  assert(space.isEmpty() && address.size() == 0);    // Location to write to.
  assert(space.rank() == 2);          // Only 2 space for now.
  assert(raster.typeId() == TI_INT4);

  NcFile file(name.c_str(), NcFile::Replace);

  if(!file.is_valid()) {
    throwCannotBeCreated(name, RASTER);
  }

  NcDim* rows = file.add_dim("row", raster.nrRows());
  NcDim* cols = file.add_dim("col", raster.nrCols());
  NcVar* data = file.add_var("data", netCDFType(raster.typeId()), rows, cols);
  write(raster, data);

/*
#include <iostream>
#include <netcdfcpp.h>

using namespace std;

// We are writing 2D data, a 6 x 12 grid. 
static const int NDIMS = 2;
static const int NX = 6;
static const int NY = 12;

// Return this in event of a problem.
static const int NC_ERR = 2;

int 
main(void)
{
   // This is the data array we will write. It will just be filled
   // with a progression of numbers for this example.
   int dataOut[NX][NY];
  
   // Create some pretend data. If this wasn't an example program, we
   // would have some real data to write, for example, model output.
   for(int i = 0; i < NX; i++) 
      for(int j = 0; j < NY; j++)
	 dataOut[i][j] = i * NY + j;

   // Create the file. The Replace parameter tells netCDF to overwrite
   // this file, if it already exists.
   NcFile dataFile("simple_xy.nc", NcFile::Replace);

   // You should always check whether a netCDF file creation or open
   // constructor succeeded.
   if (!dataFile.is_valid())
   {
      cout << "Couldn't open file!\n";
      return NC_ERR;
   }
  
   // For other method calls, the default behavior of the C++ API is
   // to exit with a message if there is an error.  If that behavior
   // is OK, there is no need to check return values in simple cases
   // like the following.

   // When we create netCDF dimensions, we get back a pointer to an
   // NcDim for each one.
   NcDim* xDim = dataFile.add_dim("x", NX);
   NcDim* yDim = dataFile.add_dim("y", NY);
  
   // Define a netCDF variable. The type of the variable in this case
   // is ncInt (32-bit integer).
   NcVar *data = dataFile.add_var("data", ncInt, xDim, yDim);
     
   // Write the pretend data to the file. Although netCDF supports
   // reading and writing subsets of data, in this case we write all
   // the data in one operation.
   data->put(&dataOut[0][0], NX, NY);

   // The file will be automatically close when the NcFile object goes
   // out of scope. This frees up any internal netCDF resources
   // associated with the file, and flushes any buffers.
   cout << "*** SUCCESS writing example file simple_xy.nc!" << endl;

   return 0;
}
*/
}



void NetCDFRasterDriver::remove(
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

