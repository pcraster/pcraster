#ifndef INCLUDED_DAL_VTKBLOCKDRIVER
#include "dal_VTKBlockDriver.h"
#define INCLUDED_DAL_VTKBLOCKDRIVER
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif



/*!
  \file
  This file contains the implementation of the VTKBlockDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VTKBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

void VTKBlockDriver::regularBlockProperties(
         size_t* nrVoxelsPerStack,
         double* voxelThickness,
         Raster const& voxels)
{
  for(size_t i = 0; i < voxels.nrCells(); ++i) {
    std::vector<REAL4> const& stack(voxels.cell<std::vector<REAL4> >(i));

    if(!stack.empty()) {
      // Block is regular, these values are valid for the whole block.
      *nrVoxelsPerStack = stack.size();
      *voxelThickness = stack[0];
      break;
    }
  }
}



template<typename ValueType>
void VTKBlockDriver::cellData(
         std::string& result,
         Block const& block)
{
  assert(block.nrCells() > 0);

  size_t nrVoxelsPerStack = block.cell<std::vector<ValueType> >(0).size();
  std::vector<std::vector<ValueType> const*> stacks(block.nrCells());
  size_t i;

  // Store pointers to stacks for efficiency.
  for(int row = block.nrRows() - 1; row >= 0; --row) {
    for(size_t col = 0; col < block.nrCols(); ++col) {
      i = row * block.nrCols() + col;
      stacks[i] = &block.cell<std::vector<ValueType> >(i);
    }
  }

  // Values are build from the bottom to the top, first the bottom layer of
  // voxels, than the second, etc. Also, we start with the bottom left cell
  // of a raster instead of the upper left cell.
  for(size_t voxel = 0; voxel < nrVoxelsPerStack; ++voxel) {
    for(int row = block.nrRows() - 1; row >= 0; --row) {
      for(size_t col = 0; col < block.nrCols(); ++col) {
        i = row * block.nrCols() + col;
        result += " ";
        if(pcr::isMV((*stacks[i])[voxel])) {
          result += "-999";
        }
        else {
          result += boost::lexical_cast<std::string>(REAL4((*stacks[i])[voxel]));
        }
      }
    }
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF VTKBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

VTKBlockDriver::VTKBlockDriver()

  : BlockDriver(Format("VTKBlock", "VTK block file format",
         BLOCK, Format::File, Format::Block, Format::Attribute)),
    TextFileDriver()

{
  DriverProperties& properties = this->properties().value<DriverProperties>(
         DAL_DRIVER_GENERAL);
  properties |= CombinesDiscretisationAndData;
  properties |= Writer;

  std::vector<std::string> extensions;
  extensions.push_back(".vtk");
  extensions.push_back(".xml");
  format().setExtensions(extensions);
}



/* NOT IMPLEMENTED
//! Copy constructor.
VTKBlockDriver::VTKBlockDriver(
         VTKBlockDriver const& rhs)

  : Base(rhs)

{
}
*/



VTKBlockDriver::~VTKBlockDriver()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
VTKBlockDriver& VTKBlockDriver::operator=(
         VTKBlockDriver const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



bool VTKBlockDriver::exists(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */) const
{
  assert(false);
  return false;
}



Block* VTKBlockDriver::open(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         TypeId /* typeId */) const
{
  assert(false);
  return 0;
}



Block* VTKBlockDriver::read(
         std::string const& /* name */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         TypeId /* typeId */) const
{
  assert(false);
  return 0;
}



void VTKBlockDriver::write(
         Block const& block,
         boost::filesystem::path const& path) const
{
  assert(
         block.containsDiscretisationInfo() ||
         block.containsDiscretisationInfoAndData());

  if(!block.isRegular()) {
    throwCannotWrite(path.string(), BLOCK,
         (boost::format("Driver %1% only supports regular blocks")
           % name()).str());
  }

  std::ofstream stream;

  if(!TextFileDriver::open(stream, path)) {
    throwCannotBeOpened(path.string(), BLOCK);
  }

  // TODO create empty block when extremes are not set.
  // assert(block.baseElevation()->hasExtremes());
  // TODO create empty block when raster is empty.
  // assert(block.nrCells() > 0);

  std::string vtkTypeId;
  double voxelThickness = 0.0;
  size_t nrVoxelsPerStack = 0;
  std::string cellData;

  if(block.containsDiscretisationInfo()) {
    vtkTypeId = "Float32";
    this->regularBlockProperties(
         &nrVoxelsPerStack, &voxelThickness,
         dynamic_cast<Raster const&>(block));
    this->cellData<REAL4>(cellData, block);
  }
  else {
    // Block contains discretisation info and data.
    switch(block.typeId()) {
      case(TI_UINT1_VECTOR): {
        vtkTypeId = "UInt8";
        this->regularBlockProperties(
            &nrVoxelsPerStack, &voxelThickness, *block.voxels());
        this->cellData<UINT1>(cellData, block);
        break;
      }
      case(TI_INT4_VECTOR): {
        vtkTypeId = "Int32";
        this->regularBlockProperties(
            &nrVoxelsPerStack, &voxelThickness, *block.voxels());
        this->cellData<INT4>(cellData, block);
        break;
      }
      case(TI_REAL4_VECTOR): {
        vtkTypeId = "Float32";
        this->regularBlockProperties(
            &nrVoxelsPerStack, &voxelThickness, *block.voxels());
        this->cellData<REAL4>(cellData, block);
        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }

  double voxelSpacing = voxelThickness / block.cellSize();

  stream << (boost::format("\
<VTKFile type=\"ImageData\">\n\
<ImageData\n\
  WholeExtent=\"%1% %2% %3% %4% %5% %6%\"\n\
  Origin=\"%1% %3% %5%\"\n\
  Spacing=\"%7% %8% %9%\">\n\
  <Piece\n\
    Extent=\"%1% %2% %3% %4% %5% %6%\">\n\
    <CellData\n\
      Scalars=\"Thickness\">\n\
      <DataArray\n\
        type=\"Float32\"\n\
        Name=\"Attribute\">\n\
        %10%\n\
      </DataArray>\n\
    </CellData>\n\
  </Piece>\n\
</ImageData>\n\
</VTKFile>\n")
         // WholeExtent
         % 0
         % block.nrCols()
         % 0
         % block.nrRows()
         % 0
         % nrVoxelsPerStack
         // Spacing
         % 1.0
         % 1.0
         % voxelSpacing
         // Data
         % cellData
         ).str();
}



void VTKBlockDriver::write(
         Block const& block,
         DataSpace const& space,
         DataSpaceAddress const& address,
         std::string const& name) const
{
  write(block, pathForDataSpaceAddress(name, space, address));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

