#include "stddefx.h"
#include "block_claycompactor.h"
#include "block_compactors.h"
#include "block_dummycompactor.h"
#include "block_functions.h"
#include "block_sandcompactor.h"
#include "calc_spatial.h"
#include "dal_Exception.h"
#include "discr_block.h"
#include "discr_blockdata.h"
#include "discr_raster.h"
#include "discr_rasterdata.h"
#include "discr_voxelstack.h"
#include "geo_rasterspace.h"
#include "raster_functions.h"
#include "voxelstack_functions.h"
#include "blockpy_functions.h"

#include <pybind11/pybind11.h>



// discr::VoxelStack const& (discr::Block::*cell)(size_t) const =
//          &discr::Block::cell;
discr::Block*
  (*resampleBlock)
    (discr::Block const&, REAL4) = blockpy::resample;
discr::Block*
  (*readBlock)
    (std::string const&) = block::read;
void
  (*writeBlock)
    (discr::Block const&, std::string const&) = block::write;
void
  (*writeBlockVTK)
    (discr::Block const&, std::string const&) = block::writeVTK;


void
  (*noCompactionAdd1)
    (discr::Block&, size_t, REAL4) = block::noCompactionAdd;
void
  (*noCompactionAdd2)
    (discr::Block&, discr::RasterData<REAL4> const&) = block::noCompactionAdd;




#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
discr::BlockData<REAL4>*                                                       \
  (*name##Value)                                                               \
    (discr::BlockData<REAL4> const&, REAL4) = blockpy::name;                   \
discr::BlockData<REAL4>*                                                       \
  (*name##BlockData)                                                           \
    (discr::BlockData<REAL4> const&, discr::BlockData<REAL4> const&) =         \
         blockpy::name;

PCR_OPERATOR_TEMPLATES(add)
PCR_OPERATOR_TEMPLATES(substract)
PCR_OPERATOR_TEMPLATES(multiply)
PCR_OPERATOR_TEMPLATES(divide)



PYBIND11_MODULE(_pcraster_block_python, module)
{
  using namespace pybind11;

  register_exception_translator([](std::exception_ptr p) {
    try {
      if (p) {
        std::rethrow_exception(p);
      }
    }
    catch (dal::Exception const& exception) {
      PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
    }
  });

  class_<discr::Raster>(module, "Raster")
         // init<size_t, size_t, double, double, double>())
         ;
  class_<discr::RasterData<UINT1>>(module, "UINT1RasterData")
         // init<discr::Raster*, UINT1>())
         ;
  class_<discr::RasterData<INT4>>(module, "INT4RasterData")
         // init<discr::Raster*, INT4>())
         ;
  class_<discr::RasterData<REAL4>>(module, "REAL4RasterData")
         // init<discr::Raster*, REAL4>())
         ;

  class_<discr::Block>(module, "Block")
  //        // init<discr::RasterData<REAL4> const&>())
  //        .def("nrCells", &discr::Block::nrCells)
  //        .def("cell", cell,
  //          return_value_policy<reference_existing_object>())
         ;
  class_<discr::BlockData<UINT1>>(module, "UINT1BlockData")
         // init<discr::Block*, UINT1>())
         // .def("setDefaultValue", &discr::BlockData<UINT1>::setDefaultValue)
         ;
  class_<discr::BlockData<INT4>>(module, "INT4BlockData")
         // init<discr::Block*, INT4>())
         // .def("setDefaultValue", &discr::BlockData<INT4>::setDefaultValue)
         ;
  class_<discr::BlockData<REAL4>>(module, "REAL4BlockData")
         // init<discr::Block*, REAL4>())
         // .def("setDefaultValue", &discr::BlockData<REAL4>::setDefaultValue)
         ;
  class_<discr::VoxelStack>(module, "VoxelStack")
         // .def(vector_indexing_suite<discr::VoxelStack>())
         // .def("thickness", &discr::VoxelStack::thickness)
         ;
  class_<std::vector<UINT1>>(module, "UINT4VoxelStackData")
         ;
  class_<std::vector<INT4>>(module, "INT4VoxelStackData")
         ;
  class_<std::vector<REAL4>>(module, "REAL4VoxelStackData")
         ;
  class_<block::Compactors<block::MackeyBridgeCompactor>>(module, "Compactors")
         .def("setCompactor", &block::Compactors<block::MackeyBridgeCompactor>::setCompactor)
         ;
  class_<block::SandCompactor>(module, "SandCompactor")
         ;
  class_<block::ClayCompactor>(module, "ClayCompactor")
         ;
  class_<block::DummyCompactor>(module, "DummyCompactor")
         ;

  class_<block::Compactors<block::DeHaanCompactor>>(module, "DeHaanCompactors")
         .def("setCompactor", &block::Compactors<block::DeHaanCompactor>::setCompactor)
         ;

  class_<block::DeHaanCompactor>(module, "DeHaanCompactor")
         .def(init<double, double, double>())
         ;

  // implicitly_convertible<block::DummyCompactor, block::Compactors<block::MackeyBridgeCompactor>::Compactor>();
  // implicitly_convertible<block::DummyCompactor, block::MackeyBridgeCompactor>(); removed fttb, causes module import to fail

  // Create raster discretisation.
  module.def("createRaster", &blockpy::createRaster)
         ;

  // Create raster discretisation.
  module.def("createRaster", &raster::create,
  "Creates a new raster\n\n"
  "nrRows          Number of rows.\n"
  "nrCols          Number of columns.\n"
  "cellSize        Size of the cells.\n"
  "west            Coordinate of the left side of the western most cell.\n"
  "west            Coordinate of the top side of the northern most cell.\n");

  // Create block data.
  module.def("createUINT1RasterData", &blockpy::createRasterData<UINT1>);
  module.def("createINT4RasterData", &blockpy::createRasterData<INT4>);
  module.def("createREAL4RasterData", &blockpy::createRasterData<REAL4>);

  // Create block discretisation.
  module.def("createBlock", &block::create,
  "Creates a new block\n\n"
  "baseElevation   Dem to use for the base elevation.\n"
  "The lateral properties of the created block will be the same as for the\n"
  "raster passed in.");

  // Create block data.
  module.def("createUINT1BlockData", &blockpy::createBlockData<UINT1>);
  module.def("createINT4BlockData", &blockpy::createBlockData<INT4>);
  module.def("createREAL4BlockData", &blockpy::createBlockData<REAL4>);

  // Convert raster data to field.
#ifdef WIN32
  module.def("booleanField", &blockpy::booleanField);
  module.def("lddField", &blockpy::lddField);
  module.def("nominalField", &blockpy::nominalField);
  module.def("ordinalField", &blockpy::ordinalField);
  module.def("scalarField", &blockpy::scalarField);
  module.def("directionalField", &blockpy::directionalField);
#else
  // How it should work.
  module.def("booleanField", &blockpy::field<VS_B>);
  module.def("lddField", &blockpy::field<VS_L>);
  module.def("nominalField", &blockpy::field<VS_N>);
  module.def("ordinalField", &blockpy::field<VS_O>);
  module.def("scalarField", &blockpy::field<VS_S>);
  module.def("directionalField", &blockpy::field<VS_D>);
#endif

  // Convert field to raster data.
  module.def("uint1RasterData", &blockpy::rasterData<UINT1>);
  module.def("int4RasterData", &blockpy::rasterData<INT4>);
  module.def("real4RasterData", &blockpy::rasterData<REAL4>);

  // Cast block data.
  module.def("real4BlockData", &blockpy::cast<REAL4, UINT1>);
  module.def("real4BlockData", &blockpy::cast<REAL4, INT4>);
  module.def("real4BlockData", &blockpy::cast<REAL4, REAL4>);

  module.def("noCompactionAdd", noCompactionAdd1);
  module.def("noCompactionAdd", noCompactionAdd2);
  module.def("mackeyBridgeAdd", &block::mackeyBridgeAdd);
  module.def("deHaanAdd", &block::deHaanAdd);
  module.def("remove", &block::remove);

  module.def("baseElevation", &blockpy::baseElevation);
  module.def("surfaceElevation", &blockpy::surfaceElevation);

  module.def("resample", resampleBlock);
  module.def("resample", &blockpy::resample<UINT1>);
  module.def("resample", &blockpy::resample<INT4>);
  module.def("resample", &blockpy::resample<REAL4>);

  module.def("readBlock", readBlock);
  module.def("readUINT1BlockData", &block::read<UINT1>);
  module.def("readINT4BlockData", &block::read<INT4>);
  module.def("readREAL4BlockData", &block::read<REAL4>);

  module.def("write", writeBlock);
  module.def("writeVTK", writeBlockVTK);

  module.def("write", &block::write<UINT1>);
  module.def("write",  &block::write<INT4>);
  module.def("write", &block::write<REAL4>);

  module.def("writeVTK", &block::writeVTK<UINT1>);
  module.def("writeVTK",  &block::writeVTK<INT4>);
  module.def("writeVTK", &block::writeVTK<REAL4>);

  module.def("writeGSLIB", &block::writeGSLIB<UINT1>);
  module.def("writeGSLIB",  &block::writeGSLIB<INT4>);
  module.def("writeGSLIB", &block::writeGSLIB<REAL4>);

  module.def("writeBinary", &raster::writeBinary<UINT1>);
  module.def("writeBinary",  &raster::writeBinary<INT4>);
  module.def("writeBinary", &raster::writeBinary<REAL4>);

  module.def("setMVUINT1", &raster::setMV<UINT1>);
  module.def("setMVINT4",  &raster::setMV<INT4>);
  module.def("setMVREAL4", &raster::setMV<REAL4>);

  // def("equals", &blockpy::equals<UINT1>,
  //        return_value_policy<manage_new_object>());
  // def("equals", &blockpy::equals<INT4>,
  //        return_value_policy<manage_new_object>());
  // def("equals", &blockpy::equals<REAL4>,
  //        return_value_policy<manage_new_object>());

  module.def("voxelStack", &blockpy::voxelStack);
  module.def("nrVoxels", &voxelstack::nrVoxels);
  module.def("thickness", &voxelstack::thickness);
  module.def("uint1VoxelStackData", &blockpy::voxelStackData<UINT1>);
  module.def("int4VoxelStackData", &blockpy::voxelStackData<INT4>);
  module.def("real4VoxelStackData", &blockpy::voxelStackData<REAL4>);
  module.def("real4VoxelValue", &voxelstack::value<REAL4>);

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name, type)                                     \
module.def(#name, &blockpy::name<type>);
// def(#name, name##BlockData<type>,
//        return_value_policy<manage_new_object>());

  PCR_OPERATOR_TEMPLATES(equals, UINT1)
  PCR_OPERATOR_TEMPLATES(equals, INT4)
  PCR_OPERATOR_TEMPLATES(equals, REAL4)

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
module.def(#name, name##Value);                              \
module.def(#name, name##BlockData);

  PCR_OPERATOR_TEMPLATES(add)
  PCR_OPERATOR_TEMPLATES(substract)
  PCR_OPERATOR_TEMPLATES(multiply)
  PCR_OPERATOR_TEMPLATES(divide)

  module.def("profile", &blockpy::profile<UINT1>);
  module.def("profile",  &blockpy::profile<INT4>);
  module.def("profile", &blockpy::profile<REAL4>);

  module.def("setDefaultValue", &block::setDefaultValue<UINT1>);
  module.def("setDefaultValue", &block::setDefaultValue<INT4>);
  module.def("setDefaultValue", &block::setDefaultValue<REAL4>);


}

