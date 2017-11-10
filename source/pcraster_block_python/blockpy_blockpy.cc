#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
// This one first, to prevent warnings.
#include <boost/python.hpp>

// PCRaster library headers.
#ifndef INCLUDED_BLOCK_CLAYCOMPACTOR
#include "block_claycompactor.h"
#define INCLUDED_BLOCK_CLAYCOMPACTOR
#endif

#ifndef INCLUDED_BLOCK_COMPACTORS
#include "block_compactors.h"
#define INCLUDED_BLOCK_COMPACTORS
#endif

#ifndef INCLUDED_BLOCK_DUMMYCOMPACTOR
#include "block_dummycompactor.h"
#define INCLUDED_BLOCK_DUMMYCOMPACTOR
#endif

#ifndef INCLUDED_BLOCK_FUNCTIONS
#include "block_functions.h"
#define INCLUDED_BLOCK_FUNCTIONS
#endif

#ifndef INCLUDED_BLOCK_SANDCOMPACTOR
#include "block_sandcompactor.h"
#define INCLUDED_BLOCK_SANDCOMPACTOR
#endif

#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

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

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

#ifndef INCLUDED_RASTER_FUNCTIONS
#include "raster_functions.h"
#define INCLUDED_RASTER_FUNCTIONS
#endif

#ifndef INCLUDED_VOXELSTACK_FUNCTIONS
#include "voxelstack_functions.h"
#define INCLUDED_VOXELSTACK_FUNCTIONS
#endif

// Module headers.
#ifndef INCLUDED_BLOCKPY_FUNCTIONS
#include "blockpy_functions.h"
#define INCLUDED_BLOCKPY_FUNCTIONS
#endif


#if _MSC_VER == 1900
  // Workaround wrt Boost Python and VS2015v3
  namespace boost
  {
    template <>
    calc::Field const volatile * get_pointer(class calc::Field const volatile *f)
    {
      return f;
    }

    template <>
    geo::RasterSpace const volatile * get_pointer<class geo::RasterSpace const volatile>
      (class geo::RasterSpace const volatile *r)
    {
      return r;
    }
  }
#endif

//! Translates dal::Exception to Python RuntimeError exception.
void translator1(dal::Exception const& exception) {
  PyErr_SetString(PyExc_RuntimeError, exception.message().c_str());
}

// //! Translates com::Exception to Python RuntimeError exception.
// void translator2(com::Exception const& exception) {
//   PyErr_SetString(PyExc_RuntimeError, exception.messages().c_str());
// }



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



BOOST_PYTHON_MODULE(_pcraster_block_python)
{
  using namespace boost::python;

  // Copied from PCRaster.cc.
  register_exception_translator<dal::Exception>(&::translator1);
  // register_exception_translator<com::Exception>(&pp::translator2);
  // /Copied from PCRaster.cc.

  class_<discr::Raster, boost::noncopyable>("Raster",
         no_init)
         // init<size_t, size_t, double, double, double>())
         ;
  class_<discr::RasterData<UINT1>, boost::noncopyable>("UINT1RasterData",
         no_init)
         // init<discr::Raster*, UINT1>())
         ;
  class_<discr::RasterData<INT4>, boost::noncopyable>("INT4RasterData",
         no_init)
         // init<discr::Raster*, INT4>())
         ;
  class_<discr::RasterData<REAL4>, boost::noncopyable>("REAL4RasterData",
         no_init)
         // init<discr::Raster*, REAL4>())
         ;

  class_<discr::Block, boost::noncopyable>("Block",
         no_init)
  //        // init<discr::RasterData<REAL4> const&>())
  //        .def("nrCells", &discr::Block::nrCells)
  //        .def("cell", cell,
  //          return_value_policy<reference_existing_object>())
         ;
  class_<discr::BlockData<UINT1>, boost::noncopyable>("UINT1BlockData",
         no_init)
         // init<discr::Block*, UINT1>())
         // .def("setDefaultValue", &discr::BlockData<UINT1>::setDefaultValue)
         ;
  class_<discr::BlockData<INT4>, boost::noncopyable>("INT4BlockData",
         no_init)
         // init<discr::Block*, INT4>())
         // .def("setDefaultValue", &discr::BlockData<INT4>::setDefaultValue)
         ;
  class_<discr::BlockData<REAL4>, boost::noncopyable>("REAL4BlockData",
         no_init)
         // init<discr::Block*, REAL4>())
         // .def("setDefaultValue", &discr::BlockData<REAL4>::setDefaultValue)
         ;
  class_<discr::VoxelStack, boost::noncopyable>("VoxelStack",
         no_init)
         // .def(vector_indexing_suite<discr::VoxelStack>())
         // .def("thickness", &discr::VoxelStack::thickness)
         ;
  class_<std::vector<UINT1>, boost::noncopyable>("UINT4VoxelStackData",
         no_init)
         ;
  class_<std::vector<INT4>, boost::noncopyable>("INT4VoxelStackData",
         no_init)
         ;
  class_<std::vector<REAL4>, boost::noncopyable>("REAL4VoxelStackData",
         no_init)
         ;
  class_<block::Compactors<block::MackeyBridgeCompactor>, boost::noncopyable>("Compactors")
         .def("setCompactor", &block::Compactors<block::MackeyBridgeCompactor>::setCompactor)
         ;
  class_<block::SandCompactor>("SandCompactor")
         ;
  class_<block::ClayCompactor>("ClayCompactor")
         ;
  class_<block::DummyCompactor>("DummyCompactor")
         ;

  class_<block::Compactors<block::DeHaanCompactor>, boost::noncopyable>("DeHaanCompactors")
         .def("setCompactor", &block::Compactors<block::DeHaanCompactor>::setCompactor)
         ;

  class_<block::DeHaanCompactor>("DeHaanCompactor",
         init<double, double, double>())
         ;

  // implicitly_convertible<block::DummyCompactor, block::Compactors<block::MackeyBridgeCompactor>::Compactor>();
  implicitly_convertible<block::DummyCompactor, block::MackeyBridgeCompactor>();

  // Create raster discretisation.
  def("createRaster", &blockpy::createRaster,
         return_value_policy<manage_new_object>())
         ;

  // Create raster discretisation.
  def("createRaster", &raster::create,
         return_value_policy<manage_new_object>(),
  "Creates a new raster\n\n"
  "nrRows          Number of rows.\n"
  "nrCols          Number of columns.\n"
  "cellSize        Size of the cells.\n"
  "west            Coordinate of the left side of the western most cell.\n"
  "west            Coordinate of the top side of the northern most cell.\n");

  // Create block data.
  def("createUINT1RasterData", &blockpy::createRasterData<UINT1>,
         return_value_policy<manage_new_object>());
  def("createINT4RasterData", &blockpy::createRasterData<INT4>,
         return_value_policy<manage_new_object>());
  def("createREAL4RasterData", &blockpy::createRasterData<REAL4>,
         return_value_policy<manage_new_object>());

  // Create block discretisation.
  def("createBlock", &block::create,
         return_value_policy<manage_new_object>(),
  "Creates a new block\n\n"
  "baseElevation   Dem to use for the base elevation.\n"
  "The lateral properties of the created block will be the same as for the\n"
  "raster passed in.");

  // Create block data.
  def("createUINT1BlockData", &blockpy::createBlockData<UINT1>,
         return_value_policy<manage_new_object>());
  def("createINT4BlockData", &blockpy::createBlockData<INT4>,
         return_value_policy<manage_new_object>());
  def("createREAL4BlockData", &blockpy::createBlockData<REAL4>,
         return_value_policy<manage_new_object>());

  // Convert raster data to field.
#ifdef WIN32
  def("booleanField", &blockpy::booleanField,
         return_value_policy<manage_new_object>());
  def("lddField", &blockpy::lddField,
         return_value_policy<manage_new_object>());
  def("nominalField", &blockpy::nominalField,
         return_value_policy<manage_new_object>());
  def("ordinalField", &blockpy::ordinalField,
         return_value_policy<manage_new_object>());
  def("scalarField", &blockpy::scalarField,
         return_value_policy<manage_new_object>());
  def("directionalField", &blockpy::directionalField,
         return_value_policy<manage_new_object>());
#else
  // How it should work.
  def("booleanField", &blockpy::field<VS_B>,
         return_value_policy<manage_new_object>());
  def("lddField", &blockpy::field<VS_L>,
         return_value_policy<manage_new_object>());
  def("nominalField", &blockpy::field<VS_N>,
         return_value_policy<manage_new_object>());
  def("ordinalField", &blockpy::field<VS_O>,
         return_value_policy<manage_new_object>());
  def("scalarField", &blockpy::field<VS_S>,
         return_value_policy<manage_new_object>());
  def("directionalField", &blockpy::field<VS_D>,
         return_value_policy<manage_new_object>());
#endif

  // Convert field to raster data.
  def("uint1RasterData", &blockpy::rasterData<UINT1>,
         return_value_policy<manage_new_object>());
  def("int4RasterData", &blockpy::rasterData<INT4>,
         return_value_policy<manage_new_object>());
  def("real4RasterData", &blockpy::rasterData<REAL4>,
         return_value_policy<manage_new_object>());

  // Cast block data.
  def("real4BlockData", &blockpy::cast<REAL4, UINT1>,
         return_value_policy<manage_new_object>());
  def("real4BlockData", &blockpy::cast<REAL4, INT4>,
         return_value_policy<manage_new_object>());
  def("real4BlockData", &blockpy::cast<REAL4, REAL4>,
         return_value_policy<manage_new_object>());

  def("noCompactionAdd", noCompactionAdd1);
  def("noCompactionAdd", noCompactionAdd2);
  def("mackeyBridgeAdd", &block::mackeyBridgeAdd);
  def("deHaanAdd", &block::deHaanAdd);
  def("remove", &block::remove);

  def("baseElevation", &blockpy::baseElevation,
         return_value_policy<manage_new_object>());
  def("surfaceElevation", &blockpy::surfaceElevation,
         return_value_policy<manage_new_object>());

  def("resample", resampleBlock,
         return_value_policy<manage_new_object>());
  def("resample", &blockpy::resample<UINT1>,
         return_value_policy<manage_new_object>());
  def("resample", &blockpy::resample<INT4>,
         return_value_policy<manage_new_object>());
  def("resample", &blockpy::resample<REAL4>,
         return_value_policy<manage_new_object>());

  def("readBlock", readBlock,
         return_value_policy<manage_new_object>());
  def("readUINT1BlockData", &block::read<UINT1>,
         return_value_policy<manage_new_object>());
  def("readINT4BlockData", &block::read<INT4>,
         return_value_policy<manage_new_object>());
  def("readREAL4BlockData", &block::read<REAL4>,
         return_value_policy<manage_new_object>());

  def("write", writeBlock);
  def("writeVTK", writeBlockVTK);

  def("write", &block::write<UINT1>);
  def("write",  &block::write<INT4>);
  def("write", &block::write<REAL4>);

  def("writeVTK", &block::writeVTK<UINT1>);
  def("writeVTK",  &block::writeVTK<INT4>);
  def("writeVTK", &block::writeVTK<REAL4>);

  def("writeGSLIB", &block::writeGSLIB<UINT1>);
  def("writeGSLIB",  &block::writeGSLIB<INT4>);
  def("writeGSLIB", &block::writeGSLIB<REAL4>);

  def("writeBinary", &raster::writeBinary<UINT1>);
  def("writeBinary",  &raster::writeBinary<INT4>);
  def("writeBinary", &raster::writeBinary<REAL4>);

  def("setMVUINT1", &raster::setMV<UINT1>);
  def("setMVINT4",  &raster::setMV<INT4>);
  def("setMVREAL4", &raster::setMV<REAL4>);

  // def("equals", &blockpy::equals<UINT1>,
  //        return_value_policy<manage_new_object>());
  // def("equals", &blockpy::equals<INT4>,
  //        return_value_policy<manage_new_object>());
  // def("equals", &blockpy::equals<REAL4>,
  //        return_value_policy<manage_new_object>());

  def("voxelStack", &blockpy::voxelStack,
         return_value_policy<manage_new_object>());
  def("nrVoxels", &voxelstack::nrVoxels);
  def("thickness", &voxelstack::thickness);
  def("uint1VoxelStackData", &blockpy::voxelStackData<UINT1>,
         return_value_policy<manage_new_object>());
  def("int4VoxelStackData", &blockpy::voxelStackData<INT4>,
         return_value_policy<manage_new_object>());
  def("real4VoxelStackData", &blockpy::voxelStackData<REAL4>,
         return_value_policy<manage_new_object>());
  def("real4VoxelValue", &voxelstack::value<REAL4>);

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name, type)                                     \
def(#name, &blockpy::name<type>,                                                         \
       return_value_policy<manage_new_object>());
// def(#name, name##BlockData<type>,
//        return_value_policy<manage_new_object>());

  PCR_OPERATOR_TEMPLATES(equals, UINT1)
  PCR_OPERATOR_TEMPLATES(equals, INT4)
  PCR_OPERATOR_TEMPLATES(equals, REAL4)

#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
def(#name, name##Value,                                                        \
       return_value_policy<manage_new_object>());                              \
def(#name, name##BlockData,                                                    \
       return_value_policy<manage_new_object>());

  PCR_OPERATOR_TEMPLATES(add)
  PCR_OPERATOR_TEMPLATES(substract)
  PCR_OPERATOR_TEMPLATES(multiply)
  PCR_OPERATOR_TEMPLATES(divide)

  def("profile", &blockpy::profile<UINT1>,
         return_value_policy<manage_new_object>());
  def("profile",  &blockpy::profile<INT4>,
         return_value_policy<manage_new_object>());
  def("profile", &blockpy::profile<REAL4>,
         return_value_policy<manage_new_object>());

  def("setDefaultValue", &block::setDefaultValue<UINT1>);
  def("setDefaultValue", &block::setDefaultValue<INT4>);
  def("setDefaultValue", &block::setDefaultValue<REAL4>);


}

