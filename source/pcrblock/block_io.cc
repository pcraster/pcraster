#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

#ifndef INCLUDED_BOOST_SCOPED_PTR
#include <boost/scoped_ptr.hpp>
#define INCLUDED_BOOST_SCOPED_PTR
#endif

// PCRaster library headers.
#ifndef INCLUDED_DAL_GSLIBBLOCKDRIVER
#include "dal_GSLIBBlockDriver.h"
#define INCLUDED_DAL_GSLIBBLOCKDRIVER
#endif

#ifndef INCLUDED_DAL_PCRBLOCKDRIVER
#include "dal_PCRBlockDriver.h"
#define INCLUDED_DAL_PCRBLOCKDRIVER
#endif

#ifndef INCLUDED_DAL_VTKBLOCKDRIVER
#include "dal_VTKBlockDriver.h"
#define INCLUDED_DAL_VTKBLOCKDRIVER
#endif

// Module headers.
#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif



namespace block {

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Exception instead of POSTCOND.
*/
discr::Block* read(
         std::string const& name)
{
  dal::PCRBlockDriver driver;
  boost::scoped_ptr<dal::Block> blockCopy(
         static_cast<dal::BlockDriver&>(driver).read(name));
  POSTCOND(blockCopy->containsDiscretisationInfo());

  std::auto_ptr<discr::Block> result(new discr::Block(
         blockCopy->nrRows(), blockCopy->nrCols(), blockCopy->cellSize(),
         blockCopy->west(), blockCopy->north()));

  for(size_t i = 0; i < blockCopy->nrCells(); ++i) {
    if(pcr::isMV(blockCopy->baseElevation()->cell<REAL4>(i))) {
      result->cell(i).setMV();
    }
    else {
      discr::VoxelStack& destination(result->cell(i));
      std::vector<REAL4> const& source(blockCopy->cell<std::vector<REAL4> >(i));

      destination.setBaseElevation(blockCopy->baseElevation()->cell<REAL4>(i));
      destination.reserve(source.size());
      destination.insert(destination.begin(), source.begin(), source.end());
    }
  }

  return result.release();
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Exception instead of POSTCOND.
*/
template<typename ValueType>
discr::BlockData<ValueType>* read(
         std::string const& name,
         discr::Block* block)
{
  dal::PCRBlockDriver driver;
  boost::scoped_ptr<dal::Block> blockCopy(
         static_cast<dal::BlockDriver&>(driver).read(name));
  POSTCOND(blockCopy->containsData());

  std::auto_ptr<discr::BlockData<ValueType> > result(
         new discr::BlockData<ValueType>(block));
  POSTCOND(blockCopy->nrRows() == block->nrRows());
  POSTCOND(blockCopy->nrCols() == block->nrCols());

  for(size_t i = 0; i < block->nrCells(); ++i) {
    if(!block->cell(i).isMV()) {
      std::vector<ValueType>& destination(result->cell(i));
      std::vector<ValueType>& source(
         blockCopy->cell<std::vector<ValueType> >(i));
      POSTCOND(source.size() == block->cell(i).size());
      POSTCOND(destination.size() == block->cell(i).size());

      std::copy(source.begin(), source.end(), destination.begin());
    }
  }

  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY((*(result.get())));
  return result.release();
}



static dal::Block* createBlockForDiscretisation(
         discr::Block const& block)
{
  std::auto_ptr<dal::Block> result(new dal::Block(
         block.nrRows(), block.nrCols(), block.cellSize(),
         block.west(), block.north()));
  result->createCells();
  result->baseElevation()->createCells();
  result->setIsRegular(block.isRegular());

  for(size_t i = 0; i < block.nrCells(); ++i) {
    if(block.cell(i).isMV()) {
      pcr::setMV(result->baseElevation()->cell<REAL4>(i));
    }
    else {
      result->baseElevation()->cell<REAL4>(i) = block.cell(i).baseElevation();
      std::vector<REAL4>& stack(result->cell<std::vector<REAL4> >(i));
      stack.reserve(block.cell(i).size());
      stack.insert(stack.begin(), block.cell(i).begin(), block.cell(i).end());
    }
  }

  result->baseElevation()->setExtremes();

  return result.release();
}



template<typename ValueType>
static dal::Block* createBlockForData(
         discr::BlockData<ValueType> const& data)
{
  discr::Block const& block(*data.block());
  std::auto_ptr<dal::Block> result(new dal::Block(
         block.nrRows(), block.nrCols(),
         dal::TypeTraits<std::vector<ValueType> >::typeId));
  result->createCells();

  DEVELOP_POSTCOND(result->cellsAreCreated());

  for(size_t i = 0; i < data.block()->nrCells(); ++i) {
    std::vector<ValueType>& stack(result->cell<std::vector<ValueType> >(i));
    stack.reserve(data.cell(i).size());
    stack.insert(stack.begin(), data.cell(i).begin(), data.cell(i).end());
  }

  return result.release();
}



static void write(
         discr::Block const& block,
         std::string const& name,
         dal::BlockDriver& driver)
{
  std::auto_ptr<dal::Block> blockToWrite(createBlockForDiscretisation(block));
  driver.write(*blockToWrite, name);
}



void write(
         discr::Block const& block,
         std::string const& name)
{
  dal::PCRBlockDriver driver;
  write(block, name, driver);
}



void writeVTK(
         discr::Block const& block,
         std::string const& name)
{
  dal::VTKBlockDriver driver;
  write(block, name, driver);
}



template<typename ValueType>
static void write(
         discr::BlockData<ValueType> const& data,
         std::string const& name,
         dal::BlockDriver& driver)
{
  std::auto_ptr<dal::Block> blockToWrite(createBlockForData<ValueType>(data));
  DEVELOP_POSTCOND(blockToWrite->containsData());

  if(static_cast<dal::BlockDriver const&>(driver)
         .properties().value<dal::DriverProperties>(DAL_DRIVER_GENERAL) &
         dal::CombinesDiscretisationAndData) {
    // Set up a block which contains data AND discretisation information.
    // The data is already in the block, now add discretisation information.
    blockToWrite->setVoxels(createBlockForDiscretisation(*data.block()));
    DEVELOP_POSTCOND(blockToWrite->containsDiscretisationInfoAndData());
  }

  driver.write(*blockToWrite, name);
}



template<typename ValueType>
void write(
         discr::BlockData<ValueType> const& data,
         std::string const& name)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  dal::PCRBlockDriver driver;
  write<ValueType>(data, name, driver);
}



template<typename ValueType>
void writeVTK(
         discr::BlockData<ValueType> const& data,
         std::string const& name)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  dal::VTKBlockDriver driver;
  write<ValueType>(data, name, driver);
}



template<typename ValueType>
void writeGSLIB(
         discr::BlockData<ValueType> const& data,
         std::string const& name)
{
  DEVELOP_CHECK_BLOCK_DATA_CONSISTENCY(data);

  dal::GSLIBBlockDriver driver;
  write<ValueType>(data, name, driver);
}



/*
template<typename ValueType>
void write(
         discr::BlockData<ValueType> const& data,
         REAL4 height,
         std::string const& name)
{
  discr::Raster<ValueType> profile(dynamic_cast<Raster const&>(data.block()));




  dal::Raster raster;



  dal::CSFRasterDriver driver;
  static_cast<dal::RasterDriver&>(driver).write(name, raster);
}
*/



template
discr::BlockData<UINT1>* read<UINT1>(
         std::string const&,
         discr::Block*);
template
discr::BlockData<INT4>* read<INT4>(
         std::string const&,
         discr::Block*);
template
discr::BlockData<REAL4>* read<REAL4>(
         std::string const&,
         discr::Block*);
template
void write<UINT1>(
         discr::BlockData<UINT1> const&,
         std::string const&);
template
void write<INT4>(
         discr::BlockData<INT4> const&,
         std::string const&);
template
void write<REAL4>(
         discr::BlockData<REAL4> const&,
         std::string const&);
template
void writeVTK<UINT1>(
         discr::BlockData<UINT1> const&,
         std::string const&);
template
void writeVTK<INT4>(
         discr::BlockData<INT4> const&,
         std::string const&);
template
void writeVTK<REAL4>(
         discr::BlockData<REAL4> const&,
         std::string const&);
template
void writeGSLIB<UINT1>(
         discr::BlockData<UINT1> const&,
         std::string const&);
template
void writeGSLIB<INT4>(
         discr::BlockData<INT4> const&,
         std::string const&);
template
void writeGSLIB<REAL4>(
         discr::BlockData<REAL4> const&,
         std::string const&);
/*
template
void write<UINT1>(
         discr::BlockData<UINT1> const&,
         REAL4 height,
         std::string const&);
template
void write<INT4>(
         discr::BlockData<INT4> const&,
         REAL4 height,
         std::string const&);
template
void write<REAL4>(
         discr::BlockData<REAL4> const&,
         REAL4 height,
         std::string const&);
         */

} // namespace block

