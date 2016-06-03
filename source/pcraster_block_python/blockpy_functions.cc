#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_BLOCKPY_FUNCTIONS
#include "blockpy_functions.h"
#define INCLUDED_BLOCKPY_FUNCTIONS
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif

#ifndef INCLUDED_BLOCK_FUNCTIONS
#include "block_functions.h"
#define INCLUDED_BLOCK_FUNCTIONS
#endif

#ifndef INCLUDED_DISCR_BLOCK
#include "discr_block.h"
#define INCLUDED_DISCR_BLOCK
#endif

#ifndef INCLUDED_DISCR_BLOCKDATA
#include "discr_blockdata.h"
#define INCLUDED_DISCR_BLOCKDATA
#endif

#ifndef INCLUDED_DISCR_RASTERDATA
#include "discr_rasterdata.h"
#define INCLUDED_DISCR_RASTERDATA
#endif

#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif

// Module headers.



namespace blockpy {

//! Creates a spatial field object based on the data passed in.
/*!
  \param     data Data object to copy.
  \return    spatial field
  \exception .
  \warning   .
  \sa        .

  Just a conversion from discr::RasterData<> to calc::Field.
*/
// template<PCR_VS valueScale>
// calc::Field* field(
//          discr::RasterData<typename ValueScaleTraits<valueScale>::ValueType> const* data)
// {
//   return new calc::Spatial(*data);
// }

#ifdef WIN32
calc::Field* booleanField(
         discr::RasterData<UINT1> const*data)
{
  return new calc::Spatial(VS_B, *data);
}

calc::Field* nominalField(
         discr::RasterData<INT4> const*data)
{
  return new calc::Spatial(VS_N, *data);
}

calc::Field* ordinalField(
         discr::RasterData<INT4> const*data)
{
  return new calc::Spatial(VS_O, *data);
}

calc::Field* scalarField(
         discr::RasterData<REAL4> const*data)
{
  return new calc::Spatial(VS_S, *data);
}

calc::Field* directionalField(
         discr::RasterData<REAL4> const*data)
{
  return new calc::Spatial(VS_D, *data);
}

calc::Field* lddField(
         discr::RasterData<UINT1> const*data)
{
  return new calc::Spatial(VS_L, *data);
}



#else
// How it should work:
template<>
calc::Field* field<VS_B>(
         discr::RasterData<ValueScaleTraits<VS_B>::ValueType> const* data)
{
  return new calc::Spatial(VS_B, *data);
}

template<>
calc::Field* field<VS_L>(
         discr::RasterData<ValueScaleTraits<VS_L>::ValueType> const* data)
{
  return new calc::Spatial(VS_L, *data);
}

template<>
calc::Field* field<VS_N>(
         discr::RasterData<ValueScaleTraits<VS_N>::ValueType> const* data)
{
  return new calc::Spatial(VS_N, *data);
}

template<>
calc::Field* field<VS_O>(
         discr::RasterData<ValueScaleTraits<VS_O>::ValueType> const* data)
{
  return new calc::Spatial(VS_O, *data);
}

template<>
calc::Field* field<VS_S>(
         discr::RasterData<ValueScaleTraits<VS_S>::ValueType> const* data)
{
  return new calc::Spatial(VS_S, *data);
}

template<>
calc::Field* field<VS_D>(
         discr::RasterData<ValueScaleTraits<VS_D>::ValueType> const* data)
{
  return new calc::Spatial(VS_D, *data);
}
#endif



discr::Raster* createRaster(
         geo::RasterSpace const& space)
{
  return new discr::Raster(space.nrRows(), space.nrCols(), space.cellSize(),
         space.west(), space.north());
}



template<typename T>
discr::RasterData<T>* rasterData(
         calc::Field const* field,
         discr::Raster* raster)
{
  std::auto_ptr<discr::RasterData<T> > result;

  if(!field->isSpatial()) {
    result.reset(new discr::RasterData<T>(raster, *field->src_t<T>()));
  }
  else {
    result.reset(new discr::RasterData<T>(raster, field->src_t<T>()));
  }

  return result.release();
}

template
discr::RasterData<UINT1>* rasterData(
         calc::Field const*,
         discr::Raster*);
template
discr::RasterData<INT4>* rasterData(
         calc::Field const*,
         discr::Raster*);
template
discr::RasterData<REAL4>* rasterData(
         calc::Field const*,
         discr::Raster*);



template<typename T>
discr::RasterData<T>* createRasterData(
         discr::Raster* raster,
         T defaultValue)
{
  return new discr::RasterData<T>(raster, defaultValue);
}

template
discr::RasterData<UINT1>* createRasterData(
         discr::Raster* raster,
         UINT1 defaultValue);
template
discr::RasterData<INT4>* createRasterData(
         discr::Raster* raster,
         INT4 defaultValue);
template
discr::RasterData<REAL4>* createRasterData(
         discr::Raster* raster,
         REAL4 defaultValue);



template<typename T>
discr::BlockData<T>* createBlockData(
         discr::Block* block,
         T defaultValue)
{
  return new discr::BlockData<T>(block, defaultValue);
}

template
discr::BlockData<UINT1>* createBlockData(
         discr::Block* block,
         UINT1 defaultValue);
template
discr::BlockData<INT4>* createBlockData(
         discr::Block* block,
         INT4 defaultValue);
template
discr::BlockData<REAL4>* createBlockData(
         discr::Block* block,
         REAL4 defaultValue);



discr::RasterData<REAL4>* baseElevation(
         discr::Block const& arg1)
{
  discr::RasterData<REAL4>* result = new discr::RasterData<REAL4>(&arg1);
  block::baseElevation(*result, arg1);
  return result;
}



discr::RasterData<REAL4>* surfaceElevation(
         discr::Block const& arg1)
{
  discr::RasterData<REAL4>* result = new discr::RasterData<REAL4>(&arg1);
  block::surfaceElevation(*result, arg1);
  return result;
}



//! Creates a VoxelStack object, copies the stack from \a block at \a row, \a col into it and returns the result.
/*!
  \param     block Block to copy voxel stack from.
  \param     row Row number of stack to copy.
  \param     col Column number of stack to copy.
  \return    Newly created voxel stack.
  \warning   \a row and \a col are cell numbers ranging from 1 to nrRows and
             nrCols respectively.
*/
discr::VoxelStack* voxelStack(
         discr::Block const& block,
         size_t row,
         size_t col)
{
  DEVELOP_PRECOND(row >= 1 && row <= block.nrRows());
  DEVELOP_PRECOND(col >= 1 && col <= block.nrCols());

  --row; --col;

  return new discr::VoxelStack(block.cell(row * block.nrCols() + col));
}



template<typename T>
std::vector<T>* voxelStackData(
         discr::BlockData<T> const& data,
         size_t row,
         size_t col)
{
  DEVELOP_PRECOND(row >= 1 && row <= data.block()->nrRows());
  DEVELOP_PRECOND(col >= 1 && col <= data.block()->nrCols());

  --row; --col;

  return new std::vector<T>(data.cell(row * data.block()->nrCols() + col));
}

template
std::vector<UINT1>* voxelStackData(
         discr::BlockData<UINT1> const&,
         size_t row,
         size_t col);
template
std::vector<INT4>* voxelStackData(
         discr::BlockData<INT4> const&,
         size_t row,
         size_t col);
template
std::vector<REAL4>* voxelStackData(
         discr::BlockData<REAL4> const&,
         size_t row,
         size_t col);



template<typename T>
discr::BlockData<UINT1>* equals(
         discr::BlockData<T> const& data,
         T value)
{
  discr::BlockData<UINT1>* result = new discr::BlockData<UINT1>(
         const_cast<discr::BlockData<T>&>(data).block());
  block::equals(*result, data, value);
  return result;
}



template
discr::BlockData<UINT1>* equals(
         discr::BlockData<UINT1> const&,
         UINT1 value);
template
discr::BlockData<UINT1>* equals(
         discr::BlockData<INT4> const&,
         INT4 value);
template
discr::BlockData<UINT1>* equals(
         discr::BlockData<REAL4> const&,
         REAL4 value);



#undef PCR_OPERATOR_TEMPLATES
#define PCR_OPERATOR_TEMPLATES(name)                                           \
template<typename T>                                                           \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const& lhs,                                   \
         T rhs)                                                                \
{                                                                              \
  discr::BlockData<REAL4>* result = new discr::BlockData<REAL4>(lhs);          \
  block::name(*result, rhs);                                                   \
  return result;                                                               \
}                                                                              \
                                                                               \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         UINT1);                                                               \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         INT4);                                                                \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         REAL4);                                                               \
                                                                               \
template<typename T>                                                           \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const& lhs,                                   \
         discr::BlockData<T> const& rhs)                                       \
{                                                                              \
  discr::BlockData<REAL4>* result = new discr::BlockData<REAL4>(lhs);          \
  block::name(*result, rhs);                                                   \
  return result;                                                               \
}                                                                              \
                                                                               \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         discr::BlockData<UINT1> const&);                                      \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         discr::BlockData<INT4> const&);                                       \
template                                                                       \
discr::BlockData<REAL4>* name(                                                 \
         discr::BlockData<REAL4> const&,                                       \
         discr::BlockData<REAL4> const&);

PCR_OPERATOR_TEMPLATES(add)
PCR_OPERATOR_TEMPLATES(substract)
PCR_OPERATOR_TEMPLATES(divide)
PCR_OPERATOR_TEMPLATES(multiply)



// template<typename T>
// discr::BlockData<REAL4>* add(
//          discr::BlockData<REAL4> const& lhs,
//          discr::BlockData<T> const& rhs)
// {
//   discr::BlockData<REAL4>* result = new discr::BlockData<REAL4>(lhs);
//   block::add(*result, rhs);
//   return result;
// }
// 
// template
// discr::BlockData<REAL4>* add(
//          discr::BlockData<REAL4> const&,
//          discr::BlockData<UINT1> const&);
// template
// discr::BlockData<REAL4>* add(
//          discr::BlockData<REAL4> const&,
//          discr::BlockData<INT4> const&);
// template
// discr::BlockData<REAL4>* add(
//          discr::BlockData<REAL4> const&,
//          discr::BlockData<REAL4> const&);
// 
// 
// 
// template<typename T>
// discr::BlockData<REAL4>* divide(
//          discr::BlockData<REAL4> const& lhs,
//          T rhs)
// {
//   discr::BlockData<REAL4>* result = new discr::BlockData<REAL4>(lhs);
//   block::divide(*result, rhs);
//   return result;
// }
// 
// template
// discr::BlockData<REAL4>* divide(
//          discr::BlockData<REAL4> const&,
//          UINT1);
// template
// discr::BlockData<REAL4>* divide(
//          discr::BlockData<REAL4> const&,
//          INT4);
// template
// discr::BlockData<REAL4>* divide(
//          discr::BlockData<REAL4> const&,
//          REAL4);



discr::Block* resample(
         discr::Block const& block,
         REAL4 thickness)
{
  discr::Block* result =
         new discr::Block(static_cast<discr::Raster const&>(block));
  block::resample(*result, block, thickness);
  return result;
}



template<typename T>
discr::BlockData<T>* resample(
         discr::BlockData<T> const& data,
         discr::Block* block)
{
  discr::BlockData<T>* result = new discr::BlockData<T>(block);
  block::resample(*result, data);
  return result;
}



template
discr::BlockData<UINT1>* resample(
         discr::BlockData<UINT1> const&,
         discr::Block*);

template
discr::BlockData<INT4>* resample(
         discr::BlockData<INT4> const&,
         discr::Block*);

template
discr::BlockData<REAL4>* resample(
         discr::BlockData<REAL4> const&,
         discr::Block*);



template<typename T>
discr::RasterData<T>* profile(
         discr::BlockData<T> const& data,
         REAL4 height)
{
  discr::RasterData<T>* result = new discr::RasterData<T>(
         const_cast<discr::Raster*>(
           dynamic_cast<discr::Raster const*>(data.block())));
  block::profile<T>(*result, data, height);
  return result;
}



template
discr::RasterData<UINT1>* profile(
         discr::BlockData<UINT1> const&,
         REAL4);

template
discr::RasterData<INT4>* profile(
         discr::BlockData<INT4> const&,
         REAL4);

template
discr::RasterData<REAL4>* profile(
         discr::BlockData<REAL4> const&,
         REAL4);



template<typename DestinationType, typename SourceType>
discr::BlockData<DestinationType>* cast(
         discr::BlockData<SourceType> const& rhs)
{
  discr::BlockData<DestinationType>* result =
         new discr::BlockData<DestinationType>(
         const_cast<discr::BlockData<SourceType>&>(rhs).block());
  block::cast<DestinationType, SourceType>(*result, rhs);
  return result;
}

template
discr::BlockData<REAL4>* cast(
         discr::BlockData<UINT1> const&);
template
discr::BlockData<REAL4>* cast(
         discr::BlockData<INT4> const&);
template
discr::BlockData<REAL4>* cast(
         discr::BlockData<REAL4> const&);

} // namespace blockpy

