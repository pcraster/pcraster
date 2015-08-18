#ifndef INCLUDED_DAL_MEMORYRASTERDATA
#include "dal_MemoryRasterData.h"
#define INCLUDED_DAL_MEMORYRASTERDATA
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif



/*!
  \file
  This file contains the implementation of the MemoryRasterData class.
*/



namespace dal {

//------------------------------------------------------------------------------

/*
class MemoryRasterDataPrivate
{
public:

  MemoryRasterDataPrivate()
  {
  }

  ~MemoryRasterDataPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYRASTERDATA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYRASTERDATA MEMBERS
//------------------------------------------------------------------------------

MemoryRasterData::MemoryRasterData(
         DataSpace const& dataSpace,
         TypeId typeId,
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : MemoryData(),
    d_dataSpace(dataSpace),
    d_typeId(typeId),
    d_nrRows(nrRows),
    d_nrCols(nrCols),
    d_cellSize(cellSize),
    d_west(west),
    d_north(north)

{
#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



MemoryRasterData::MemoryRasterData(
         std::vector<boost::any>& values,
         DataSpace const& dataSpace,
         TypeId typeId,
         size_t nrRows,
         size_t nrCols,
         double cellSize,
         double west,
         double north)

  : MemoryData(),
    d_values(values),
    d_dataSpace(dataSpace),
    d_typeId(typeId),
    d_nrRows(nrRows),
    d_nrCols(nrCols),
    d_cellSize(cellSize),
    d_west(west),
    d_north(north)

{
#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  updateExtremes();
}



//! Copy constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
MemoryRasterData::MemoryRasterData(
         MemoryRasterData const& rhs)

  : MemoryData(rhs),
    d_values(rhs.d_values),
    d_dataSpace(rhs.d_dataSpace),
    d_typeId(rhs.d_typeId),
    d_nrRows(rhs.d_nrRows),
    d_nrCols(rhs.d_nrCols),
    d_cellSize(rhs.d_cellSize),
    d_west(rhs.d_west),
    d_north(rhs.d_north),
    d_min(rhs.d_min),
    d_max(rhs.d_max)

{
}



MemoryRasterData::~MemoryRasterData()
{
}



//! Assignment operator.
MemoryRasterData& MemoryRasterData::operator=(
         MemoryRasterData const& rhs)
{
  if(this != &rhs) {
    static_cast<MemoryData&>(*this) = rhs;
    d_values = rhs.d_values;
    d_dataSpace = rhs.d_dataSpace;
    d_typeId = rhs.d_typeId;
    d_nrRows = rhs.d_nrRows;
    d_nrCols = rhs.d_nrCols;
    d_cellSize = rhs.d_cellSize;
    d_west = rhs.d_west;
    d_north = rhs.d_north;
    d_min = rhs.d_min;
    d_max = rhs.d_max;
  }

  return *this;
}



void MemoryRasterData::add(
         MemoryRasterData const& /* data */,
         DataSpaceAddress const& /* address */)
{
}



bool MemoryRasterData::hasExtremes()
{
  return !d_min.empty() && !d_max.empty();
}



void MemoryRasterData::updateExtremes()
{
  switch(d_typeId) {
    case TI_INT1 : { updateExtremes<INT1>(); break; }
    case TI_INT2 : { updateExtremes<INT2>(); break; }
    case TI_INT4 : { updateExtremes<INT4>(); break; }
    case TI_UINT1: { updateExtremes<UINT1>(); break; }
    case TI_UINT2: { updateExtremes<UINT2>(); break; }
    case TI_UINT4: { updateExtremes<UINT4>(); break; }
    case TI_REAL4: { updateExtremes<REAL4>(); break; }
    case TI_REAL8: { updateExtremes<REAL8>(); break; }
    case TI_UINT1_VECTOR: { assert(d_typeId != TI_UINT1_VECTOR); break; }
    case TI_INT4_VECTOR: { assert(d_typeId != TI_INT4_VECTOR); break; }
    case TI_REAL4_VECTOR: { assert(d_typeId != TI_REAL4_VECTOR); break; }
    case TI_STRING: { assert(d_typeId != TI_STRING); break; }
    case TI_NR_TYPES: { assert(d_typeId != TI_NR_TYPES); break; }
  }
}



void* MemoryRasterData::cells(
         std::vector<boost::any> values)
{
  assert(values.size() == 1);

  void* result = 0;

  switch(d_typeId) {
    case TI_INT1 : { result = boost::any_cast<INT1* >(values[0]); break; }
    case TI_INT2 : { result = boost::any_cast<INT2* >(values[0]); break; }
    case TI_INT4 : { result = boost::any_cast<INT4* >(values[0]); break; }
    case TI_UINT1: { result = boost::any_cast<UINT1*>(values[0]); break; }
    case TI_UINT2: { result = boost::any_cast<UINT2*>(values[0]); break; }
    case TI_UINT4: { result = boost::any_cast<UINT4*>(values[0]); break; }
    case TI_REAL4: { result = boost::any_cast<REAL4*>(values[0]); break; }
    case TI_REAL8: { result = boost::any_cast<REAL8*>(values[0]); break; }
    case TI_UINT1_VECTOR: { assert(d_typeId != TI_UINT1_VECTOR); break; }
    case TI_INT4_VECTOR: { assert(d_typeId != TI_INT4_VECTOR); break; }
    case TI_REAL4_VECTOR: { assert(d_typeId != TI_REAL4_VECTOR); break; }
    case TI_STRING: { result = boost::any_cast<std::string*>(values[0]); break; }
    case TI_NR_TYPES: { assert(d_typeId != TI_NR_TYPES); break; }
  }

  assert(result);

  return result;
}



// template<class T>
// void* MemoryRasterData::cells(
//          std::vector<boost::any> values)
// {
//   void* result = 
// }



template<class T>
void* MemoryRasterData::cells(
         std::vector<boost::any> values,
         DataSpace space,
         DataSpaceAddress address)
{
  void* result = 0;

  // Use coordinate in address to find element in values vector.
  // Determine which element of the values vector contains the stuff the
  // coordinate in the address points to.

  assert(address.size() > 0);
  T valueToFind = address.coordinate<T>(0);

  size_t i = 0;
  while(i < values.size()) {
    if(comparable<T>(valueToFind, boost::any_cast<T>(boost::get<0>(
         boost::any_cast<boost::tuple<T, std::vector<boost::any> > >(
         values[i]))))) {
      break;
    }

    ++i;
  }

  // Otherwise coordinate not found and we shouldn't be here in the first
  // place.
  assert(i < values.size());

  if(space.rank() == 1) {
    // If this is the last coordinate, the values vector contains the pointer
    // to the data we need.
    result = cells(
         boost::get<1>(
         boost::any_cast<boost::tuple<T, std::vector<boost::any> > >(
         values[i])));
  }
  else {
    // If there are more coordinates, the values vector contains another
    // values vector.
    space.eraseDimension(0);
    address.eraseCoordinate(0);

    result = cells(
         boost::get<1>(
         boost::any_cast<boost::tuple<T, std::vector<boost::any> > >(
         values[i])), space, address);

  }

  assert(result);

  return result;
}



void* MemoryRasterData::cells(
         std::vector<boost::any> values,
         DataSpace space,
         DataSpaceAddress address)
{
  assert(space.rank() == address.size());
  assert(!values.empty());

  void* result = 0;

  if(space.rank() == 0) {
    result = cells(values);
  }
  else {
    Dimension const& dimension(space.dimension(0));

    switch(dimension.meaning()) {
      case Scenarios: {
        result = cells<std::string>(values, space, address);

        break;
      }
      case CumulativeProbabilities: {
        result = cells<float>(values, space, address);

        break;
      }
      case Samples:
      case Time:
      case Space: {
        result = cells<size_t>(values, space, address);

        break;
      }
      case NrMeanings: {
        assert(dimension.meaning() != NrMeanings);
      }
    }
  }

  assert(result);

  return result;
}



#ifdef DEBUG_DEVELOP
template<typename T>
void MemoryRasterData::checkConsistency(
         std::vector<boost::any> values)
{
  assert(values.size() == 1);
  assert(values[0].type() == typeid(T*));
}



void MemoryRasterData::checkConsistency(
         std::vector<boost::any> values)
{
  switch(d_typeId) {
    case TI_INT1:         { checkConsistency<INT1>(values);        break; }
    case TI_INT2:         { checkConsistency<INT2>(values);        break; }
    case TI_INT4:         { checkConsistency<INT4>(values);        break; }
    case TI_UINT1:        { checkConsistency<UINT1>(values);       break; }
    case TI_UINT2:        { checkConsistency<UINT2>(values);       break; }
    case TI_UINT4:        { checkConsistency<UINT4>(values);       break; }
    case TI_REAL4:        { checkConsistency<REAL4>(values);       break; }
    case TI_REAL8:        { checkConsistency<REAL8>(values);       break; }
    case TI_UINT1_VECTOR: { assert(d_typeId != TI_UINT1_VECTOR);   break; }
    case TI_INT4_VECTOR:  { assert(d_typeId != TI_INT4_VECTOR);    break; }
    case TI_REAL4_VECTOR: { assert(d_typeId != TI_REAL4_VECTOR);   break; }
    case TI_STRING:       { checkConsistency<std::string>(values); break; }
    case TI_NR_TYPES:     { assert(d_typeId != TI_NR_TYPES);       break; }
  }
}



template<class T>
void MemoryRasterData::checkConsistency(
         std::vector<boost::any> values,
         DataSpace space)
{
  space.eraseDimension(0);

  for(size_t i = 0; i < values.size(); ++i) {
    assert(values[i].type() ==
         typeid(boost::tuple<T, std::vector<boost::any> >));

    checkConsistency(
         boost::get<1>(
         boost::any_cast<boost::tuple<T, std::vector<boost::any> > >(
         values[i])), space);
  }
}



void MemoryRasterData::checkConsistency(
         std::vector<boost::any> values,
         DataSpace space)
{
  // values contains <T, std::vector<boost::any> > records, unless space does
  // not contain dimensions anymore. In that case values contains one pointer
  // to the data with type d_typeId.
  // Check this structure.

  if(space.rank() == 0) {
    checkConsistency(values);
  }
  else {
    Dimension const& dimension(space.dimension(0));

    switch(dimension.meaning()) {
      case Scenarios: {
        checkConsistency<std::string>(values, space);

        break;
      }
      case CumulativeProbabilities: {
        checkConsistency<float>(values, space);

        break;
      }
      case Samples:
      case Time:
      case Space: {
      /*
        switch(dimension.discretisation()): {
          case(RegularDiscretisation): {
            checkConsistency<...>(values, space);
            break;
          }
          case(BorderedDiscretisation): {
            checkConsistency<...>(values, space);
            break;
          }
          default: {
            assert(false);
            break;
          }
        }
      */

        checkConsistency<size_t>(values, space);

        break;
      }
      case NrMeanings: {
        assert(dimension.meaning() != NrMeanings);

        break;
      }
    }
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Double check, is this set of functions really recursive as it
             should be?
*/
void MemoryRasterData::checkConsistency()
{
  assert(d_typeId != TI_NR_TYPES);
  assert(d_nrRows > 0);
  assert(d_nrCols > 0);
  assert(d_cellSize > 0.0);
  assert(!d_dataSpace.hasSpace());

  checkConsistency(d_values, d_dataSpace);
}
#endif



DataSpace const& MemoryRasterData::dataSpace() const
{
  return d_dataSpace;
}



bool MemoryRasterData::exists() const
{
  return dataSpace().rank() == 0 && !d_values.empty();
}



bool MemoryRasterData::exists(
         DataSpaceAddress const& address) const
{
  return dataSpace().contains(address) && !d_values.empty();
}



//! Returns a newly created Raster object with the layered cell values.
/*!
  \return    Pointer to a new raster or 0.
  \warning   This function creates a new Raster object, it is yours to delete.

  It is assumed that the data space in which the raster is defined is empty.
  If no such raster exists 0 is returned.

  The created Raster object won't delete the layered cell values upon
  destruction, they will stay in memory.
*/
Raster* MemoryRasterData::raster(
         RasterContents contents)
{
  // assert(d_dataSpace.rank() == 0);

  // TODO Use shared_ptr and return shared_ptr.
  Raster* raster = 0;

  if(exists()) {
    raster = new Raster(d_nrRows, d_nrCols, d_cellSize, d_west, d_north,
      d_typeId);

    if(hasExtremes()) {
      raster->setExtremes(d_min, d_max);
    }

    if(contents == IncludingValues) {
      raster->transfer(cells(d_values), Matrix::DoNotTakeOwnerShip);
    }
  }

  return raster;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
*/
Raster* MemoryRasterData::raster(
         TypeId typeId,
         RasterContents contents)
{
  if(typeId == TI_NR_TYPES || typeId == d_typeId) {
    return raster(contents);
  }


  assert(d_dataSpace.rank() == 0);




  // Return newly created Raster object with layered pointer to the data.
  // If typeId differs from the way the data is stored now create a copy of
  // it which may be deleted by the caller.
  // Return 0 when not available.
  return 0;
}



Raster* MemoryRasterData::raster(
         DataSpaceAddress const& address,
         RasterContents contents)
{
  // Find an object at address position.
  // Return 0 when not available.
  // TODO Use shared_ptr and return shared_ptr.
  Raster* raster = 0;

  if(exists(address)) {
    raster = new Raster(d_nrRows, d_nrCols, d_cellSize, d_west, d_north,
         d_typeId);

    if(hasExtremes()) {
      raster->setExtremes(d_min, d_max);
    }

    if(contents == IncludingValues) {
      raster->transfer(cells(d_values, d_dataSpace, address),
         Matrix::DoNotTakeOwnerShip);
    }
  }

  return raster;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
*/
Raster* MemoryRasterData::raster(
         DataSpaceAddress const& address,
         TypeId typeId,
         RasterContents contents)
{
  if(typeId == TI_NR_TYPES || typeId == d_typeId) {
    return raster(address, contents);
  }

  // Find an object at address position.
  // Return newly created Raster object with layered pointer to the data.
  // If typeId differs from the way the data is stored now create a copy of
  // it which may be deleted by the caller.
  // Return 0 when not available.
  return 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

