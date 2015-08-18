#include "ag_Vector.h"

// External headers.

// Project headers.
#include "dal_VectorDriver.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the Vector class.
*/

namespace {

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VECTOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VECTOR MEMBERS
//------------------------------------------------------------------------------

Vector::Vector(
         std::string const& name,
         dal::DataSpace const& space)

  : RasterDataset(name, space)

{
  std::auto_ptr<dal::Vector> vector(dataSource().open<dal::Vector>());
  assert(vector.get());
  assert(vector->typeId() == dal::TI_REAL4 ||
         vector->typeId() == dal::TI_REAL8);

  dal::TypeId useTypeId = dal::TI_REAL4;

  _vector.reset(vector.release());

  _vector->setTypeId(useTypeId);
  _vector->createCells();

  dal::VectorDriver const* driver =
         dynamic_cast<dal::VectorDriver const*>(dataSource().reader());
  assert(driver);

  boost::any min, max;

  if(driver->extremes(min, max, _vector->typeId(), dataSource().name(),
         dataSource().enclosingDataSpace())) {
    setExtremes(min, max);
  }
}



Vector::~Vector()
{
}



void Vector::read(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address)
{
  assert(_vector);

  dal::DataSpaceAddress localAddress(this->localAddress(space, address));
  assert(dataSource().dataSpace().rank() == localAddress.size());

  if(isRead(localAddress)) {
    setAddressRead(localAddress);
  }
  else {
    dal::DataSpaceAddress localAddressWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(localAddress, dal::Space));

    if(!dataSource().enclosingDataSpace().contains(localAddressWithoutSpace)) {
      setAddressRead(dataSource().dataSpace().address());
      assert(!isRead());
      assert(!isRead(localAddress));
      assert(!isRead(addressRead()));
    }
    else {
      dataSource().read(*_vector, localAddressWithoutSpace);
      setAddressRead(localAddress);
      assert(isRead(addressRead()));
    }
  }
}



bool Vector::isRead(
         dal::DataSpaceAddress const& address) const
{
  bool result = false;

  if(isRead()) {
    dal::DataSpaceAddress addressWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(address, dal::Space));
    dal::DataSpaceAddress addressReadWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(addressRead(), dal::Space));
    dal::DataSpace const& space(dataSource().enclosingDataSpace());

    if(space.hasScenarios()) {
      // <hack>
      // Discard scenario setting of address. Make it equal to the scenario
      // in the currently read address.
      size_t index = space.indexOf(dal::Scenarios);
      addressWithoutSpace.setCoordinate<std::string>(index,
         addressReadWithoutSpace.coordinate<std::string>(index));
      // </hack>
    }

    result = space.equal(addressReadWithoutSpace, addressWithoutSpace);
  }

  return result;
}



bool Vector::isRead() const
{
  bool result = false;

  if(addressRead().size() == dataSource().dataSpace().size()) {
    dal::DataSpaceAddress addressReadWithoutSpace(
         dataSource().dataSpace().eraseCoordinates(addressRead(), dal::Space));
    dal::DataSpace const& space(dataSource().enclosingDataSpace());

    result = space.isValid(addressReadWithoutSpace);
  }

  return result;
}



dal::RasterDimensions const& Vector::dimensions() const
{
  return _vector->dimensions();
}



size_t Vector::nrRows() const
{
  return _vector->nrRows();
}



size_t Vector::nrCols() const
{
  return _vector->nrCols();
}



double Vector::cellSize() const
{
  return _vector->cellSize();
}



template<typename T>
bool Vector::isMV(
         size_t row,
         size_t col) const
{
  T value;
  this->value<T>(value, row, col);

  return pcr::isMV(value);
}



bool Vector::isMV(
         size_t row,
         size_t col) const
{
  bool result = true;

  switch(typeId()) {
    case dal::TI_REAL4: {
      isMV<REAL4>(row, col);
      break;
    }
    case dal::TI_REAL8: {
      isMV<REAL8>(row, col);
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}



dal::TypeId Vector::typeId() const
{
  return _vector->typeId();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

