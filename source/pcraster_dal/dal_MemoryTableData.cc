#include "dal_MemoryTableData.h"

#include <map>
#include <string>



/*!
  \file
  This file contains the implementation of the MemoryTableData class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYTABLEDATA MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYTABLEDATA MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     space Enclosing data space of table.
  \param     address Address in \a space this table is located at.
  \param     table Table to store.
  \warning   \a space must not contain Space dimensions.
*/
MemoryTableData::MemoryTableData(
         DataSpace const& space,
         DataSpaceAddress const& address,
         Table* table)

  : MemoryData(),
    d_dataSpace(space)

{
  assert(!space.hasSpace());

  initialiseValues(d_values, d_dataSpace);
  add(table, address);
}



//! Copy constructor.
/*!
  \param     rhs Object to copy construct from.
  \warning   The stored pointer are shallow copied.
*/
MemoryTableData::MemoryTableData(
         MemoryTableData const& rhs)

  : MemoryData(rhs),
    d_values(rhs.d_values),
    d_dataSpace(rhs.d_dataSpace)

{
  // copy(rhs.d_values, rhs.d_dataSpace, d_values);
}



//! Destructor.
/*!
  \sa        clear()

  The layered (pointer to the) data is not deleted. The user of this class is
  responible for resource management.
*/
MemoryTableData::~MemoryTableData()
{
  // No, the owner of this object should call clear.
  // clear();
}



/* NOT IMPLEMENTED
//! Assignment operator.
MemoryTableData& MemoryTableData::operator=(
         MemoryTableData const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void MemoryTableData::initialiseValues(
         std::vector<std::any>& values)
{
  assert(values.empty());
  values.push_back(std::vector<std::any>());
}



void MemoryTableData::initialiseValues(
         std::vector<std::any>& values,
         DataSpace space)
{
  // If space has no dimensions any more, this is the level where the actual
  // data is stored and we reserve 1 place.
  // If space has one dimension left, it is level where the tables can be
  // stored. We reserve one values vector per coordinate.
  // If space has more than one dimension left, we need to enlarge the tree of
  // values for another dimension.
  if(space.isEmpty()) {
    initialiseValues(values);
  }
  else {
    Dimension const dimension(space.dimension(0));
    space.eraseDimension(0);

    // Length of values corresponds with the nrCoordinates of the
    // enclosing dimension. We need to add room for coordinates of the current
    // dimension.
    switch(dimension.meaning()) {
      case Scenarios:
      case CumulativeProbabilities:
      case Samples:
      case Time:
      case Space: {
        values.resize(dimension.nrCoordinates(), std::vector<std::any>());

        for(auto & value : values) {
          initialiseValues(
             std::any_cast<std::vector<std::any>&>(value), space);
        }

        break;
      }
      default: {
        assert(false);
        break;
      }
    }
  }
}



// void MemoryTableData::copy(
//          std::vector<boost::any> const& sourceValues,
//          DataSpace space,
//          std::vector<boost::any>& destinationValues)
// {
//   assert(destinationValues.empty());
//
//
// }



void MemoryTableData::add(
         Table* table,
         std::vector<std::any>& values)
{
  assert(values.size() == 1);
  values[0] = table;
}



void MemoryTableData::add(
         Table* table,
         DataSpace space,
         DataSpaceAddress address,
         std::vector<std::any>& values)
{
  if(space.isEmpty()) {
    add(table, values);
  }
  else {
    Dimension const dimension(space.dimension(0));
    size_t index = values.size();

    switch(dimension.meaning()) {
      case Scenarios: {
        index = dimension.indexOf(address.coordinate<std::string>(0));
        break;
      }
      case CumulativeProbabilities: {
        index = dimension.indexOf(address.coordinate<float>(0));
        break;
      }
      case Samples:
      case Time:
      case Space: {
        index = dimension.indexOf(address.coordinate<size_t>(0));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    space.eraseDimension(0);
    address.eraseCoordinate(0);

    add(table, space, address,
       std::any_cast<std::vector<std::any>&>(values[index]));
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   \a address must be contained by the layered data space.
             This function assumes space is already allocated for this table.
  \sa        .

*/
void MemoryTableData::add(
         Table* table,
         DataSpaceAddress const& address)
{
  assert(d_dataSpace.contains(address));
  add(table, d_dataSpace, address, d_values);
}



void MemoryTableData::clear(
         std::vector<std::any>& values)
{
  assert(values.size() == 1);

  if(exists(values)) {
    delete std::any_cast<Table*>(values[0]);
    values.clear();
  }
}



void MemoryTableData::clear(
         std::vector<std::any>& values,
         DataSpace space)
{
  if(space.isEmpty()) {
    clear(values);
  }
  else {
    Dimension const dimension(space.dimension(0));
    space.eraseDimension(0);

    for(size_t i = 0; i < dimension.nrCoordinates(); ++i) {
      clear(std::any_cast<std::vector<std::any>&>(values[i]), space);
    }
  }
}



//! Clears / deletes the layered data objects from memory by calling delete.
/*!
  \warning   This function assumes all layered data objects are allocated using the new operator. If this is not the case, don't call this function.
*/
void MemoryTableData::clear()
{
  clear(d_values, d_dataSpace);
}



//! Returns the data space the layered data objects are located in.
/*!
  \return    data space
*/
DataSpace const& MemoryTableData::dataSpace() const
{
  return d_dataSpace;
}



bool MemoryTableData::exists(
         std::vector<std::any> const& values) const
{
  assert(values.size() == 1);
  return std::any_cast<Table*>(&values[0]) != nullptr;
}



bool MemoryTableData::exists(
         DataSpace space,
         DataSpaceAddress address,
         std::vector<std::any> const& values) const
{
  bool result = false;

  if(space.isEmpty()) {
    result = exists(values);
  }
  else {
    Dimension const& dimension(space.dimension(0));
    size_t index = values.size();

    switch(dimension.meaning()) {
      case Scenarios: {
        index = dimension.indexOf(address.coordinate<std::string>(0));
        break;
      }
      case CumulativeProbabilities: {
        index = dimension.indexOf(address.coordinate<float>(0));
        break;
      }
      case Samples:
      case Time:
      case Space: {
        index = dimension.indexOf(address.coordinate<size_t>(0));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    space.eraseDimension(0);
    address.eraseCoordinate(0);

    result = exists(space, address,
         std::any_cast<std::vector<std::any> const&>(values[index]));
  }

  return result;
}



//! Returns whether a data object is stored at \a address.
/*!
  \param     address Address to look at for data.
  \return    true or false
*/
bool MemoryTableData::exists(
         DataSpaceAddress const& address) const
{
  return exists(d_dataSpace, address, d_values);
}



Table const* MemoryTableData::table(
         std::vector<std::any>& values)
{
  assert(values.size() == 1);

  Table const* result = nullptr;

  if(exists(values)) {
    result = *std::any_cast<Table*>(&values[0]);
  }

  return result;
}



Table const* MemoryTableData::table(
         std::vector<std::any>& values,
         DataSpace space,
         DataSpaceAddress address)
{
  assert(space.contains(address));

  Table const* result = nullptr;

  if(space.isEmpty()) {
    result = table(values);
  }
  else {
    Dimension const dimension(space.dimension(0));
    size_t index = values.size();

    switch(dimension.meaning()) {
      case Scenarios: {
        index = dimension.indexOf(address.coordinate<std::string>(0));
        break;
      }
      case CumulativeProbabilities: {
        index = dimension.indexOf(address.coordinate<float>(0));
        break;
      }
      case Samples:
      case Time:
      case Space: {
        index = dimension.indexOf(address.coordinate<size_t>(0));
        break;
      }
      default: {
        assert(false);
        break;
      }
    }

    space.eraseDimension(0);
    address.eraseCoordinate(0);

    result = table(std::any_cast<std::vector<std::any>&>(values[index]),
         space, address);
  }

  return result;
}



//! Returns the table stored at \a address.
/*!
  \param     address Address to look at for data.
  \return    Stored pointer or 0 if not present.
*/
Table const* MemoryTableData::table(
         DataSpaceAddress const& address)
{
  return table(d_values, d_dataSpace, address);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

