#ifndef INCLUDED_DAL_MEMORYTABLEDATA
#include "dal_MemoryTableData.h"
#define INCLUDED_DAL_MEMORYTABLEDATA
#endif

// Library headers.
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



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
         std::vector<boost::any>& values)
{
  assert(values.empty());
  values.push_back(std::vector<boost::any>());
}



void MemoryTableData::initialiseValues(
         std::vector<boost::any>& values,
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
    Dimension dimension(space.dimension(0));
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
        values.resize(dimension.nrCoordinates(), std::vector<boost::any>());

        for(size_t i = 0; i < values.size(); ++i) {
          initialiseValues(
             boost::any_cast<std::vector<boost::any>&>(values[i]), space);
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
         std::vector<boost::any>& values)
{
  assert(values.size() == 1);
  values[0] = table;
}



void MemoryTableData::add(
         Table* table,
         DataSpace space,
         DataSpaceAddress address,
         std::vector<boost::any>& values)
{
  if(space.isEmpty()) {
    add(table, values);
  }
  else {
    Dimension dimension(space.dimension(0));
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
       boost::any_cast<std::vector<boost::any>&>(values[index]));
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
         std::vector<boost::any>& values)
{
  assert(values.size() == 1);

  if(exists(values)) {
    delete boost::any_cast<Table*>(values[0]);
    values.clear();
  }
}



void MemoryTableData::clear(
         std::vector<boost::any>& values,
         DataSpace space)
{
  if(space.isEmpty()) {
    clear(values);
  }
  else {
    Dimension dimension(space.dimension(0));
    space.eraseDimension(0);

    for(size_t i = 0; i < dimension.nrCoordinates(); ++i) {
      clear(boost::any_cast<std::vector<boost::any>&>(values[i]), space);
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
         std::vector<boost::any> const& values) const
{
  assert(values.size() == 1);
  return boost::any_cast<Table*>(&values[0]) != 0;
}



bool MemoryTableData::exists(
         DataSpace space,
         DataSpaceAddress address,
         std::vector<boost::any> const& values) const
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
         boost::any_cast<std::vector<boost::any> const&>(values[index]));
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
         std::vector<boost::any>& values)
{
  assert(values.size() == 1);

  Table const* result = 0;

  if(exists(values)) {
    result = *boost::any_cast<Table*>(&values[0]);
  }

  return result;
}



Table const* MemoryTableData::table(
         std::vector<boost::any>& values,
         DataSpace space,
         DataSpaceAddress address)
{
  assert(space.contains(address));

  Table const* result = 0;

  if(space.isEmpty()) {
    result = table(values);
  }
  else {
    Dimension dimension(space.dimension(0));
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

    result = table(boost::any_cast<std::vector<boost::any>&>(values[index]),
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

