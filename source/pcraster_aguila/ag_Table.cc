#include "ag_Table.h"

// Library headers.
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <boost/lexical_cast.hpp>

// PCRaster library headers.
#include "dal_DataSource.h"
#include "dal_DataSpaceAddressMapper.h"
#include "dal_StepCoordinateMapper.h"
#include "dal_Table.h"
#include "dal_TableDriver.h"
#include "com_exception.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the Table class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TABLE MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Add code to temporarely cache tables. Currently, tables with n
             attribute columns are read n times. Better is to read the whole
             table once, store it in the cache and let subsequent table reads
             pick up the cached table from the library::cacheDataPool() using
             the dal::MemoryTableDriver.
*/
Table::Table(
         std::string const& name,
         dal::DataSpace const& space)

  : Dataset(name, space),
    d_table(0), d_valueScale(VS_NOTDETERMINED)

{
  dal::TableDriver* driver =
         dynamic_cast<dal::TableDriver*>(dataSource().reader());
  assert(driver);

  std::auto_ptr<dal::Table> table(dataSource().open<dal::Table>());
  assert(table.get());


  size_t selectedTimeCol = table->nrCols();
  size_t selectedAttrCol = table->nrCols();
  {
      boost::tuple<std::string, std::vector<std::string> > tuple =
             dal::splitNameAndSelection(this->name());

      if(!boost::get<1>(tuple).empty()) {
          if(boost::get<1>(tuple).size() != 2) {
              std::string message = (boost::format(
                   "Selection specification of %1% (%2%): "
                   "Must contain two indices.\n")
                   % this->name()
                   % dal::dataSpaceToString(space)).str();
              throw com::Exception(message);
          }
          else {
              selectedTimeCol = boost::lexical_cast<size_t>(
                  boost::get<1>(tuple)[0]);
              selectedAttrCol = boost::lexical_cast<size_t>(
                  boost::get<1>(tuple)[1]);

              if(selectedTimeCol == 0 || selectedTimeCol > table->nrCols() ||
                      selectedAttrCol == 0 ||
                      selectedAttrCol > table->nrCols()) {
                  std::string message = (boost::format(
                       "Selection specification of %1% (%2%): "
                       "Indices must be in range [1, %3%].\n")
                       % this->name()
                       % dal::dataSpaceToString(space)
                       % table->nrCols()
                  ).str();
                  throw com::Exception(message);
              }

              --selectedTimeCol;
              --selectedAttrCol;
          }
      }
  }


  if(selectedTimeCol < table->nrCols()) {
      d_timeCol = selectedTimeCol;
  }
  else {
      d_timeCol = table->indexOf("date");
      d_timeCol = d_timeCol < table->nrCols() ? d_timeCol : 0;
  }
  assert(d_timeCol < table->nrCols());

  if(selectedAttrCol < table->nrCols()) {
      d_attrCol = selectedAttrCol;
  }
  else {
      d_attrCol = table->indexOf(
          boost::filesystem::path(name).filename().string());
      d_attrCol = d_attrCol < table->nrCols() ? d_attrCol : 1;
  }
  assert(d_attrCol < table->nrCols());

  assert(d_attrCol != d_timeCol);

  if(d_attrCol == d_timeCol) {
      std::string message = (boost::format(
           "Attribute column %1% from %2% (%3%): Same as time column.\n"
           "Use a different column as attribute column")
           % (d_attrCol + 1)
           % this->name()
           % dal::dataSpaceToString(space)).str();
      throw com::Exception(message);
  }


  // Besides UINT1, INT4 should work too.
  // if(!dal::isUnsignedInteger(table->typeId(d_timeCol))) {
  //        "Valid types are unsigned integral types")
  if(!dal::isInteger(table->typeId(d_timeCol))) {
    std::string message = (boost::format(
         "Time column %1% from %2% (%3%): Values have type %4%.\n"
         "Valid types are integer types")
         % (d_timeCol + 1)
         % this->name()
         % dal::dataSpaceToString(space)
         % dal::typeIdToString(table->typeId(d_timeCol))).str();
    throw com::Exception(message);
  }

  if(!dal::isNumeric(table->typeId(d_attrCol))) {
    std::string message = (boost::format(
         "Attribute column %1% from %2% (%3%): Values have type %4%.\n"
         "Valid types are numerical types")
         % (d_attrCol + 1)
         % this->name()
         % dal::dataSpaceToString(space)
         % dal::typeIdToString(table->typeId(d_attrCol))).str();
    throw com::Exception(message);
  }

  table->setTypeId(d_timeCol, dal::TI_UINT4);
  table->setTypeId(d_attrCol, dal::TI_REAL4);

  d_valueScale = VS_SCALAR;
  d_table = table.release();
  d_table->createCols();

  boost::any min, max;
  if(driver->extremes(min, max, d_attrCol, d_table->typeId(d_attrCol),
         dataSource().name(), dataSource().enclosingDataSpace())) {
    setExtremes(min, max);
  }
}



Table::~Table()
{
  delete d_table;
}



size_t Table::nrCols() const
{
  return d_table->nrCols();
}



size_t Table::nrRecs() const
{
  return d_table->nrRecs();
}



void Table::read(
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address)
{
  assert(d_table);

  dal::DataSpaceAddress localAddress(this->localAddress(space, address));

  assert(dataSource().dataSpace().rank() == localAddress.size());

  // After translation to local addresses we might end up with invalid
  // coordinates.
  // TODO check why contains below seem to return true on an invalid address.
  for(size_t i = 0; i < dataSource().dataSpace().rank(); ++i) {
    if(dataSource().dataSpace().dimension(i).meaning() != dal::Time &&
         !localAddress.isValid(i)) {
      setAddressRead(dataSource().dataSpace().address());
      assert(!isRead());
      assert(!isRead(localAddress));
      assert(!isRead(addressRead()));
      return;
    }
  }

  if(!isRead(localAddress)) {
    if(!dataSource().dataSpace().contains(localAddress)) {
      setAddressRead(dataSource().dataSpace().address());
      assert(!isRead());
      assert(!isRead(localAddress));
      assert(!isRead(addressRead()));
    }
    else {
      dataSource().read(*d_table, localAddress);

      // Translate time steps in local coordinates to global coordinates.
      size_t id = dataSource().dataSpace().indexOf(dal::Time);
      dal::StepCoordinateMapper const* mapper =
         dynamic_cast<dal::StepCoordinateMapper const*>(
         globalToLocalMapper().mapper(id));

      if(mapper) {
        double timeStepOffset = mapper->source(1.0) - 1.0;
        assert(dal::greaterOrComparable(timeStepOffset, 0.0));
        assert(dal::comparable(std::fmod(timeStepOffset, 1.0), 0.0));

        // Transform time steps from local coordinates to global coordinates.
        // TODO this is naive, map each coordinate using the mapper.
        dal::Array<UINT4>& col(d_table->col<UINT4>(timeCol()));
        std::transform(col.begin(), col.end(), col.begin(),
           boost::bind(std::plus<UINT4>(), _1,
           static_cast<UINT4>(timeStepOffset)));
      }

      setAddressRead(localAddress);
      assert(isRead(localAddress));
      assert(isRead(addressRead()));
    }
  }
}



CSF_VS Table::valueScale() const
{
  assert(d_valueScale != VS_NOTDETERMINED);
  return d_valueScale;
}



bool Table::isRead() const
{
  bool result = false;

  // if(addressRead().size() == 0) {
  //   result = dataSource().dataSpace().size() == 0;
  // }
  // else {
  if(addressRead().size() > 0) {
    result = true;
    /*
    dal::DataSpaceAddress address(dataSource().enclosingDataSpace().trim(
         dataSource().dataSpace(), addressRead()));

    for(size_t i = 0; i < addressRead.size(); ++i) {
      if(!addressRead.isValid(i)) {
        result = false;
        break;
      }
    }
    */
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
bool Table::isRead(
         dal::DataSpaceAddress const& address) const
{
  bool result = false;

  if(isRead()) {
    // In case of time series data we need to read in new data if a coordinate
    // changes on one of the dimensions, EXCEPT for the time dimension. This
    // is because one read of a time series file reads in all time steps
    // available.
    dal::DataSpace const& space(dataSource().dataSpace());
    dal::DataSpace const& enclosingSpace(dataSource().enclosingDataSpace());

    result = enclosingSpace.equal(
         enclosingSpace.trim(space, addressRead()),
         enclosingSpace.trim(space, address));
  }

  return result;
}



size_t Table::timeCol() const
{
  return d_timeCol;
}



size_t Table::attrCol() const
{
  return d_attrCol;
}



template<>
bool Table::value<std::string>(
         std::string& result,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address) const
{
  dal::DataSpaceAddress trimmedAddress = dataSource().dataSpace().trim(
         space, address);
  result = "TODO";
  // dataSource().read(result, trimmedAddress);

  return result != "mv";
}



dal::Table const& Table::table() const
{
  assert(d_table);
  return *d_table;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

