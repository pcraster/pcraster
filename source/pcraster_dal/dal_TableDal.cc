#include "dal_TableDal.h"
#include "dal_GeoEASTableDriver.h"
#include "dal_SQLTableDriver.h"
#include "dal_TextTableDriver.h"
#include "dal_Utils.h"

#include <QStringList>
#include <QSqlDatabase>


/*!
  \file
  This file contains the implementation of the TableDal class.
*/


namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEDAL MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TABLEDAL MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     addAllDrivers Whether or not to add all available drivers automatically.
*/
TableDal::TableDal(bool addAllDrivers)

  : Dal(false)

{
  if(addAllDrivers) {
    #include "autoAddTableDrivers.cc"
  }
}



//! Destructor.
/*!
*/
TableDal::~TableDal()
{
}



//! Opens the tabular dataset pointed to by name.
/*!
  \param     name Name of table dataset.
  \return    A pointer to a newly created Table object or 0 if no driver could
             open \a name.

  The caller is responsible of deleting the Table object again.
*/
std::tuple<std::shared_ptr<Table>, TableDriver*> TableDal::open(
         std::string const& name,
         bool raiseException)
{
  assert(nrDrivers() > 0);
  std::shared_ptr<Dataset> dataset;
  Driver* driver = nullptr;
  std::tie(dataset, driver) = Dal::open(name, TABLE);

  // Table* table = dynamic_cast<Table*>(Dal::open(name, TABLE));

  if(!dataset && raiseException) {
    throwCannotBeOpened(name, TABLE);
  }

  return std::make_tuple(std::dynamic_pointer_cast<Table>(dataset),
      dynamic_cast<TableDriver*>(driver));
}



//! Reads the tabular dataset pointed to by \a name.
/*!
  \param     name Name of table dataset.
  \param     typeId Type id of values to use.
  \return    A pointer to a newly created Table object.
  \exception Exception If no driver could read \a name.

  The caller is responsible of deleting the Table object again.
*/
Table* TableDal::read(std::string const& name, TypeId typeId)
{
  assert(nrDrivers() > 0);

  TableDriver* driver = nullptr;
  std::tie(std::ignore, driver) = open(name, true);
  assert(driver);

  return driver->read(name, typeId);
}



//! Reads the tabular dataset pointed to by \a name into \a table.
/*!
  \param     name Name of table dataset.
  \param     table Table object to fill.
  \exception Exception If no driver could read \a name.
  \warning   The configuration of \a table (nrRows, nrCols, typeId of the
             columns) must be in sync with the information in \a name.
  \sa        TableDriver::read(std::string const&, Table&)
*/
void TableDal::read(
         std::string const& name,
         Table& table)
{
  assert(nrDrivers() > 0);

  TableDriver* driver = nullptr;
  std::tie(std::ignore, driver) = open(name, true);
  assert(driver);

  driver->read(table, name);
}



TableDriver* TableDal::driverByDataset(
         std::string const& name)
{
  return dynamic_cast<TableDriver*>(Dal::driverByDataset(name, DataSpace()));
}



TableDriver* TableDal::driverByName(
         std::string const& name)
{
  return dynamic_cast<TableDriver*>(Dal::driverByName(name));
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal

