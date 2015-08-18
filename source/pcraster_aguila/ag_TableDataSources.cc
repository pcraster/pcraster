#include "ag_TableDataSources.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the TableDataSources class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class TableDataSourcesPrivate
{
public:

  TableDataSourcesPrivate()
  {
  }

  ~TableDataSourcesPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TABLEDATASOURCES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TABLEDATASOURCES MEMBERS
//------------------------------------------------------------------------------

ag::TableDataSources::TableDataSources()

  : DataObjectBase<Table>(geo::TIMESERIES)

{
}



ag::TableDataSources::~TableDataSources()
{
}



ag::DataInfo<ag::Table> ag::TableDataSources::openData(
         std::string const &name,
         dal::DataSpace const &space) const
{
  Table* table(new Table(name, space));

  return DataInfo<Table>(table, table->valueScale(), table->dataSpace());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



