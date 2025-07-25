#include "stddefx.h"
#include "calc_reporttable.h"
#include "calc_report.h"
#include "calc_id.h"

#include <sstream>

/*!
  \file
  This file contains the implementation of the ReportTable class.
*/

//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPORTTABLE MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPORTTABLE MEMBERS
//------------------------------------------------------------------------------

calc::ReportTable::ReportTable():
  d_reportDefault(Report::reportDefault())
  
{
}

calc::ReportTable::~ReportTable()
{
  for(auto & i : d_table)
    delete i.second;
}

//! add a report, error if already defined
/*
   \param r to add, deleted in case of error
 */
void calc::ReportTable::add(const Report& r)
{
  auto *rs=new Report(r);
  if (d_timer.lastInt() > 0)
    rs->update(d_timer);

  // first insert with 0
  std::pair<Table::iterator,bool> p=
    d_table.insert(std::make_pair(r.name(),rs));

  // first definition if error, or (new) position
  auto fd =p.first;
  if (!p.second) { // pcrcalc/test238
      delete rs;
      std::ostringstream msg;
      msg << r.qName() << " is used twice as report name, first use at "
          << fd->second->shortPosText();
      r.posError(msg);
  }
  if (r.name() == "reportdefault")
    d_reportDefault=r;
}

//! find report by name
/*!
 * \param   name if name is reportdefault then reportDefault() is returned
 * \throws  name.posError if name is not a report name
 */
const calc::Report* calc::ReportTable::find(const calc::Id& name) const
{
   PRECOND(!name.empty());
   if (name() == "reportdefault")
     return reportDefault();

   auto p=d_table.find(name());
   if (p == d_table.end()) // pcrcalc/test237
        name.posError(name.qName()+" is not a report name");
   return p->second;
}

/*! Note that the correct report default contents is only known after all
 *  report definitions have been added, the address of reportDefault is
 *  however fixed
 */
const calc::Report* calc::ReportTable::reportDefault() const
{
    return &d_reportDefault;
}

//! record if one or more report statements are parsed in input
void calc::ReportTable::setReportFound(bool reportFound)
{
  d_reportFound=reportFound;
}

//! get value of reportFound
bool calc::ReportTable::reportFound() const
{
  return d_reportFound;
}


//! end time > 0
void calc::ReportTable::update(const Timer& timer)
{
  PRECOND(timer.lastInt() > 0);
  d_timer=timer;
  d_reportDefault.update(d_timer);
  for(auto & i : d_table)
    i.second->update(d_timer);
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



