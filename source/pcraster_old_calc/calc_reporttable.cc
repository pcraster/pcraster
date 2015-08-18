#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif


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

calc::ReportTable::ReportTable()
{
  d_reportDefault = new ReportDefault();
}

calc::ReportTable::~ReportTable()
{
    delete d_reportDefault;
    for(Table::iterator i=d_table.begin(); i != d_table.end(); i++)
         delete i->second;
}

//! add a report, error if already defined
/*
   \param r to add, deleted in case of error
 */
void calc::ReportTable::insert(const ReportDefinition *r)
{
  try {
   std::pair<Table::iterator,bool> p=d_table.insert(std::make_pair(r->name(),r));
   Table::const_iterator fd =p.first; // first definition if error, or (new) position
   if (!p.second) { // pcrcalc/test238
       std::ostringstream msg;
       msg << r->qName() << " is used twice as report name, first use at "
           << fd->second->definitionPoint();
       r->posError(msg);
   }
   if (r->name() == "reportdefault")
     d_reportDefault->setDefinition(r);
  } catch (...) {
    delete r;
    throw;
  }
}

const calc::Report *calc::ReportTable::find(const calc::Symbol& u) const
{
   if (u.name() == "reportdefault")
     return reportDefault();

   Table::const_iterator p=d_table.find(u.name());
   if (p == d_table.end()) // pcrcalc/test237
        u.posError(u.qName()+" is not a report name");
   return p->second;
}

const calc::Report *calc::ReportTable::reportDefault() const
{
    return d_reportDefault;
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



