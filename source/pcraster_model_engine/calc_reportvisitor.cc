#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_REPORTVISITOR
#include "calc_reportvisitor.h"
#define INCLUDED_CALC_REPORTVISITOR
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_ASTSYMBOLTABLE
#include "calc_astsymboltable.h"
#define INCLUDED_CALC_ASTSYMBOLTABLE
#endif
#ifndef INCLUDED_CALC_REPORTTABLE
#include "calc_reporttable.h"
#define INCLUDED_CALC_REPORTTABLE
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif

/*!
  \file
  This file contains the implementation of the ReportVisitor class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ReportVisitorPrivate
{
public:

  ReportVisitorPrivate()
  {
  }

  ~ReportVisitorPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC REPORTVISITOR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF REPORTVISITOR MEMBERS
//------------------------------------------------------------------------------

calc::ReportVisitor::ReportVisitor(
    bool               reportLastAssOfEverySymbol,
    ReportTable const& reports,
    Timer const&       timer):
  d_reports(reports),
  d_timer(timer),
  d_inDynamic(false),
  d_reportLastAssOfEverySymbol(reportLastAssOfEverySymbol),
  d_currentReport(0),
  d_currentStat(0)
{
}

calc::ReportVisitor::~ReportVisitor()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::ReportVisitor& calc::ReportVisitor::operator=(const ReportVisitor& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::ReportVisitor::ReportVisitor(const ReportVisitor& rhs):
  Base(rhs)
{
}
*/

//! a no-op not visiting the expressions
void calc::ReportVisitor::visitExpr(BaseExpr *)
{
}

void calc::ReportVisitor::visitStat(ASTStat *s)
{
  d_currentStat=s;
  d_currentReport=0;
  if (s->reportParsed()) {
    //  reportById && reportInSitu are mutually exclusive
    if (s->reportInSitu()) {
       // e.g. report(1,3,5)  a = .......;
       s->reportInSitu()->update(d_timer);
       d_currentReport=s->reportInSitu();
    } else {
       Id id(s->reportById());
       if (!id.empty()) {
         // e.g. report(oneThreeFive)  a = .......;
         d_currentReport=d_reports.find(s->reportById());
       } else {
         // e.g. report  a = .......;
         d_currentReport=d_reports.reportDefault();
       }
    }
  }

  ASTVisitor::visitStat(s);
}

void calc::ReportVisitor::visitAss(ASTAss *a)
{
  for(size_t i=0; i<a->nrPars(); ++i) {
    ASTPar *p(a->par(i));
    if (d_reportLastAssOfEverySymbol) {
      // always update
      updateReportPar(p);
    } else {
      // only update iff explicit report
      if (d_currentReport)
        updateReportPar(p);
    }
  }
}

void calc::ReportVisitor::enterDynamicSection(DynamicSection *)
{
  PRECOND(!d_inDynamic); // only 1 DynamicSection
  d_inDynamic=true;
}
void calc::ReportVisitor::jumpOutDynamicSection(DynamicSection *)
{
  PRECOND(d_inDynamic); // only 1 DynamicSection
  d_inDynamic=false;
}

void calc::ReportVisitor::updateReportPar(ASTPar const *p)
{
  if (!d_reportLastAssOfEverySymbol) {
    ReportPars::const_iterator f=d_reportPars.find(p->name());
    if (f != d_reportPars.end()) {
      // duplicate report, pcrcalc255
      p->symError("Report already done previous ("
                       +f->second.d_par->shortPosText()+")");
    }
  }
  ReportPar pp = { p,
     d_currentReport ? d_currentReport : d_reports.reportDefault(),
                   d_inDynamic };
  // always overwrite
  d_reportPars[p->name()]= pp;
}

calc::ReportPars const& calc::ReportVisitor::reportPars() const
{
  return d_reportPars;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



