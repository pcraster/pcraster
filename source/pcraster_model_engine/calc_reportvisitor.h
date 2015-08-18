#ifndef INCLUDED_CALC_REPORTVISITOR
#define INCLUDED_CALC_REPORTVISITOR



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
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
#ifndef INCLUDED_CALC_ASTVISITOR
#include "calc_astvisitor.h"
#define INCLUDED_CALC_ASTVISITOR
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif



namespace calc {
  // ReportVisitor declarations.
}



namespace calc {

class ReportTable;
class Report;
class Timer;

//! mapped value of ReportPars
/*!
 * ptrs are not owned.
 */
struct ReportPar {
  //! left par of ASTAss that is reported
  ASTPar const *d_par;
  //! the report for par, 0 if no explicit report clause
  Report const *d_report;
  //! is par part of the dynamic section?
  bool          d_inDynamic;
};

//! return value of ReportVisitor::reportPars()
typedef std::map<std::string,ReportPar>  ReportPars;


//! update all Report objects in AST and create a map of report positions
/*!
 * The AST is updated for all Report object's that are part of ASTStat:
 *  - the steps to write are updated on base of the timer argument.
 *
 * A check is done if explicit reports are not done more than once for
 * every symbol.
 *
 * After visit(), reportedPars will return the map of which parameters
 * are reported where.
 *
 */
class ReportVisitor : public ASTVisitor
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ReportVisitor&           operator=           (const ReportVisitor& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReportVisitor               (const ReportVisitor& rhs);

  //! the defined reports
  ReportTable const&     d_reports;


  //! the timer information
  Timer const&           d_timer;

  //! are we visiting within the dynamic section?
  bool             d_inDynamic;

  /*! report last assignment of each par assigned discarding the
   *   possible report keyword of the statement.
   *  - for simple scripts with no explicit report, dynamic or
   *     interface section
   *  - for the XML interface, reports are discarded
   */
  bool             d_reportLastAssOfEverySymbol;

  const Report    *d_currentReport;
  //! may be modified for fixing timeoutput
  ASTStat         *d_currentStat;

  ReportPars     d_reportPars;

  void             updateReportPar             (ASTPar const *p);
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ReportVisitor               (bool reportLastAssOfEverySymbol,
                                                ReportTable const& reports,
                                                Timer const& timer);

  /* virtual */    ~ReportVisitor              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void              visitExpr                   (BaseExpr *e);
  void              visitStat                   (ASTStat *s);
  void              visitAss                    (ASTAss  *s);

  void              enterDynamicSection         (DynamicSection *);
  void              jumpOutDynamicSection       (DynamicSection *);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  ReportPars const& reportPars                 () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
