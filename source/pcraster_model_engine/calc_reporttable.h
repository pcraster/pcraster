#ifndef INCLUDED_CALC_REPORTTABLE
#define INCLUDED_CALC_REPORTTABLE


#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif
#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif


namespace calc {
  // ReportTable declarations.
}



namespace calc {

class Id;


/*!
  3 functions <ol>
  <li> table of report definitions (below timer)</li>
  <li>manage the reportdefault statement</li>
  <li>records if report statements is parsed in input reportFound</li>
  </ol>
 */
class ReportTable
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ReportTable&           operator=           (const ReportTable&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReportTable               (const ReportTable&);

  /*! store as Report ptr since we do pass out ptrs and a map
   * is by value
   */
  typedef          std::map<std::string,Report *> Table;
  Table            d_table;
  Report           d_reportDefault;
  Timer            d_timer;

  bool             d_reportFound;


public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ReportTable               ();

  /* virtual */    ~ReportTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              add                      (const Report& r);
  void              setReportFound           (bool reportFound);
  void              update               (const Timer& timer);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Report*     find                     (const Id& name) const;
  const Report*     reportDefault            () const;
  bool              reportFound              () const;

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
