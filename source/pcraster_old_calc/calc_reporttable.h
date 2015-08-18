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



namespace calc {
  // ReportTable declarations.
}



namespace calc {

class Symbol;
class Report;
class ReportDefinition;
class ReportDefault;


//! table of report definitions, manage the reportdefault statement
class ReportTable
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  ReportTable&           operator=           (const ReportTable&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ReportTable               (const ReportTable&);

  typedef        std::map<std::string,const ReportDefinition *> Table;
  Table          d_table;
  ReportDefault *d_reportDefault;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ReportTable               ();

  /* virtual */    ~ReportTable              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              insert                   (const ReportDefinition *r);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const Report*     find                     (const Symbol& s) const;
  const Report*     reportDefault            () const;

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
