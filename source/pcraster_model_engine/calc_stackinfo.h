#ifndef INCLUDED_CALC_STACKINFO
#define INCLUDED_CALC_STACKINFO



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif


// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif
#ifndef INCLUDED_CALC_GRIDSTAT
#include "calc_gridstat.h"
#define INCLUDED_CALC_GRIDSTAT
#endif

namespace calc {
  // StackInfo declarations.
}



namespace calc {


class Report;

//! data collection and information for dynamic data written
class StackInfo : public GridStat
{

private:

  //! never 0!
  const Report*    d_report;

  //! value scale of stack
  VS               d_vs;

  //! name of stack
  std::string      d_stackName;

  //! -1 option
  bool             d_flushTssAtEachTimeStep;

  const Report* report              () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  StackInfo&       operator=               (const StackInfo& rhs);

                   StackInfo               (const StackInfo& rhs);

                   StackInfo               ();

  /* virtual */    ~StackInfo              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             setReport           (const Report* report);
  void             setVs               (const VS vs);
  void             setStackName        (const std::string& stackName);
  void             setFlushTssAtEachTimeStep(bool flushTssAtEachTimeStep);


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  VS    vs                          () const;
  const std::string& stackName      () const;
  bool  flushTssAtEachTimeStep      () const;

  bool             reportTimeStep   (size_t timeStep) const;
  size_t           lastInt          () const;
  size_t           startInt         () const;

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
