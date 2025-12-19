#ifndef INCLUDED_CALC_STACKINFO
#define INCLUDED_CALC_STACKINFO

#include "stddefx.h"
#include "calc_types.h"
#include "calc_gridstat.h"

#include <string>


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
  const Report*    d_report{};

  //! value scale of stack
  VS               d_vs;

  //! name of stack
  std::string      d_stackName;

  //! -1 option
  bool             d_flushTssAtEachTimeStep{false};

  const Report* report              () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  StackInfo&       operator=               (const StackInfo& rhs);

                   StackInfo               (const StackInfo& rhs);

                   StackInfo               ();

  /* virtual */    ~StackInfo              () override;

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
