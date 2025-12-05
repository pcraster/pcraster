#ifndef INCLUDED_OLDCALC_TIMERVALUE
#define INCLUDED_OLDCALC_TIMERVALUE

#include "stddefx.h"
#include "calc_symbol.h"
#include "calc_vs.h"



namespace calc {
  // TimerValue declarations.
}

namespace calc {


//! timer section items
class TimerValue : public Symbol {
private:

  VS     d_vs;
  double d_value{};

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  //! Copy constructor.
                   TimerValue               (const TimerValue&);

                   TimerValue               (const Symbol&   s);

                   TimerValue               ();

  /* virtual */    ~TimerValue              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  TimerValue&           operator=           (const TimerValue&) = default;

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t value() const;
  size_t check(const std::string& name) const;

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

void checkTimerSection(
    const TimerValue& start,
    const TimerValue& end,
    const TimerValue& slice);



} // namespace calc

#endif
