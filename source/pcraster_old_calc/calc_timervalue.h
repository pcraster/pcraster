#ifndef INCLUDED_CALC_TIMERVALUE
#define INCLUDED_CALC_TIMERVALUE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"
#define INCLUDED_CALC_VS
#endif


namespace calc {
  // TimerValue declarations.
}

namespace calc {


//! timer section items
class TimerValue : public Symbol {
private:

  //! Assignment operator. DEFAULT
  //   TimerValue&           operator=           (const TimerValue&);


  VS     d_vs;
  double d_value;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------
  
  //! Copy constructor.
                   TimerValue               (const TimerValue&);

                   TimerValue               (const Symbol&   s);

                   TimerValue               ();

  /* virtual */    ~TimerValue              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

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
