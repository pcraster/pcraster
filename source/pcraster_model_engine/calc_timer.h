#ifndef INCLUDED_CALC_TIMER
#define INCLUDED_CALC_TIMER

#include "stddefx.h"



namespace calc {
  // Timer declarations.
}



namespace calc {



//! The model timer
/*!
 * Each model has a timer, even static models. It keeps track of the number
 * of timesteps set, the number of timesteps to be done, etc.
 *
 * Currently only the timesteps as series of integers 0,d_startInt to
 * d_lastInt is supported. 0 means the initial section. Incrementing
 * from the initial will set d_currentInt to d_startInt
 */
class Timer
{

private:

  //! 0 for initial section, > 0 for dynamic section
  size_t           d_currentInt{0};
  //! the firststep to set of a dynamic model
  size_t           d_startInt{0};
  //! the last step to set, 0 means not a dynamic timer
  size_t           d_lastInt{0};

  //  Assignment operator. DEFAULT
  // Timer&           operator=           (Timer const& rhs);

  //  Copy constructor. DEFAULT
  //               Timer               (Timer const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Timer               ();

  /* virtual */    ~Timer              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void             setCurrentInt       (size_t currentInt);
  void             setStartInt         (size_t startInt);
  void             setLastInt          (size_t lastInt);

  void             increment           ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  size_t           currentInt          () const;
  size_t           startInt            () const;
  size_t           lastInt             () const;

  bool             dynamic             () const;


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
