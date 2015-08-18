#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TIMER
#include "calc_timer.h"
#define INCLUDED_CALC_TIMER
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Timer class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class TimerPrivate
{
public:

  TimerPrivate()
  {
  }

  ~TimerPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TIMER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TIMER MEMBERS
//------------------------------------------------------------------------------

//! ctor, as a static timer
calc::Timer::Timer():
  d_currentInt(0),
  d_startInt(0),
  d_lastInt(0)
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
calc::Timer::Timer(Timer const& rhs)

  : Base(rhs)

{
}
*/



calc::Timer::~Timer()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
calc::Timer& calc::Timer::operator=(Timer const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! set value of d_currentInt
void calc::Timer::setCurrentInt(size_t currentInt)
{
  d_currentInt=currentInt;
}

//! set value of d_startInt
void calc::Timer::setStartInt(size_t startInt)
{
  d_startInt=startInt;
}

//! set value of d_lastInt
void calc::Timer::setLastInt(size_t lastInt)
{
  d_lastInt=lastInt;
}

//! get value of d_currentInt
size_t calc::Timer::currentInt() const
{
  return d_currentInt;
}

//! get value of d_startInt
size_t calc::Timer::startInt() const
{
  return d_startInt;
}

//! get value of d_lastInt
size_t calc::Timer::lastInt() const
{
  return d_lastInt;
}

bool calc::Timer::dynamic() const
{
  return d_lastInt != 0;
}

void  calc::Timer::increment()
{
  if (d_currentInt)
    ++d_currentInt;
  else {
    d_currentInt=d_startInt;
  }
}


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



