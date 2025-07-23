#include "stddefx.h"
#include "calc_gridstat.h"
#include "com_math.h"



/*!
  \file
  This file contains the implementation of the GridStat class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class GridStatPrivate
{
public:

  GridStatPrivate()
  {
  }

  ~GridStatPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GRIDSTAT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GRIDSTAT MEMBERS
//------------------------------------------------------------------------------

calc::GridStat::GridStat()
  
{
}



calc::GridStat::~GridStat()
{
}

/* NOT IMPLEMENTED
//! Assignment operator.
calc::GridStat& calc::GridStat::operator=(const GridStat& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}

//! Copy constructor. NOT IMPLEMENTED.
calc::GridStat::GridStat(const GridStat& rhs):
  Base(rhs)
{
}
*/

void calc::GridStat::merge(const GridStat& s)
{
  if (!s.d_minMaxSet)
    return;

  if (!d_minMaxSet) {
    d_minMaxSet=true;
    d_min=s.d_min;
    d_max=s.d_max;
  }
  else {
    com::minimize(d_min,s.d_min);
    com::maximize(d_max,s.d_max);
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



