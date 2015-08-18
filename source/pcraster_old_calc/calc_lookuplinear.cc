#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LOOKUPLINEAR
#include "calc_lookuplinear.h"
#define INCLUDED_CALC_LOOKUPLINEAR
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_INTERVAL
#include "com_interval.h"
#define INCLUDED_COM_INTERVAL
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
// Module headers.



/*!
  \file
  This file contains the implementation of the LookupLinear class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class LookupLinearPrivate
{
public:

  LookupLinearPrivate()
  {
  }

  ~LookupLinearPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LOOKUPLINEAR MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LOOKUPLINEAR MEMBERS
//------------------------------------------------------------------------------

/*!
 * \todo
 *  <ul>
 *  <li> check correct order of table records
 *  <li> check that number of columns is exactly 1
 *  </ul>
 */
calc::LookupLinear::LookupLinear(
    VS outType):
  LookupTable(outType)
{
}



calc::LookupLinear::~LookupLinear()
{
}

bool calc::LookupLinear::find(double& result, const std::vector<double>& key) const
{
  PRECOND(key.size() == 1);
  PRECOND(nrKeys()   == 1);
  const Records& r(records());
  size_t lt(r.size());
  size_t gt(0);
  for (size_t i = 0; i < r.size(); i++) {
    // r is sorted from low to high
    switch(r[i].compare(key)) {
      case -1: // key is less
          // assign a lower one that is most close to key
          lt=i;
          break;
      case 0:
         result=r[i].result();
         return true;
      case 1:
        // assign the first greater one that is most close to key
         if (!gt)
            gt=i;
         break;
      default:
          DEVELOP_POSTCOND(false);
    }
  }
  if (lt < gt && gt < r.size()) {
    DEVELOP_PRECOND(r[lt].interval(0)->max() != com::Interval<double>::maxLimit());
    DEVELOP_PRECOND(r[gt].interval(0)->min() != com::Interval<double>::minLimit());
    // interpolate linear
    result = com::interpolate2(key[0],
               r[lt].interval(0)->max(),r[lt].result(),
               r[gt].interval(0)->min(),r[gt].result());
    return true;
  }
  return false;
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



