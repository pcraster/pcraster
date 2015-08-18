#include "stddefx.h" 

#ifndef INCLUDED_CALC_CONSTANTNUMBERTYPE
#include "calc_constantnumbertype.h"
#define INCLUDED_CALC_CONSTANTNUMBERTYPE
#endif

#ifndef INCLUDED_CALC_VS
#include "calc_vs.h"   // intersect
#define INCLUDED_CALC_VS
#endif


#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

static VS TypeNumber(double value)
{
  int set = (int)VS_FIELD;
  if ( value >= (double)LONG_MIN && value <= (double)LONG_MAX )
  {
    long i = (long)value;
    if ( value == ((double)i) )
    {
           /* if the value is not equal to 0 or 1 
        then it's not a boolean */
           if (value != 0 && value != 1)
      set &= ~VS_B; /* mask VS_B out of set */
           /* same sort of check for ldd */
           if (value < 1 || value > 9 )
      set &= ~VS_L; /* idem */
    } else {
    /* it's a real: mask classifieds out */
    set &= ~(VS_N|VS_O|VS_B|VS_L);
    }
  } else {
    /* even if it is an integer we cannot store it in a real: mask classifieds out */
    set &= ~(VS_N|VS_O|VS_B|VS_L);
  }
  VS setTypes = (VS)set;
  return setTypes;
}



calc::ConstantNumberType::ConstantNumberType(double value, VS restrictBy):
  calc::FieldType(intersect(TypeNumber(value),restrictBy),ST_NONSPATIAL)
{
}
