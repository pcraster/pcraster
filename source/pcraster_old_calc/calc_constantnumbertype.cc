#include "stddefx.h"
#include "calc_constantnumbertype.h"
#include "calc_vs.h"  // intersect

#include <climits>
#include <sstream>

static VS TypeNumber(double value)
{
  int set = static_cast<int>(VS_FIELD);
  if (value >= static_cast<double>LONG_MIN && value <= static_cast<double>(LONG_MAX)) {
    long const i = static_cast<long>(value);
    if (value == (static_cast<double>(i))) {
      /* if the value is not equal to 0 or 1 
        then it's not a boolean */
      if (value != 0 && value != 1) {
        set &= ~VS_B; /* mask VS_B out of set */
      }
      /* same sort of check for ldd */
      if (value < 1 || value > 9) {
        set &= ~VS_L; /* idem */
      }
    } else {
      /* it's a real: mask classifieds out */
      set &= ~(VS_N | VS_O | VS_B | VS_L);
    }
  } else {
    /* even if it is an integer we cannot store it in a real: mask classifieds out */
    set &= ~(VS_N | VS_O | VS_B | VS_L);
  }
  VS const setTypes = static_cast<VS>(set);
  return setTypes;
}

calc::ConstantNumberType::ConstantNumberType(double value, VS restrictBy)
    : calc::FieldType(intersect(TypeNumber(value), restrictBy), ST_NONSPATIAL)
{
}
