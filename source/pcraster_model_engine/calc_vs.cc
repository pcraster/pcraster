#include "stddefx.h"
#include "calc_vs.h"
#include "misc.h"
#include "calc_file.h"
#include "calc_exception.h"
#include "geo_csfmap.h"
#include "geo_exception.h"
#include "calc_map2csf.h"
#include <sstream>

/*! pick one value scale that is the
 * biggest from a set of value scales
 */
VS biggestVs(
  VS setOfVs) /* set of value scales */
{
  if(isIn(VS_S, setOfVs)) /* Scalar before direction, dir is special */
    return VS_S;
  if(isIn(VS_D, setOfVs))
    return VS_D;
  /* actually no diff between VS_O and VS_N */
  if(isIn(VS_O, setOfVs))
    return VS_O;
  if(isIn(VS_N, setOfVs))
    return VS_N;
  /* boolean before ldd, ldd is special */
  if(isIn(VS_B, setOfVs))
    return VS_B;
  if(isIn(VS_L, setOfVs))
    return VS_L;
  POSTCOND(false); // NEVER
  return VS_UNKNOWN;
}

std::ostream& operator<<(
  std::ostream& msg,
  VS   setOfVs)
{
    // PRECOND(setOfVs != VS_UNKNOWN);

    if (setOfVs == VS_FIELD) {
     msg << "map or nonspatial";
    return msg;
    }
    typedef struct VS_NAME {
     VS vs;
     const char *name;
    } VS_NAME;
    const VS_NAME names[]= {
    { VS_S,"scalar"},
    { VS_N,"nominal"},
    { VS_O,"ordinal"},
    { VS_B, "boolean"},
    { VS_L,"ldd"},
    { VS_D,"directional"},
    { VS_TABLE,"table"},
    { VS_TSS,"timeseries"},
    { VS_ARRAY,       "array"},
    { VS_INDEX,       "array-index"},
    { VS_STRING,      "string"},
    { VS_INDEXTABLE, "indextable"},
    { VS_INDEXSET,   "array-set"},
    { VS_OBJECT,     "object"},
    { VS_MAPSTACK,   "map-stack"},
    { VS_STATISTICS, "statistics"},
    { VS_NULL,       "Python None"}, // could also by 0 ptr in LinkOut/LinkIn api.
    { VS_UNKNOWN,    "unknown!"}
   };

  size_t const nrVs = ARRAY_SIZE(names);
  size_t thisSet[ARRAY_SIZE(names)];
  size_t i = 0;
  size_t thisSetNr=0;
  for(i=0; i<nrVs; i++)
    if (isIn(names[i].vs, setOfVs)) {
      thisSet[thisSetNr] = i;
      thisSetNr++;
    }

  if(!thisSetNr) {
    msg << "unknown!";
    return msg;
  }

  if (thisSetNr > 1)
    msg << "one of (";
  for (i = 0; i < thisSetNr-1; i++) {
     msg << names[thisSet[i]].name << ",";
  }
  msg << names[thisSet[i]].name;
  if (thisSetNr > 1) /* terminate set */
    msg << ")";
  return msg;
}

//! create string from a vs set
/*! layout of string is
      <ol>
       <li><b><i>name</i></b> in case the set size is 1</li>
       <li><b> one of ( <i>name-1</i>,...,<i>name-n</i>)</b>
         if the set size is greater than 1</li>
      </ol>
*/
std::string toString(VS setOfVs)
{
  std::ostringstream msg;
  msg << setOfVs;
  return msg.str();
}

# define IS_IN_MACRO(x, set)    ( ((int)(x)) & ((int)(set)) )
# define UNION_MACRO(set1, set2)  ( ((int)(set1)) | ((int)(set2)) )

bool isSubset(VS x, VS set)  { return ((VS)IS_IN_MACRO(x,set)) == x; }
bool isIn(VS x, VS set)      { return ((VS)IS_IN_MACRO(x,set)) != VS_UNKNOWN; }
VS   intersect(VS x, VS set) { return (VS)IS_IN_MACRO(x,set); }
VS   unionSet(VS set1, VS set2) { return (VS)UNION_MACRO(set1,set2); }
int  nrInSet(VS set) { return NRBITSET_TYPE(set,VS); }

//! return set of VS possible for this number
/*!
 * \bug Parser::sign_expr() does not support negative nominal or
 *      ordinals, shift and shift0 could be nominal otherwise
 */
PCR_ME_EXPORT VS vsOfNumber(double value)
{
  int set = (int)VS_FIELD;
  if ( value >= (double)LONG_MIN && value <= (double)LONG_MAX )
  {
    long const i = (long)value;
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
    /* even if it is an integer we cannot store it in a real:
       mask classifieds out
     */
    set &= ~(VS_N|VS_O|VS_B|VS_L);
  }
  return (VS)set;
}

void calc::checkOneVs(
    VS vsToCheck,
    const char* typeDescr)
{
   // worry on the correct type if written as output
   if (nrInSet(vsToCheck) != 1) {
     // pcrcalc38[a]
     // pcrcalc68
     std::ostringstream s;
     s << "Use a conversion function to pick "
       << typeDescr << "\npossible data type is " << vsToCheck;
     throw calc::Exception(s.str());
   }
}
