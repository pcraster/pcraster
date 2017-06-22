#ifndef INCLUDED_CALC_VS
#define INCLUDED_CALC_VS

#ifndef INCLUDED_PCRDLL
#include "pcrdll.h"
#define INCLUDED_PCRDLL
#endif

#ifndef INCLUDED_CALC_TYPES
#include "calc_types.h"
#define INCLUDED_CALC_TYPES
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

VS biggestVs(VS setOfVs);

std::string toString(VS vs);
bool isSubset(VS x, VS set);
bool isIn(VS x, VS set);
inline bool isField(VS x) {
  return isIn(x,VS_FIELD);
}
VS   intersect(VS x, VS set);
VS   unionSet(VS set1, VS set2);
int  nrInSet(VS set);
PCR_DLL_FUNC(VS) vsOfNumber(double val);
VS expectedFileType(const std::string& fileName,VS typeExpected);

PCR_DLL_CLASS std::ostream &operator<<(std::ostream& s, VS vs);

namespace calc {
   void checkOneVs(VS vsToCheck,const char* arg);
}

#endif
