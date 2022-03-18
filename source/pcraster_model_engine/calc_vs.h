#ifndef INCLUDED_CALC_VS
#define INCLUDED_CALC_VS

#include "pcraster_model_engine_export.h"

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
PCR_ME_EXPORT VS vsOfNumber(double val);
VS expectedFileType(const std::string& fileName,VS typeExpected);

PCR_ME_EXPORT std::ostream &operator<<(std::ostream& s, VS vs);

namespace calc {
   void checkOneVs(VS vsToCheck,const char* arg);
}

#endif
