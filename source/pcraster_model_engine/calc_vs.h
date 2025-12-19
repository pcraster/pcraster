#ifndef INCLUDED_CALC_VS
#define INCLUDED_CALC_VS

#include "pcraster_model_engine_export.h"
#include "calc_types.h"

#include <string>
#include <iostream>


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
