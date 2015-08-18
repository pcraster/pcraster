#ifndef INCLUDED_CALC_VS
#define INCLUDED_CALC_VS

#ifndef INCLUDED_VSENUM
#include "vsenum.h"
#define INCLUDED_VSENUM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

VS biggestVs(VS setOfVs);

std::string toString(VS vs);
bool isSubset(VS x, VS set);
bool isIn(VS x, VS set);
VS   intersect(VS x, VS set);
VS   unionSet(VS set1, VS set2);
int  nrInSet(VS set);
VS   vsOfNumber(double val);
VS expectedFileType(const std::string& fileName,VS typeExpected);

#endif
