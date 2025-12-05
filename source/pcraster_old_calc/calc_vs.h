#ifndef INCLUDED_OLDCALC_VS
#define INCLUDED_OLDCALC_VS

#include "vsenum.h"

#include <string>

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
