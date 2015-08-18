#ifndef INCLUDED_CALC_QUOTE
#define INCLUDED_CALC_QUOTE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

std::string quote(const std::string& name);
std::string quote(double value);

#endif
