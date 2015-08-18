#include "stddefx.h"

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

std::string quote(const std::string& name) {
  return "'"+name+"'";
}

std::string quote(double value) {
  return quote(com::doubleToStr(value));
}
