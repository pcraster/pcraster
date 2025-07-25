#include "stddefx.h"
#include "calc_quote.h"
#include "com_strlib.h"

std::string quote(const std::string& name) {
  return "'"+name+"'";
}

std::string quote(double value) {
  return quote(com::doubleToStr(value));
}
