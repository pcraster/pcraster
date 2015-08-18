#ifndef INCLUDED_CALC_GLOBARGS
#define INCLUDED_CALC_GLOBARGS

#ifndef INCLUDED_CALC_FIELDSTACK
#include "calc_fieldstack.h"
#define INCLUDED_CALC_FIELDSTACK
#endif

namespace calc {

class Operator;
class GlobArg;
class Compressor;

//! collection of map arguments taken from stack
class GlobArgs {
  const size_t d_nrArgs;
  void **d_vals;
  FieldsPopped d_fields;
  std::vector<GlobArg *> d_args;
public:
  GlobArgs(const Operator& op,
      const Compressor& compressor,
      FieldStack& stack, size_t nrActualArgs=0);

  //! get MAP_* ptr of i'th arg
  void *MAP_ptr(size_t i) {
    return d_vals[i];
  };
  FieldHandle Field_ptr(size_t i) {
    return d_fields[i];
  };
  const void **mapVals();
  ~GlobArgs();
};

}

#endif
