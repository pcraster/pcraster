#ifndef INCLUDED_CALC_METHODOPERATOR
#define INCLUDED_CALC_METHODOPERATOR

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

class ModelLinkMethodSignature;
class Operator;

//! dynamic created operator info for modellink method
class MethodOperator {
 Operator *d_op;
public:
 MethodOperator(
  const std::string& name,
  const ModelLinkMethodSignature& s);
 ~MethodOperator();
 const Operator& operator()() const { return *d_op; }
};

}

#endif
