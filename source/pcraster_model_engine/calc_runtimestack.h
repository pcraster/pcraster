#ifndef INCLUDED_CALC_RUNTIMESTACK
#define INCLUDED_CALC_RUNTIMESTACK

#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

namespace calc {
class DataValue;
class Field;

//! Stack used in RunTimeEnv.
/*!
 */

class RunTimeStack {
 private:
  //! DataValue data
  std::stack<DataValue *> d_stack;
 public:

  RunTimeStack();

  ~RunTimeStack();

  DataValue     *pop();

  void push(DataValue* v);

  void clean();

  size_t size() const;
};

}

#endif
