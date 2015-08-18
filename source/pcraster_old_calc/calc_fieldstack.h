#ifndef INCLUDED_CALC_FIELDSTACK
#define INCLUDED_CALC_FIELDSTACK

#ifndef INCLUDED_STACK
#include <stack>
#define INCLUDED_STACK
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_FIELD
#include "calc_field.h"
#define INCLUDED_CALC_FIELD
#endif

namespace calc {

class FieldStack {
 private:
  std::stack<FieldHandle> d_stack;
 public:
  FieldStack();

  ~FieldStack();

  FieldHandle popDest(VS newVs);

  FieldHandle popReadOnly();

  void push(FieldHandle v);
};

//! pop \a nr of (src)fields
class FieldsPopped : public std::vector<FieldHandle>
{
public:
  FieldsPopped(FieldStack& stack, size_t nr);
};

}

#endif
