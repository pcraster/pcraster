#ifndef INCLUDED_CALC_FIELDSTACK
#define INCLUDED_CALC_FIELDSTACK

#include "calc_field.h"

#include <stack>
#include <vector>


namespace calc {

class FieldStack {
 private:
  std::stack<FieldHandle> d_stack;
 public:
  FieldStack();

  ~FieldStack();

  FieldHandle popDest(VS newVs);

  FieldHandle popReadOnly();

  void push(const FieldHandle& v);
};

//! pop \a nr of (src)fields
class FieldsPopped : public std::vector<FieldHandle>
{
public:
  FieldsPopped(FieldStack& stack, size_t nr);
};

}

#endif
