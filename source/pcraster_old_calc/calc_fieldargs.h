#ifndef INCLUDED_CALC_FIELDARGS
#define INCLUDED_CALC_FIELDARGS

#ifndef INCLUDED_VECTOR
# include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_CALC_ELEMENT
#include "calc_element.h"
#define INCLUDED_CALC_ELEMENT
#endif

namespace calc {

class Operator;
class FieldExpr;
class FieldStack;
class InfoScript;

//! holds all arguments of type field expr
class FieldArgs {
 public:
  typedef std::vector<FieldExpr *> Args;
 private:
  void checkArgs();
  //! position of arguments
  Element     d_pos;
  //! the operator the arguments belong to
  const Operator& d_op;
  //! the field arguments
  Args d_args;
  bool d_ownArgs;
 public:
  // ACCESSORS
  const Operator& op() const;
  // CREATORS
  FieldArgs(const Element& p, const Operator& op, Args& args);
  //! the modellink hack
  FieldArgs(const Element& p, const Operator& op, const Args& args);
  virtual ~FieldArgs();

  void restrictFieldArgs(size_t fieldArgOffset);

  void skipExecution();
  void prepareExecution();
  void executeArgs(FieldStack& stack);
 protected:
         Args& fieldArgs()       { return d_args;}
   const Args& fieldArgs() const { return d_args;}

  size_t           nrFieldArgs         () const
  {
    return d_args.size();
  }

  void print(InfoScript& i)const;

};

}

#endif
