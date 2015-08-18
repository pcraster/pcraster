#ifndef INCLUDED_CALC_INDEXSELECTED
#define INCLUDED_CALC_INDEXSELECTED

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

//! an index on a parameter
/*
 */
class  IndexSelected {
 public:
  virtual ~IndexSelected() {};
  virtual int select() const=0;
  virtual std::string selectedName() const=0;
  virtual std::string variableName() const=0;
};

}

#endif
