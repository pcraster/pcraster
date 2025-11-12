#ifndef INCLUDED_CALC_INDEXSELECTED
#define INCLUDED_CALC_INDEXSELECTED

#include <string>


namespace calc {

//! an index on a parameter
/*
 */
class  IndexSelected {
 public:
  virtual ~IndexSelected() {}
  virtual int select() const=0;
  virtual std::string selectedName() const=0;
  virtual std::string variableName() const=0;
};

}

#endif
