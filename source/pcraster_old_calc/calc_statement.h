#ifndef INCLUDED_CALC_STATEMENT
#define INCLUDED_CALC_STATEMENT

#ifndef INCLUDED_CALC_ELEMENT
#include "calc_element.h"
#define INCLUDED_CALC_ELEMENT
#endif

namespace calc {

class InfoScript;

//! a statement like 'id = expr'
class Statement: public Element {
 protected:
  // CONSTRUCTORS
  Statement( const Element& pos);

  //! must be implemented in a derived to do useful work
  virtual void run() =0;

 public:
  // DESTRUCTORS
  virtual ~Statement();

  // MANIPULATORS
  //! return true if nonspatial to spatial promotion occurred
  virtual bool buildTypes()=0;
  virtual void prepareExecution()=0;

  void start();

  // ACCESSORS

  //! write script to info format
  virtual void print(InfoScript& i) const;
};

}

#endif
