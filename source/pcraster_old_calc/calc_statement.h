#ifndef INCLUDED_OLDCALC_STATEMENT
#define INCLUDED_OLDCALC_STATEMENT

#include "calc_element.h"



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
  ~Statement() override;

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
