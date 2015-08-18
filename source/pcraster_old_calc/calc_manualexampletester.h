#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#define INCLUDED_CALC_MANUALEXAMPLETESTER



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  class PathName;
}



namespace calc {

//! implements a single test for each manual example
class ManualExampleTester
{

private:

  // Assignment operator. DEFAULT
  // ManualExampleTester&           operator=           (const ManualExampleTester&);

  // Copy constructor. DEFAULT
  //  ManualExampleTester               (const ManualExampleTester&);

  std::string              d_expr;
  std::vector<std::string> d_result;
  std::vector<std::string> d_option;
  // set to cloneNotSet.map by default that should generate a non existing map
  std::string              d_clone;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ManualExampleTester               (const std::string& expr);

  /* virtual */    ~ManualExampleTester              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void addResult(const std::string& result);
  void addOption(const std::string& result);
  void setClone(const std::string& clone);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void test() const;

  static com::PathName validatedDirectory();

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace calc

#endif
