#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#define INCLUDED_CALC_MANUALEXAMPLETESTER

#include "stddefx.h"

#include <string>
#include <vector>



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

  std::string              d_script;
  std::vector<std::string> d_result;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ManualExampleTester               (const std::string& script);

  /* virtual */    ~ManualExampleTester              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              addResult                         (const std::string& result);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void              test                              () const;

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
