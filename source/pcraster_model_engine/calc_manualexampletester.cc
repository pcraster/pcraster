#include "stddefx.h"
#include "calc_manualexampletester.h"
#include "geo_filecreatetester.h"
#include "com_exception.h"
#include "calc_modelbuilder.h"

#include <set>
#include <string>

/*!
  \file
  This file contains the implementation of the ManualExampleTester class.
*/



//------------------------------------------------------------------------------

/*
namespace calc {

class ManualExampleTesterPrivate
{
public:

  ManualExampleTesterPrivate()
  {
  }

  ~ManualExampleTesterPrivate()
  {
  }

};

} // namespace calc
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MANUALEXAMPLETESTER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MANUALEXAMPLETESTER MEMBERS
//------------------------------------------------------------------------------

calc::ManualExampleTester::ManualExampleTester(
  const std::string& script):
   d_script(script)
{
}

void calc::ManualExampleTester::addResult(const std::string& result)
{
  d_result.push_back(result);
}

calc::ManualExampleTester::~ManualExampleTester()
{
}

/*!
 * \throws
 *   com::Exception if validated result is not equal to one computed
 */
void calc::ManualExampleTester::test() const
{
 try {

  ModelBuilder mb;
  mb.setModel(d_script);

  // first remove results ----------------------------------------------------------------
  std::vector<geo::FileCreateTester> fct;
  fct.reserve(d_result.size());
  for(const auto & i : d_result){
    fct.push_back(geo::FileCreateTester(i));
  }

  // create results ----------------------------------------------------------------------
  mb.execute();

  // check results -------------------------------------------------------------------------

  for(size_t i=0; i<d_result.size(); ++i) {
    com::PathName now(d_result[i]);
    com::PathName validated(validatedDirectory());
    validated+=now;
    fct[i].equalTo(validated,true);
  }
 } catch (com::Exception& e) {
   e.prepend("Expr: "+d_script);
   throw e;
 } catch(...) {
   com::Exception e("Unknown exception");
   e.prepend("Expr: "+d_script);
   throw e;
 }
}

com::PathName calc::ManualExampleTester::validatedDirectory()
{
    return {"validated"};
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



