#include "stddefx.h"
#include "calc_manualexampletester.h"
#include "com_strlib.h"
#include "geo_filecreatetester.h"
#include "com_exception.h"
#include "mathx.h"
#include "calc_modelbuilder.h"


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

calc::ManualExampleTester::ManualExampleTester(const std::string& expr):
   d_expr(expr),
   d_clone("cloneNotSet_failureExpected.map")
{
}

void calc::ManualExampleTester::addResult(const std::string& result)
{
  d_result.push_back(result);
}

void calc::ManualExampleTester::addOption(const std::string& option)
{
  d_option.push_back(option);
}

void calc::ManualExampleTester::setClone(const std::string& clone)
{
  d_clone=clone;
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
  ModelBuilder mb;
  // simpel test for -M but needs areamap for each command :=(
  // mb.setMVCompression(true);

  SetRan(1); // always the same seed

  mb.setClone(d_clone);

  for(const auto & i : d_option)
    mb.setGlobalOption(i);

  std::string statement;
  statement += com::join(d_result,",");
  statement += "="+d_expr;

  // hack in old calc to skip tests new or changed functions
  if (statement.find("dynwave") != std::string::npos)
    return;
  if (statement.find("areaorder") != std::string::npos)
    return;
  if (statement.find("argorder") != std::string::npos)
    return;
  if (statement.find("kinematic") != std::string::npos)
    return;


  mb.addStatement(statement,true);

  // remove results
  std::vector<geo::FileCreateTester> fct;
  fct.reserve(d_result.size());
  for(const auto & i : d_result){
    fct.push_back(geo::FileCreateTester(i));
  }

  // create results
  mb.execute();

  // check results
  for(size_t i=0; i<d_result.size(); ++i) {
    com::PathName now(d_result[i]);
    com::PathName validated(validatedDirectory());
    validated+=now;
    validated.setExtension("map");

    try {
      fct[i].equalTo(validated,true);
    } catch (com::Exception& e) {
      e.prepend("Expr: "+d_expr);
      throw e;
    }
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



