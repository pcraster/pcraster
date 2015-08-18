#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#include "calc_manualexampletester.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTER
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_MATHX
#include "mathx.h"
#define INCLUDED_MATHX
#endif

// Module headers.
#ifndef INCLUDED_CALC_MODELBUILDER
#include "calc_modelbuilder.h"
#define INCLUDED_CALC_MODELBUILDER
#endif


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

  for(size_t i=0; i<d_option.size(); ++i)
    mb.setGlobalOption(d_option[i]);

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
  for(size_t i=0; i<d_result.size(); ++i)
    fct.push_back(geo::FileCreateTester(d_result[i]));

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
    return com::PathName("validated");
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



