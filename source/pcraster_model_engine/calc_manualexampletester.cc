#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#include "calc_manualexampletester.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTER
#endif

// Library headers.
#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
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
  for(size_t i=0; i<d_result.size(); ++i)
    fct.push_back(geo::FileCreateTester(d_result[i]));

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
    return com::PathName("validated");
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



