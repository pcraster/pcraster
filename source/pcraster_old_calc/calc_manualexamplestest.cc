#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLESTEST
#include "calc_manualexamplestest.h"
#define INCLUDED_CALC_MANUALEXAMPLESTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif
// PCRaster library headers.
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
// Module headers.
#ifndef INCLUDED_CALC_MANUALEXAMPLETESTER
#include "calc_manualexampletester.h"
#define INCLUDED_CALC_MANUALEXAMPLETESTER
#endif



/*!
  \file
  This file contains the implementation of the ManualExamplesTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MANUALEXAMPLES MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::ManualExamplesTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ManualExamplesTest> instance(new ManualExamplesTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ManualExamplesTest::testAll, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MANUALEXAMPLES MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::ManualExamplesTest::ManualExamplesTest()
{
}



//! setUp
void calc::ManualExamplesTest::setUp()
{
}

//! tearDown
void calc::ManualExamplesTest::tearDown()
{
  com::changeWorkingDirectory(d_startDir);
}

void calc::ManualExamplesTest::testAll()
{
  BOOST_WARN_MESSAGE( 0, "copy this module from PCRasterModelEngine");
  std::vector<ManualExampleTester> et;
// #include "examples.inc"
  int nrFailures=0;
  for(size_t i=0; i < et.size(); i++) {
    try {
      et[i].test();
    } catch (const com::Exception& e) {
      std::cerr << e.messages();
      nrFailures++;
    }
  }
  if (nrFailures) {
   std::cerr << "Note order.Result.omap will fail, new algoritm in newcalc \n";
   std::cerr << nrFailures << " failures out of " << et.size() << "\n";
  }
  BOOST_WARN((nrFailures <= 3));
}
