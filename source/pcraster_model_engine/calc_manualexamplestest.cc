#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MANUALEXAMPLESTEST
#include "calc_manualexamplestest.h"
#define INCLUDED_CALC_MANUALEXAMPLESTEST
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

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
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
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



void calc::ManualExamplesTest::testAll()
{
    std::vector<ManualExampleTester> et;
#include "cpptest.cc"
    std::vector<std::string> failMsgs;
    for(size_t i=0; i < et.size(); i++)
      try {
        et[i].test();
      } catch (const com::Exception& e) {
        failMsgs.push_back(e.messages());
      }

    size_t nrFailuresAllowed=0;
#ifdef WIN32
    if (failMsgs.size()==1) {
     std::cerr << "TODO view on win32 slightly different" << std::endl;
     nrFailuresAllowed=1;
    }
#endif

    std::ostringstream msgs;
    if (failMsgs.size()) {
     // msgs << " compile mode:  " << compile[c] << "\n";
     for (size_t i=0; i < failMsgs.size(); ++i)
     {
        msgs << "---- MANUAL EXAMPLE FAILURE # " << i << "---------------------------------" << "\n";
        msgs << failMsgs[i];
     }
     msgs << failMsgs.size() << " MANUAL EXAMPLES FAILURES OUT OF " << et.size() << "\n";
    }
#if _MSC_VER == 1400
#ifdef DEBUG_DEVELOP
   // Bugzilla 178
   nrFailuresAllowed=2;
#endif
#endif
    // TODO this will fail on WIN32 due to view function (see above)
    BOOST_CHECK_MESSAGE(failMsgs.size() == nrFailuresAllowed, msgs.str());
}
