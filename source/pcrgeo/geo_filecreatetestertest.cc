#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_FILECREATETESTERTEST
#include "geo_filecreatetestertest.h"
#define INCLUDED_GEO_FILECREATETESTERTEST
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

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
// Module headers.
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif



/*!
  \file
  This file contains the implementation of the FileCreateTesterTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC FILECREATETESTER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*geo::FileCreateTesterTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<FileCreateTesterTest> instance(new FileCreateTesterTest());

   suite->add(BOOST_CLASS_TEST_CASE(&FileCreateTesterTest::testEqualToCsf, instance));
   suite->add(BOOST_CLASS_TEST_CASE(&FileCreateTesterTest::testEqualToTss, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF FILECREATETESTER MEMBERS
//------------------------------------------------------------------------------

//! ctor
geo::FileCreateTesterTest::FileCreateTesterTest()
{
}



//! setUp
void geo::FileCreateTesterTest::setUp()
{
}

//! tearDown
void geo::FileCreateTesterTest::tearDown()
{
}


//! test equal to
/*!
   \todo compare outn2.map to constant 2
 */
void geo::FileCreateTesterTest::testEqualToCsf()
{
  {
    FileCreateTester m("failureExpected.map");
    BOOST_CHECK(!m.equalTo("dtmsmall.map",false));
  }
  {
    bool catched(false);
    try {
      FileCreateTester m("failureExpected.map");
      m.equalTo("dtmsmall.map",true);
    } catch (const com::FileError& e) {
      BOOST_CHECK(e.messages().find("is not created"));
      catched=true;
    }
    BOOST_CHECK(catched);
  }

 FileCreateTester m1("FileCreateTester_inp1s.map");

 com::move("inpXs.map","FileCreateTester_inp1s.map");

 BOOST_CHECK(m1.equalTo("inp1s.map"));

 BOOST_CHECK(!m1.equalTo("inp5s.map",false));

 BOOST_CHECK(!m1.equalTo("dtmsmall.map",false));

 {
   bool catched(false);
   try {
      BOOST_CHECK(!m1.equalTo("dtmsmall.map"));
   } catch (const com::Exception& e) {
      catched=true;
      BOOST_CHECK(e.messages().find("location") != std::string::npos);
   }
   BOOST_CHECK(catched);
 }
 {
   bool catched(false);
   try {
      BOOST_CHECK(!m1.equalTo("inp1n.map"));
   } catch (const com::Exception& e) {
      catched=true;
      BOOST_CHECK(e.messages().find("value scale") != std::string::npos);
   }
   BOOST_CHECK(catched);
 }

 FileCreateTester o2("out2n.map");

 m1.setDifferenceFile("out2n.map");
 BOOST_CHECK(!m1.equalTo("inp5s.map",false));

 BOOST_CHECK(o2.equalTo("out2n.map"));
}

void geo::FileCreateTesterTest::testEqualToTss()
{
  {
    com::PathName p1("FileCreateTesterTest1.tss");
    FileCreateTester m(p1,false);
    BOOST_CHECK(m.equalTo(p1));
  }
  {
    com::PathName p1("FileCreateTesterTest1.tss");
    com::PathName p2("FileCreateTesterTest2.tss");
    FileCreateTester m(p1,false);
    BOOST_CHECK(!m.equalTo(p2,false));
  }
  {
    com::PathName p1("FileCreateTesterTest1.tss");
    com::PathName p2("FileCreateTesterTest1v.tss");
    FileCreateTester m(p1,false);
    BOOST_CHECK(!m.equalTo(p2,false));
  }
  {
    com::PathName p1("FileCreateTesterTest1.tss");
    com::PathName p2("FileCreateTesterTest1e.tss");
    FileCreateTester m(p1,false);
    BOOST_CHECK(m.equalTo(p2));
  }
  bool doesNotSupportNonNativeAsciiFormat=false;
  // hence we have a unix2dos hack in testrun.prolog
  BOOST_WARN(doesNotSupportNonNativeAsciiFormat);
}
