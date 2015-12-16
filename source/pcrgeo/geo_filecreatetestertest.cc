#define BOOST_TEST_MODULE pcraster geo file_create_tester
#include <boost/test/unit_test.hpp>
#include "com_exception.h"
#include "com_file.h"
#include "geo_filecreatetester.h"


//! test equal to
/*!
   \todo compare outn2.map to constant 2
 */
BOOST_AUTO_TEST_CASE(test_equal_to_csf)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(test_equal_to_tss)
{
  using namespace geo;

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
