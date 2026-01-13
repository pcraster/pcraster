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
    FileCreateTester const m("failureExpected.map");
    BOOST_TEST(!m.equalTo("dtmsmall.map", false));
  }
  {
    bool catched(false);
    try {
      FileCreateTester const m("failureExpected.map");
      m.equalTo("dtmsmall.map", true);
    } catch (const com::FileError &e) {
      BOOST_TEST(e.messages().find("is not created"));
      catched = true;
    }
    BOOST_TEST(catched);
  }

  FileCreateTester m1("FileCreateTester_inp1s.map");

  com::move("inpXs.map", "FileCreateTester_inp1s.map");

  BOOST_TEST(m1.equalTo("inp1s.map"));

  BOOST_TEST(!m1.equalTo("inp5s.map", false));

  BOOST_TEST(!m1.equalTo("dtmsmall.map", false));

  {
    bool catched(false);
    try {
      BOOST_TEST(!m1.equalTo("dtmsmall.map"));
    } catch (const com::Exception &e) {
      catched = true;
      BOOST_TEST(e.messages().find("location") != std::string::npos);
    }
    BOOST_TEST(catched);
  }
  {
    bool catched(false);
    try {
      BOOST_TEST(!m1.equalTo("inp1n.map"));
    } catch (const com::Exception &e) {
      catched = true;
      BOOST_TEST(e.messages().find("value scale") != std::string::npos);
    }
    BOOST_TEST(catched);
  }

  FileCreateTester const o2("out2n.map");

  m1.setDifferenceFile("out2n.map");
  BOOST_TEST(!m1.equalTo("inp5s.map", false));

  BOOST_TEST(o2.equalTo("out2n.map"));
}

BOOST_AUTO_TEST_CASE(test_equal_to_tss)
{
  using namespace geo;

  {
    com::PathName const p1("FileCreateTesterTest1.tss");
    FileCreateTester const m(p1, false);
    BOOST_TEST(m.equalTo(p1));
  }
  {
    com::PathName const p1("FileCreateTesterTest1.tss");
    com::PathName const p2("FileCreateTesterTest2.tss");
    FileCreateTester const m(p1, false);
    BOOST_TEST(!m.equalTo(p2, false));
  }
  {
    com::PathName const p1("FileCreateTesterTest1.tss");
    com::PathName const p2("FileCreateTesterTest1v.tss");
    FileCreateTester const m(p1, false);
    BOOST_TEST(!m.equalTo(p2, false));
  }
  {
    com::PathName const p1("FileCreateTesterTest1.tss");
    com::PathName const p2("FileCreateTesterTest1e.tss");
    FileCreateTester const m(p1, false);
    BOOST_TEST(m.equalTo(p2));
  }
  bool const doesNotSupportNonNativeAsciiFormat = false;
  // hence we have a unix2dos hack in testrun.prolog
  BOOST_TEST_WARN(doesNotSupportNonNativeAsciiFormat);
}
