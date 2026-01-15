#define BOOST_TEST_MODULE pcraster model_engine cmdlinecalc
#include <boost/test/unit_test.hpp>
#include "com_file.h"
#include "geo_filecreatetester.h"
#include "calc_messagestestdb.h"
#include "calc_cmdlinecalc.h"
#include "calc_LibraryClass.h"

// NOTE use string failureExpected in files expected to fail, see style guide


struct Fixture : public calc::LibraryClassNoQt {

  Fixture() : calc::LibraryClassNoQt("cmdlinecalc")
  {
  }

  ~Fixture() override
  {
  }
};

BOOST_GLOBAL_FIXTURE(Fixture);

BOOST_AUTO_TEST_CASE(testScriptFile)
{
  {
    // TODO test this result: "ERROR: File 'failureExpectedNotExistant.mod': No such file or directory"
    const char *argv[3] = {"pcrcalc", "-f", "failureExpectedNotExistant.mod"};
    int const r = calc::executeCommandLine(3, const_cast<char **>(argv));
    BOOST_TEST(r == 1);
  }

  // model outcomes are already tested in calc_executortest.cc
  // TODO NO THEY ARE NOT
  calc::MessagesTestDB *db = calc::MessagesTestDB::instance();
  std::string const model = db->model("pcrcalc382");

  com::write(model, "tmp.mod");

  {
    const char *argv[3] = {"pcrcalc", "-f", "tmp.mod"};
    int const r = calc::executeCommandLine(3, const_cast<char **>(argv));
    BOOST_TEST(r == 0);
  }
  {
    const char *argv[6] = {"pcrcalc", "--clone", "inp1b.map", "-m", "-f", "tmp.mod"};
    int const r = calc::executeCommandLine(6, const_cast<char **>(argv));
    BOOST_TEST(r == 0);
  }
  {
    //    const char *argv[6]= { "pcrcalc", "-m", "-r", "/home/cees/tmp/pcrtest",
    //                                      "-f", "/home/cees/tmp/pcrtest/pcrtest.mod"
    //                   };
    bool const absolutePathInRunDirectory = false;
    BOOST_TEST_WARN(absolutePathInRunDirectory);
    int const r = 1;  // TODO calc::executeCommandLine(6, argv); MAKE a bugzilla note
    BOOST_TEST_WARN(r == 0);
  }
}

BOOST_AUTO_TEST_CASE(testModelAsArgs)
{
  {
    geo::FileCreateTester const fct("tmp.res");
    const char *argv[2] = {"pcrcalc", "tmp.res = inp1s.map + 4"};
    int const r = calc::executeCommandLine(2, const_cast<char **>(argv));
    BOOST_TEST(r == 0);
    BOOST_TEST(fct.equalTo("inp5s.map", false));
  }
  {
    geo::FileCreateTester const fct("tmp.res");

    const char *argv[] = {"pcrcalc", "tmp.res", "=", "inp1s.map", "+", " 4"};
    int const r = calc::executeCommandLine(ARRAY_SIZE(argv), const_cast<char **>(argv));
    BOOST_TEST(r == 0);
    BOOST_TEST(fct.equalTo("inp5s.map", false));
  }
}
