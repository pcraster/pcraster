#define BOOST_TEST_MODULE pcraster model_engine lexinput
#include <boost/test/unit_test.hpp>
#include "calc_lexinput.h"

BOOST_AUTO_TEST_CASE(testInstallStringScript)
{
  using namespace calc;

  std::string const s("this is a test string");
  std::string r;
  LexInput li;
  const char *argv[1];
  argv[0] = s.c_str();
  li.installArgvScript(1, argv, false);
  int c = 0;
  while ((c = li.getChar()) != EOF) {
    r += (char)c;
  }
  // need for an additonal newline
  BOOST_TEST(r == (s + " "));

  // test piece of code from calc_lexinputsource.cc:
  // erase sealed contents from memory
  std::string contents("pietpaal");
  contents.replace(contents.begin(), contents.end(), contents.size(), 0);
  BOOST_TEST(contents.size() == 8);
  for (char const content : contents) {
    BOOST_TEST(content == 0);
  }
}
