#define BOOST_TEST_MODULE pcraster old_calc lex_input
#include <boost/test/unit_test.hpp>
#include "calc_lexinput.h"


BOOST_AUTO_TEST_CASE(install_string_script)
{
  using namespace calc;

  std::string s("this is a test string");
  std::string r;
  LexInput li;
  const char *argv[1];
  argv[0]=s.c_str();
  li.installArgvScript(1,argv,false);
  int c;
  while( (c = li.getChar()) != EOF)
    r+=(char)c;
  // need for an additonal newline
  BOOST_CHECK(r==(s+'\n'));

  // test piece of code from calc_lexinputsource.cc:
  // erase sealed contents from memory
  std::string contents("pietpaal");
  contents.replace(contents.begin(), contents.end(), contents.size(), 0);
  BOOST_CHECK(contents.size()==8);
  for(size_t i=0; i<contents.size(); i++)
    BOOST_CHECK(contents[i]==0);
}
