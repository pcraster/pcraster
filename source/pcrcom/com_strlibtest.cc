#define BOOST_TEST_MODULE pcraster com str_lib
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <stdexcept>
#include <functional>
#include "com_strlib.h"
#include "com_strconv.h"

//! test all strTo* funcs
BOOST_AUTO_TEST_CASE(from_string)
{
  using namespace com;

  size_t v(strToSize_t(std::string("000001")));
  BOOST_TEST(v == 1);

  v = strToSize_t(std::string("   000001"));
  BOOST_TEST(v == 1);


  // this one fails, trailing whitespace bogs
  v = strToSize_t(std::string("   000001 "));
  BOOST_TEST(v == 1);

  int const i = strToInt(std::string("-0001"));
  BOOST_TEST(i == -1);

  bool visit = false;

  visit = false;
  try {
    strToSize_t(std::string("-0001"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  BOOST_TEST(strToDouble("0.25") == 0.25);
  BOOST_TEST(strToDouble("2") == 2);

  visit = false;
  try {
    strToDouble(std::string(" 1 -10.7 -10.7 "));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToSize_t(std::string("00X0001"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);


  visit = false;
  try {
    strToInt(std::string("NotAnInteger"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToSize_t(std::string("1.0"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToInt(std::string("1.0"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToDouble(std::string("NotADouble"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToInt(std::string(""));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToInt(std::string("   \t \t \n  "));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToDouble(std::string(""));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  visit = false;
  try {
    strToDouble(std::string("   \t \t \n  "));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);

  // hex must not be supported
  visit = false;
  try {
    strToInt(std::string("0x0001"));
  } catch (const std::range_error &) {
    visit = true;
  }
  BOOST_TEST(visit);
}

//! test all *ToStr funcs
BOOST_AUTO_TEST_CASE(to_string)
{
  using namespace com;

  std::string const s(intToStr(-34));
  BOOST_TEST(s.compare(std::string("-34")) == 0);

  std::vector<int> integers;
  integers.push_back(5);
  integers.push_back(2);
  integers.push_back(4);
  integers.push_back(1);

  std::string const result = com::toString<int>(integers.begin(), integers.end(), std::string(", "));
  BOOST_TEST(result == "5, 2, 4, 1");

  // I demand no space!
  double const doubleV = 122;
  BOOST_TEST(doubleToStr(doubleV) == "122");
}

BOOST_AUTO_TEST_CASE(is_number)
{
  using namespace com;

  BOOST_TEST(isDouble("3.4"));
  BOOST_TEST(isDouble(" \t4\n"));
  BOOST_TEST(!isDouble(""));
  BOOST_TEST(!isDouble(" \t \n"));
  BOOST_TEST(!isDouble("3.4 4.5"));
}

//! test compareNoCase()
BOOST_AUTO_TEST_CASE(compare_no_case)
{
  using namespace com;

  BOOST_TEST(compareNoCase("xFg", "XFG") == 0);

  BOOST_TEST(compareNoCase("xFg", "XF") != 0);
  BOOST_TEST(compareNoCase("tFg", "XF") != 0);

  BOOST_TEST(compareNoCase("aBC", "bBC") < 0);
  BOOST_TEST(compareNoCase("bBC", "aBC") > 0);

  StringLessNoCase const lnc;
  BOOST_TEST(lnc("aBC", "bBC"));
  BOOST_TEST(!lnc("bBC", "bBC"));

  BOOST_TEST(lnc("a", "b"));
  BOOST_TEST(!lnc("b", "a"));
}

//! test RemoveFrontEndSpace()
BOOST_AUTO_TEST_CASE(remove_front_end_space)
{
  using namespace com;

  std::string const in1("no whe at end");
  std::string out1(in1);
  removeFrontEndSpace(out1);
  BOOST_TEST(in1 == out1);

  std::string const in2(" \t two words \n ");
  std::string out2(in2);
  removeFrontEndSpace(out2);
  BOOST_TEST(out2 == "two words");

  std::string const in3("");
  std::string out3(in3);
  removeFrontEndSpace(out3);
  BOOST_TEST(out3 == "");

  std::string const in4("      ");
  std::string out4(in4);
  removeFrontEndSpace(out4);
  BOOST_TEST(out4 == "");
}

//! test RemoveAllSpace()
BOOST_AUTO_TEST_CASE(remove_all_space)
{
  using namespace com;

  std::string const in1("no whe at end");
  std::string out1(in1);
  removeAllSpace(out1);
  BOOST_TEST(out1 == "nowheatend");

  std::string const in2(" \t two words \n ");
  std::string out2(in2);
  removeAllSpace(out2);
  BOOST_TEST(out2 == "twowords");

  std::string const in3("");
  std::string out3(in3);
  removeAllSpace(out3);
  BOOST_TEST(out3 == "");

  std::string const in4("      ");
  std::string out4(in4);
  removeAllSpace(out4);
  BOOST_TEST(out4 == "");
}

//! test split() and join()
BOOST_AUTO_TEST_CASE(split_and_join)
{
  using namespace com;

  {
    std::string const in1("no whe at end");
    std::vector<std::string> s1(split(in1));
    BOOST_TEST(s1.size() == 4);
    BOOST_TEST(s1[0] == "no");
    BOOST_TEST(s1[1] == "whe");
    BOOST_TEST(s1[2] == "at");
    BOOST_TEST(s1[3] == "end");

    std::string const joined(join(s1));
    BOOST_TEST(in1 == joined);
  }
  {
    std::string const in1("no \t \n whe at        end \n \t ");
    std::vector<std::string> s1(split(in1));
    BOOST_TEST(s1.size() == 4);
    BOOST_TEST(s1[0] == "no");
    BOOST_TEST(s1[1] == "whe");
    BOOST_TEST(s1[2] == "at");
    BOOST_TEST(s1[3] == "end");

    std::string const joinRes1("noXwheXatXend");
    std::string const joined1(join(s1, "X"));
    BOOST_TEST(joinRes1 == joined1);

    std::string const joinRes2("nowheatend");
    std::string const joined2(join(s1, ""));
    BOOST_TEST(joinRes2 == joined2);
  }
  {
    std::string const in1("");
    std::vector<std::string> const s1(split(in1));
    BOOST_TEST(s1.size() == 0);
    std::string const joined(join(s1, "X"));
    BOOST_TEST(in1 == joined);
  }
  {
    std::string const in1(" \t \n   \n ");
    std::vector<std::string> const s1(split(in1));
    BOOST_TEST(s1.size() == 0);
    std::string const joined(join(s1, "X"));
    BOOST_TEST(joined.empty());
  }
  {
    std::string const in1("no_spaces");
    std::vector<std::string> s1(split(in1));
    BOOST_TEST(s1.size() == 1);
    BOOST_TEST(s1[0] == in1);
    std::string const joined(join(s1, "X"));
    BOOST_TEST(in1 == joined);
  }
}

//! test split(string, splitChar)
BOOST_AUTO_TEST_CASE(split_at_char)
{
  using namespace com;

  {
    std::string const in1("no whe at end");
    std::vector<std::string> s1(split(in1, ' '));
    BOOST_TEST(s1.size() == 4);
    BOOST_TEST(s1[0] == "no");
    BOOST_TEST(s1[1] == "whe");
    BOOST_TEST(s1[2] == "at");
    BOOST_TEST(s1[3] == "end");
  }
  {
    std::string const in1("");
    std::vector<std::string> const s1(split(in1, ' '));
    BOOST_TEST(s1.size() == 0);
  }
  {
    std::string const in1("no_spaces");
    std::vector<std::string> s1(split(in1, ' '));
    BOOST_TEST(s1.size() == 1);
    BOOST_TEST(s1[0] == in1);
  }
  {
    std::string const in1("no\twhe\tat\tend\t");
    std::vector<std::string> s1(split(in1, '\t'));
    BOOST_TEST(s1.size() == 4);
    BOOST_TEST(s1[0] == "no");
    BOOST_TEST(s1[1] == "whe");
    BOOST_TEST(s1[2] == "at");
    BOOST_TEST(s1[3] == "end");
  }
}

//! test createAsCStr()
BOOST_AUTO_TEST_CASE(duplicate_as_c_str)
{
  using namespace com;

  std::string const s1in("this in");
  char *s1out = createAsCStr(s1in);
  BOOST_TEST(s1in == s1out);
  delete[] s1out;

  std::string const s2in("");
  char *s2out = createAsCStr(s2in);
  BOOST_TEST(s2in == s2out);
  delete[] s2out;

  std::string const s3in("\n");
  char *s3out = createAsCStr(s3in);
  BOOST_TEST(s3in == s3out);
  delete[] s3out;
}

//! test equalNoSpace
BOOST_AUTO_TEST_CASE(equal_no_space)
{
  using namespace com;

  std::string const s1("this in\n\nand becomes out");
  std::string const s2("this in andbecomes out");
  BOOST_TEST(equalNoSpace(s1, s2));
}

BOOST_AUTO_TEST_CASE(remove_front_end_char)
{
  using namespace com;

  std::string str;

  str = "";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "");

  str = "/";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "");

  str = "//";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "");

  str = "b";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "/b";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "//b";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "b/";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "b//";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "/b/";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");

  str = "//b//";
  removeFrontEndChar(str, '/');
  BOOST_TEST(str == "b");
}

BOOST_AUTO_TEST_CASE(replace_chars)
{
  using namespace com;

  std::string str;

  str = "";
  replaceChars(str, 'x', "XX");
  BOOST_TEST(str == "");

  str = "x132";
  replaceChars(str, 'x', "Y");
  BOOST_TEST(str == "x132");

  str = "b.b.b";
  replaceChars(str, '_', ".");
  BOOST_TEST(str == "b_b_b");

  str = "b.b.b";
  replaceChars(str, '_', ".b");
  BOOST_TEST(str == "_____");
}

BOOST_AUTO_TEST_CASE(replace_str_by_str)
{
  using namespace com;

  std::string str;

  str = "";
  BOOST_TEST(replaceStrByStr(str, "x", "XX") == "");

  str = "x132";
  BOOST_TEST(replaceStrByStr(str, "Y", "x") == "x132");
  BOOST_TEST(replaceStrByStr(str, "x", "Y") == "Y132");

  str = "b.b.b";
  BOOST_TEST(replaceStrByStr(str, ".", "_") == "b_b_b");
  BOOST_TEST(replaceStrByStr(str, "b.", "_") == "__b");
  BOOST_TEST(replaceStrByStr(str, ".b", "_") == "b__");

  // Empty string to replace. This used to crash.
  BOOST_TEST(replaceStrByStr(str, "", "_") == "b.b.b");
  BOOST_TEST(replaceStrByStr(str, ".", "") == "bbb");
}

BOOST_AUTO_TEST_CASE(remove_front_end_string)
{
  using namespace com;

  std::string str;

  str = "";
  removeFrontEndString(str, "");
  BOOST_TEST(str == "");

  str = "b";
  removeFrontEndString(str, "");
  BOOST_TEST(str == "b");

  str = "b";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "b");

  str = "a";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "");

  str = "ab";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "b");

  str = "aba";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "b");

  str = "abab";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "bab");

  str = "ababa";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "bab");

  str = "aaababaaa";
  removeFrontEndString(str, "a");
  BOOST_TEST(str == "bab");
}

BOOST_AUTO_TEST_CASE(string_less)
{
  using namespace com;

  std::string const str1("a");
  std::string const str2("b");
  std::string const str3("c");

  std::less<std::string> const comp;

  BOOST_TEST(comp(str1, str2) == true);
  BOOST_TEST(comp(str3, str2) == false);
  BOOST_TEST(comp(str3, str3) == false);
}

BOOST_AUTO_TEST_CASE(format_)
{
  std::string unformatted;
  std::string formatted;

  // Empty input.
  {
    unformatted = "";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "");
  }

  // One space.
  {
    unformatted = " ";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "   ");
  }

  // One char.
  {
    unformatted = "c";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "  c");
  }

  // Spaces and chars, exactly one line.
  {
    unformatted = "c c";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "  c c");
  }

  // Spaces and chars, more lines.
  {
    unformatted = " c c c c c c";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "   c\n  c c\n  c c\n  c");
  }

  // Spaces and chars, more lines.
  {
    unformatted = "c c c c c c ";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "  c c\n  c c\n  c c\n");
  }

  {
    unformatted = "bla bla bla bla bla";
    formatted = com::format(unformatted, 2, 5);
    BOOST_TEST(formatted == "  bla\n  bla\n  bla\n  bla\n  bla");
  }
}
