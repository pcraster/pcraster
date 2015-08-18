#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_STRLIBTEST
#include "com_strlibtest.h"
#define INCLUDED_COM_STRLIBTEST
#endif

#ifndef INCLUDED_STDEXCEP
#include <stdexcept>
#define INCLUDED_STDEXCEP
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
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

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif

#ifndef INCLUDED_COM_STRCONV
#include "com_strconv.h"
#define INCLUDED_COM_STRCONV
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::StrLibTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<StrLibTest> instance(new StrLibTest());

  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testToString, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testIsNumber, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testCompareNoCase, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testRemoveFrontEndSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testRemoveAllSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testReplaceChars, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testReplaceStrByStr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testRemoveFrontEndChar, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testRemoveFrontEndString, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testSplitAndJoin, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testSplitAtChar, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testDuplicateAsCStr, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testEqualNoSpace, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testStringLess, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testFormat, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&StrLibTest::testFromString, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! ctor
com::StrLibTest::StrLibTest()
{
}

//! setUp
void com::StrLibTest::setUp()
{
}

//! tearDown
void com::StrLibTest::tearDown()
{
}

//! test all strTo* funcs
void com::StrLibTest::testFromString()
{
  size_t v(strToSize_t(std::string("000001")));
  BOOST_CHECK(v == 1);

  v = strToSize_t(std::string("   000001"));
  BOOST_CHECK(v == 1);


  // this one fails, trailing whitespace bogs
  v = strToSize_t(std::string("   000001 "));
  BOOST_CHECK(v == 1);

  int i = strToInt(std::string("-0001"));
  BOOST_CHECK(i == -1);

  bool visit;

  visit=false;
  try {
    strToSize_t(std::string("-0001"));
  } catch (std::range_error) {
    visit=true;
  }
  BOOST_CHECK(visit);

  BOOST_CHECK(strToDouble("0.25") == 0.25);
  BOOST_CHECK(strToDouble("2") == 2);

  visit=false;
  try {
    strToDouble(std::string(" 1 -10.7 -10.7 "));
  } catch (std::range_error) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToSize_t(std::string("00X0001"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);
  

  visit=false;
  try {
    strToInt(std::string("NotAnInteger"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToSize_t(std::string("1.0"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToInt(std::string("1.0"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToDouble(std::string("NotADouble"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToInt(std::string(""));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToInt(std::string("   \t \t \n  "));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToDouble(std::string(""));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

  visit=false;
  try {
    strToDouble(std::string("   \t \t \n  "));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);
  
  // hex must not be supported
  visit=false;
  try {
   strToInt(std::string("0x0001"));
  } catch (const std::range_error& ) {
    visit=true;
  }
  BOOST_CHECK(visit);

}



//! test all *ToStr funcs
void com::StrLibTest::testToString()
{
  std::string s(intToStr(-34));
  BOOST_CHECK(s.compare(std::string("-34")) == 0);

  std::vector<int> integers;
  integers.push_back(5);
  integers.push_back(2);
  integers.push_back(4);
  integers.push_back(1);

  std::string result = com::toString<int>(integers.begin(), integers.end(),
         std::string(", "));
  BOOST_CHECK(result == "5, 2, 4, 1");

  // I demand no space!
  double doubleV=122;
  BOOST_CHECK(doubleToStr(doubleV) == "122");
}

void com::StrLibTest::testIsNumber()
{
  BOOST_CHECK( isDouble("3.4"));
  BOOST_CHECK( isDouble(" \t4\n"));
  BOOST_CHECK(!isDouble(""));
  BOOST_CHECK(!isDouble(" \t \n"));
  BOOST_CHECK(!isDouble("3.4 4.5"));
}


//! test compareNoCase()
void com::StrLibTest::testCompareNoCase()
{
  BOOST_CHECK(compareNoCase("xFg","XFG") == 0);

  BOOST_CHECK(compareNoCase("xFg","XF")  != 0);
  BOOST_CHECK(compareNoCase("tFg","XF")  != 0);

  BOOST_CHECK(compareNoCase("aBC","bBC") < 0);
  BOOST_CHECK(compareNoCase("bBC","aBC") > 0);

  StringLessNoCase lnc;
  BOOST_CHECK( lnc("aBC","bBC"));
  BOOST_CHECK(!lnc("bBC","bBC"));

  BOOST_CHECK( lnc("a","b"));
  BOOST_CHECK(!lnc("b","a"));
}

//! test RemoveFrontEndSpace()
void com::StrLibTest::testRemoveFrontEndSpace()
{
  std::string in1("no whe at end"),out1(in1);
  removeFrontEndSpace(out1);
  BOOST_CHECK(in1==out1);

  std::string in2(" \t two words \n "),out2(in2);
  removeFrontEndSpace(out2);
  BOOST_CHECK(out2 == "two words");

  std::string in3(""),out3(in3);
  removeFrontEndSpace(out3);
  BOOST_CHECK(out3 == "");

  std::string in4("      "),out4(in4);
  removeFrontEndSpace(out4);
  BOOST_CHECK(out4 == "");
}

//! test RemoveAllSpace()
void com::StrLibTest::testRemoveAllSpace()
{
  std::string in1("no whe at end"),out1(in1);
  removeAllSpace(out1);
  BOOST_CHECK(out1 == "nowheatend");

  std::string in2(" \t two words \n "),out2(in2);
  removeAllSpace(out2);
  BOOST_CHECK(out2 == "twowords");

  std::string in3(""),out3(in3);
  removeAllSpace(out3);
  BOOST_CHECK(out3 == "");

  std::string in4("      "),out4(in4);
  removeAllSpace(out4);
  BOOST_CHECK(out4 == "");
}

//! test split() and join()
void com::StrLibTest::testSplitAndJoin()
{
 {
  std::string in1("no whe at end");
  std::vector<std::string> s1(split(in1));
  BOOST_CHECK(s1.size()==4);
  BOOST_CHECK(s1[0] == "no");
  BOOST_CHECK(s1[1] == "whe");
  BOOST_CHECK(s1[2] == "at");
  BOOST_CHECK(s1[3] == "end");

  std::string joined(join(s1));
  BOOST_CHECK(in1 == joined);
 }
 {
  std::string in1("no \t \n whe at        end \n \t ");
  std::vector<std::string> s1(split(in1));
  BOOST_CHECK(s1.size()==4);
  BOOST_CHECK(s1[0] == "no");
  BOOST_CHECK(s1[1] == "whe");
  BOOST_CHECK(s1[2] == "at");
  BOOST_CHECK(s1[3] == "end");

  std::string joinRes1("noXwheXatXend");
  std::string joined1(join(s1,"X"));
  BOOST_CHECK(joinRes1 == joined1);

  std::string joinRes2("nowheatend");
  std::string joined2(join(s1,""));
  BOOST_CHECK(joinRes2 == joined2);
 }
 {
  std::string in1("");
  std::vector<std::string> s1(split(in1));
  BOOST_CHECK(s1.size()==0);
  std::string joined(join(s1,"X"));
  BOOST_CHECK(in1 == joined);
 }
 {
  std::string in1(" \t \n   \n ");
  std::vector<std::string> s1(split(in1));
  BOOST_CHECK(s1.size()==0);
  std::string joined(join(s1,"X"));
  BOOST_CHECK(joined.empty());
 }
 {
  std::string in1("no_spaces");
  std::vector<std::string> s1(split(in1));
  BOOST_CHECK(s1.size()==1);
  BOOST_CHECK(s1[0]==in1);
  std::string joined(join(s1,"X"));
  BOOST_CHECK(in1 == joined);
 }
}

//! test split(string, splitChar)
void com::StrLibTest::testSplitAtChar()
{
 {
  std::string in1("no whe at end");
  std::vector<std::string> s1(split(in1,' '));
  BOOST_CHECK(s1.size()==4);
  BOOST_CHECK(s1[0] == "no");
  BOOST_CHECK(s1[1] == "whe");
  BOOST_CHECK(s1[2] == "at");
  BOOST_CHECK(s1[3] == "end");
 }
 {
  std::string in1("");
  std::vector<std::string> s1(split(in1,' '));
  BOOST_CHECK(s1.size()==0);
 }
 {
  std::string in1("no_spaces");
  std::vector<std::string> s1(split(in1,' '));
  BOOST_CHECK(s1.size()==1);
  BOOST_CHECK(s1[0]==in1);
 }
 {
  std::string in1("no\twhe\tat\tend\t");
  std::vector<std::string> s1(split(in1,'\t'));
  BOOST_CHECK(s1.size()==4);
  BOOST_CHECK(s1[0] == "no");
  BOOST_CHECK(s1[1] == "whe");
  BOOST_CHECK(s1[2] == "at");
  BOOST_CHECK(s1[3] == "end");
 }
}

//! test createAsCStr()
void com::StrLibTest::testDuplicateAsCStr()
{
  std::string s1in("this in");
  char *s1out = createAsCStr(s1in);
  BOOST_CHECK(s1in == s1out);
  delete [] s1out;

  std::string s2in("");
  char *s2out = createAsCStr(s2in);
  BOOST_CHECK(s2in == s2out);
  delete [] s2out;

  std::string s3in("\n");
  char *s3out = createAsCStr(s3in);
  BOOST_CHECK(s3in == s3out);
  delete [] s3out;
}

//! test equalNoSpace
void com::StrLibTest::testEqualNoSpace()
{
  std::string s1("this in\n\nand becomes out");
  std::string s2("this in andbecomes out");
  BOOST_CHECK(equalNoSpace(s1,s2));
}



void com::StrLibTest::testRemoveFrontEndChar()
{
  std::string str;

  str = "";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "");

  str = "/";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "");

  str = "//";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "");

  str = "b";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "/b";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "//b";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "b/";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "b//";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "/b/";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");

  str = "//b//";
  removeFrontEndChar(str, '/');
  BOOST_CHECK(str == "b");
}

void com::StrLibTest::testReplaceChars()
{
  std::string str;

  str = "";
  replaceChars(str,'x', "XX");
  BOOST_CHECK(str == "");

  str = "x132";
  replaceChars(str,'x', "Y");
  BOOST_CHECK(str == "x132");

  str = "b.b.b";
  replaceChars(str,'_',".");
  BOOST_CHECK(str == "b_b_b");

  str = "b.b.b";
  replaceChars(str,'_',".b");
  BOOST_CHECK(str == "_____");

}

void com::StrLibTest::testReplaceStrByStr()
{
  std::string str;

  str = "";
  BOOST_CHECK(replaceStrByStr(str,"x", "XX")=="");

  str = "x132";
  BOOST_CHECK(replaceStrByStr(str,"Y", "x")== "x132");
  BOOST_CHECK(replaceStrByStr(str,"x", "Y")== "Y132");

  str = "b.b.b";
  BOOST_CHECK(replaceStrByStr(str,".","_")  == "b_b_b");
  BOOST_CHECK(replaceStrByStr(str,"b.","_")  == "__b");
  BOOST_CHECK(replaceStrByStr(str,".b","_")  == "b__");

  // Empty string to replace. This used to crash.
  BOOST_CHECK(replaceStrByStr(str,"","_")  == "b.b.b");
  BOOST_CHECK(replaceStrByStr(str,".","")  == "bbb");
}



void com::StrLibTest::testRemoveFrontEndString()
{
  std::string str;

  str = "";
  removeFrontEndString(str, "");
  BOOST_CHECK(str == "");

  str = "b";
  removeFrontEndString(str, "");
  BOOST_CHECK(str == "b");

  str = "b";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "b");

  str = "a";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "");

  str = "ab";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "b");

  str = "aba";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "b");

  str = "abab";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "bab");

  str = "ababa";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "bab");

  str = "aaababaaa";
  removeFrontEndString(str, "a");
  BOOST_CHECK(str == "bab");
}


void com::StrLibTest::testStringLess()
{
  std::string str1("a");
  std::string str2("b");
  std::string str3("c");

  std::less<std::string> comp;

  BOOST_CHECK(comp(str1, str2) == true);
  BOOST_CHECK(comp(str3, str2) == false);
  BOOST_CHECK(comp(str3, str3) == false);
}



void com::StrLibTest::testFormat()
{
  std::string unformatted, formatted;

  // Empty input.
  {
    unformatted = "";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "");
  }

  // One space.
  {
    unformatted = " ";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "   ");
  }

  // One char.
  {
    unformatted = "c";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "  c");
  }

  // Spaces and chars, exactly one line.
  {
    unformatted = "c c";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "  c c");
  }

  // Spaces and chars, more lines.
  {
    unformatted = " c c c c c c";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "   c\n  c c\n  c c\n  c");
  }

  // Spaces and chars, more lines.
  {
    unformatted = "c c c c c c ";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "  c c\n  c c\n  c c\n");
  }

  {
    unformatted = "bla bla bla bla bla";
    formatted = format(unformatted, 2, 5);
    BOOST_CHECK(formatted == "  bla\n  bla\n  bla\n  bla\n  bla");
  }
}
