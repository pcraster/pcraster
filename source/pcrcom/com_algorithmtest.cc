#define BOOST_TEST_MODULE pcraster com algorithm
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_algorithm.h"
#include <vector>


BOOST_AUTO_TEST_CASE(has_element)
{
  using namespace com;

  std::vector<int> v1;
  BOOST_CHECK(!hasElement(v1,9));
  v1.push_back(9);
  BOOST_CHECK(hasElement(v1,9));
}


BOOST_AUTO_TEST_CASE(seq_0123_etc)
{
  using namespace com;

  {
    size_t buf[6];
    std::generate_n(buf,6, SeqInc<size_t>());
    BOOST_CHECK(buf[0]==0);
    BOOST_CHECK(buf[4]==4);
    BOOST_CHECK(buf[5]==5);
  }
  {
    double buf[6];
    SeqInc<double> ts(-1, 0.5);
    std::generate_n(buf,6, ts);
    BOOST_CHECK(buf[0]==-1.0);
    BOOST_CHECK(buf[1]==-0.5);
    BOOST_CHECK(buf[4]==1.0);
  }
  {
    double buf[6];
    std::generate_n(buf,6, SeqInc<double>(-1, 0.5));
    BOOST_CHECK(buf[0]==-1.0);
    BOOST_CHECK(buf[1]==-0.5);
    BOOST_CHECK(buf[4]== 1.0);
  }
  {
    double buf[6];
    std::generate_n(buf,6, SeqInc<double>(-2));
    BOOST_CHECK(buf[0]==-2.0);
    BOOST_CHECK(buf[1]==-1.0);
    BOOST_CHECK(buf[4]== 2.0);
  }
}


BOOST_AUTO_TEST_CASE(find_value)
{
  using namespace com;

  std::map<std::string,std::string> str;
  FindValue<std::string,std::string> fStr("xx");

  BOOST_CHECK(fStr.find(str,std::string("key")) == "xx");

  str.insert(std::make_pair(std::string("KEY1"),std::string("VAL1")));
  BOOST_CHECK(fStr.find(str,std::string("KEY1")) == "VAL1");

  std::map<std::string,double> dob;
  FindValue<std::string,double> dStr(4);
  BOOST_CHECK(dStr.find(dob,std::string("key")) == 4);
  dob.insert(std::make_pair(std::string("KEY1"),1));
  BOOST_CHECK(dStr.find(dob,std::string("KEY1")) == 1);
}
