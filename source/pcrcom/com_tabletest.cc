#define BOOST_TEST_MODULE pcraster com table
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_table.h"
#include "com_file.h"


BOOST_AUTO_TEST_CASE(text_format)
{
  using namespace com;

  com::Table tab;
  std::ifstream ifs;
  // UNIX line feeds
  com::open(ifs,"zinc.unix.eas");

  ifs >> tab;


  BOOST_CHECK(tab.nrRecs()==155);
  BOOST_CHECK(tab.nrCols()==3);

  // first 2 lines
  BOOST_CHECK(tab.value(0,0)==181072);
  BOOST_CHECK(tab.value(1,0)==333611);
  BOOST_CHECK(tab.value(2,0)==1022);
  BOOST_CHECK(tab.value(0,1)==181025);
  BOOST_CHECK(tab.value(1,1)==333558);
  BOOST_CHECK(tab.value(2,1)==1141);

  // last
  BOOST_CHECK(tab.value(0,154)==180627);
  BOOST_CHECK(tab.value(1,154)==330190);
  BOOST_CHECK(tab.value(2,154)==375);
}
