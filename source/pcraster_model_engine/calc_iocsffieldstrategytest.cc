#define BOOST_TEST_MODULE pcraster model_engine iocsffieldstrategy
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "com_pathinfo.h"
#include "calc_iofieldstrategy.h"
#include "calc_rundirectory.h"
#include "calc_gridmap.h"
#include "pcrtypes.h"
#include "calc_globallibdefs.h"


struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};

BOOST_FIXTURE_TEST_SUITE(iocsffieldstrategy, Fixture)

BOOST_AUTO_TEST_CASE(testGetStackReaderDefault)
{
  using namespace calc;

 {
  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  std::string stackName("tmp");
  std::string sr = s.inPathStack(rd,stackName,1);
  std::string result("tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName,1) == result);
  BOOST_CHECK(sr == stackName);
 }

 {
  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  std::string stackName("./stackReader/tmp00000.001");
  BOOST_CHECK(com::pathExists(stackName));
  std::string sr = s.inPathStack(rd,stackName,1);
  com::PathName  result("./stackReader/tmp00000.001");
  result.makeNative();

  BOOST_CHECK(s.makeStackItemName(stackName,1) == result.toString());

  BOOST_CHECK(sr == result.toString());
 }
}

BOOST_AUTO_TEST_CASE(testGetStackReaderPath)
{
  using namespace calc;

  IoFieldStrategy s(APP_IO_PCRASTER);
  RunDirectory rd;
  com::PathName path("stackReader");

  rd.setRunDirectory(path.toString(),"");
  path.makeAbsolute();
  std::string stackName("tmp");
  std::string sr = s.inPathStack(rd,stackName,1);
  std::string item1("tmp00000.001");
  BOOST_CHECK(s.makeStackItemName(stackName,1) == item1);
  com::PathName result(path+stackName);
  BOOST_CHECK(sr == result.toString());

}

BOOST_AUTO_TEST_CASE(testGridMap)
{
  using namespace calc;

  {
    GridMapIn g("inp5s.map");
    BOOST_CHECK(g.nrRows()==5);
    BOOST_CHECK(g.nrCols()==5);
    BOOST_CHECK(g.vs()    ==VS_S);
    REAL4 data[25];
    data[0]=8;
    data[24]=8;
    g.createSpatial(data,VS_S);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK(data[1]==5);
    BOOST_CHECK(data[24]==5);
  }
  {
    GridMapIn g("all1_float.bil");
    BOOST_CHECK(g.nrRows()==4);
    BOOST_CHECK(g.nrCols()==4);
    BOOST_CHECK(g.vs()    ==VS_S);
    REAL4 data[16];
    data[0]=8;
    data[15]=8;
    g.createSpatial(data,VS_S);
    BOOST_CHECK(pcr::isMV(data[0]));
    BOOST_CHECK(data[1]==1.0f);
    BOOST_CHECK(data[14]==1.0f);
  }
}

BOOST_AUTO_TEST_SUITE_END()
