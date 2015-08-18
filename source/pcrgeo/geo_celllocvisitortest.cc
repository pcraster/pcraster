#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITORTEST
#include "geo_celllocvisitortest.h"
#define INCLUDED_GEO_CELLLOCVISITORTEST
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

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

boost::unit_test::test_suite*geo::CellLocVisitorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<CellLocVisitorTest> instance(new CellLocVisitorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&CellLocVisitorTest::testAll, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&CellLocVisitorTest::testDownstream, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

geo::CellLocVisitorTest::CellLocVisitorTest(){
}



void geo::CellLocVisitorTest::setUp()
{
}



void geo::CellLocVisitorTest::tearDown()
{
}

void geo::CellLocVisitorTest::testAll()
{
  const size_t nrRows=2;
  const size_t nrCols=3;

 // test start condition
 CellLocVisitor start(nrRows,nrCols);
 BOOST_CHECK(start.row() == 0 && start.col()==0);
 for(int i=0; i<6; i++) {
  BOOST_CHECK(start.valid());
  ++start;
 }
 BOOST_CHECK(!start.valid());

 // visit old and new way and compare
 std::vector<CellLoc>  thisOrder;
 for(size_t row=0; row < nrRows; row++)
   for(size_t col=0; col < nrCols; col++)
    thisOrder.push_back(CellLoc(row,col));

 int j=0;
 for(CellLocVisitor c(nrRows,nrCols); c.valid(); ++c) {
    BOOST_CHECK(*c == thisOrder[j]);
    j++;
 }
 BOOST_CHECK(j == 6);
}

void geo::CellLocVisitorTest::testDownstream()
{
  {
    CellLocVisitor cv(1,1);
    for(size_t l=1; l <=9; l++) {
      CellLoc c=cv.downstream(l);
      BOOST_CHECK(l==5|| !cv.downstream(c,l));
    }
  }

  CellLocVisitor cv(2,2);
  ++cv;
  BOOST_CHECK(cv.row()==0 && cv.col()==1);
  for(size_t l=1; l <=9; l++) {
      CellLoc c=cv.downstream(l);
      BOOST_CHECK((l>5||l==3)|| cv.downstream(c,l));
  }
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------


