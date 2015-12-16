#define BOOST_TEST_MODULE pcraster geo cell_loc_visitor
#include <boost/test/unit_test.hpp>
#include <vector>
#include "geo_celllocvisitor.h"


BOOST_AUTO_TEST_CASE(all)
{
  using namespace geo;

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


BOOST_AUTO_TEST_CASE(down_stream)
{
  using namespace geo;

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
