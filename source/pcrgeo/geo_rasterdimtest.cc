#define BOOST_TEST_MODULE pcraster geo raster_dim
#include <boost/test/unit_test.hpp>
#include "geo_rasterdim.h"
#include "geo_neighbour.h"


BOOST_AUTO_TEST_CASE(target)
{
  using namespace geo;

  RasterDim rd(3,3);

  // all NBS are inside grid
  LinearLoc f=rd.convert(CellLoc(1,1));
  BOOST_CHECK(f==4);


  BOOST_CHECK(rd.target<LDD>(f,7)==0);
  BOOST_CHECK(rd.target<LDD>(f,3)==8);
  BOOST_CHECK(rd.target<LDD>(f,5)==4);

  BOOST_CHECK(rd.target<NB>(f,2)==8);
/*
  std::cout << "val " << rd.target<LDD>(f,7) << " \n";
  BOOST_CHECK(rd.target<NB>(f,1, false));
  BOOST_CHECK(rd.target<NB>(f,2, false));
  BOOST_CHECK(rd.target<NB>(f,3, false));
  BOOST_CHECK(rd.target<NB>(f,4, false));
  BOOST_CHECK(rd.target<NB>(f,5, false));
  BOOST_CHECK(rd.target<NB>(f,6, false));
*/
  //BOOST_CHECK(nbTarget(f,rd,7, false));
/*
  // left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, false));
  BOOST_CHECK(lddTarget(f,rd,3, false));
  BOOST_CHECK(lddTarget(f,rd,4, true ));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, false));
  BOOST_CHECK(lddTarget(f,rd,7, true ));
  BOOST_CHECK(lddTarget(f,rd,8, false));
  BOOST_CHECK(lddTarget(f,rd,9, false));

  // top/left NBs are outside
  f=rd.convert(CellLoc(1,0));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, false));
  BOOST_CHECK(lddTarget(f,rd,3, false));
  BOOST_CHECK(lddTarget(f,rd,4, true ));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, false));
  BOOST_CHECK(lddTarget(f,rd,7, true ));
  BOOST_CHECK(lddTarget(f,rd,8, true ));
  BOOST_CHECK(lddTarget(f,rd,9, true ));

  // bottom/right NBs are outside
  f=rd.convert(CellLoc(rd.nrRows()-1,rd.nrCols()-1));

  BOOST_CHECK(lddTarget(f,rd,1, true ));
  BOOST_CHECK(lddTarget(f,rd,2, true ));
  BOOST_CHECK(lddTarget(f,rd,3, true ));
  BOOST_CHECK(lddTarget(f,rd,4, false));
  BOOST_CHECK(lddTarget(f,rd,5, false));
  BOOST_CHECK(lddTarget(f,rd,6, true ));
  BOOST_CHECK(lddTarget(f,rd,7, false));
  BOOST_CHECK(lddTarget(f,rd,8, false));
  BOOST_CHECK(lddTarget(f,rd,9, true ));
*/
}
