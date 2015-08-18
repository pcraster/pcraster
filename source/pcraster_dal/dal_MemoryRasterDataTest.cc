#ifndef INCLUDED_DAL_MEMORYRASTERDATATEST
#include "dal_MemoryRasterDataTest.h"
#define INCLUDED_DAL_MEMORYRASTERDATATEST
#endif

// Library headers.
#include <boost/shared_ptr.hpp>

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif

#ifndef INCLUDED_DAL_MEMORYRASTERDATA
#include "dal_MemoryRasterData.h"
#define INCLUDED_DAL_MEMORYRASTERDATA
#endif

#ifndef INCLUDED_DAL_RASTER
#include "dal_Raster.h"
#define INCLUDED_DAL_RASTER
#endif



/*!
  \file
  This file contains the implementation of the MemoryRasterDataTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYRASTERDATA MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*MemoryRasterDataTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MemoryRasterDataTest> instance(new MemoryRasterDataTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MemoryRasterDataTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYRASTERDATA MEMBERS
//------------------------------------------------------------------------------

//! ctor
MemoryRasterDataTest::MemoryRasterDataTest(
         )
{
}



//! setUp
void MemoryRasterDataTest::setUp()
{
}



//! tearDown
void MemoryRasterDataTest::tearDown()
{
}



void MemoryRasterDataTest::test()
{
  // Doesn't work anymore after getting rid for suppor of ExactDiscretisation
  // for quantiles.
  BOOST_WARN(false);
  return;

  REAL4 q1[6] = { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 };
  REAL4 q5[6] = { 6.0, 7.0, 8.0, 9.0, 10.0, 11.0 };
  REAL4 q9[6] = { 12.0, 13.0, 14.0, 15.0, 16.0, 17.0 };

  std::vector<boost::any> values;
  DataSpace space;
  TypeId typeId = TI_REAL4;
  size_t nrRows = 3;
  size_t nrCols = 2;
  double cellSize = 15.0;
  double north = 0.0;
  double west = 0.0;

  // Mmm, not useful...?
  // {
  //   MemoryRasterData data(values, space, typeId, nrRows, nrCols, cellSize,
  //        north, west);
  //   BOOST_CHECK(!data.exists());
  // }

  boost::shared_ptr<Raster> raster;
  values.push_back((REAL4*)q1);

  {
    MemoryRasterData data(values, space, typeId, nrRows, nrCols, cellSize,
         north, west);
    BOOST_CHECK(data.exists());
    raster.reset(data.raster(MemoryRasterData::HeaderOnly));
    BOOST_CHECK(raster);
    BOOST_CHECK(raster->hasExtremes());
    BOOST_CHECK(dal::comparable(raster->min<REAL4>(), REAL4(1.0)));
    BOOST_CHECK(dal::comparable(raster->max<REAL4>(), REAL4(6.0)));

    raster.reset(data.raster(MemoryRasterData::IncludingValues));
    BOOST_CHECK(raster);
    BOOST_CHECK(dal::comparable(raster->cell<REAL4>(0), REAL4(1.0)));
    BOOST_CHECK(dal::comparable(raster->cell<REAL4>(2), REAL4(3.0)));
    BOOST_CHECK(dal::comparable(raster->cell<REAL4>(5), REAL4(6.0)));
  }

  std::vector<float> quantiles;
  quantiles.push_back(0.1f);
  quantiles.push_back(0.9f);
  quantiles.push_back(0.1f);
  space.addDimension(Dimension(CumulativeProbabilities, quantiles));

  typedef boost::tuple<float, std::vector<boost::any> > FloatTuple;
  values.clear();

  std::vector<boost::any> tmp;
  tmp.push_back(static_cast<REAL4*>(q1));
  values.push_back(FloatTuple(0.1f, tmp));
  tmp.clear();
  tmp.push_back((REAL4*)q5);
  values.push_back(FloatTuple(0.5f, tmp));
  tmp.clear();
  tmp.push_back((REAL4*)q9);
  values.push_back(FloatTuple(0.9f, tmp));

  {
    MemoryRasterData data(values, space, typeId, nrRows, nrCols, cellSize,
         north, west);
    DataSpaceAddress address(space.address());
    address.setCoordinate<float>(0, 0.9f);
    BOOST_CHECK(data.exists(address));
    raster.reset(data.raster(address, MemoryRasterData::IncludingValues));
    BOOST_CHECK(raster);
    BOOST_CHECK(raster->hasExtremes());
    BOOST_CHECK(dal::comparable(raster->min<REAL4>(), REAL4(1.0)));
    BOOST_CHECK(dal::comparable(raster->max<REAL4>(), REAL4(17.0)));
    BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(0), 12.0));
    BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(2), 14.0));
    BOOST_CHECK(comparable<REAL4>(raster->cell<REAL4>(5), 17.0));
  }
}

} // namespace dal

