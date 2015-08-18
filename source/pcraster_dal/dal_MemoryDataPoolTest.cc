#ifndef INCLUDED_DAL_MEMORYDATAPOOLTEST
#include "dal_MemoryDataPoolTest.h"
#define INCLUDED_DAL_MEMORYDATAPOOLTEST
#endif

// Library headers.
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

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_MEMORYDATAPOOL
#include "dal_MemoryDataPool.h"
#define INCLUDED_DAL_MEMORYDATAPOOL
#endif



/*!
  \file
  This file contains the implementation of the MemoryDataPoolTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MEMORYDATAPOOL MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*MemoryDataPoolTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MemoryDataPoolTest> instance(new MemoryDataPoolTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MemoryDataPoolTest::testFillingOfDataSpace, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MEMORYDATAPOOL MEMBERS
//------------------------------------------------------------------------------

//! ctor
MemoryDataPoolTest::MemoryDataPoolTest(
         )
{
}



//! setUp
void MemoryDataPoolTest::setUp()
{
}



//! tearDown
void MemoryDataPoolTest::tearDown()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Implement.
  \todo      Think life cycle through of the data added.
*/
void MemoryDataPoolTest::testFillingOfDataSpace()
{
  {
    MemoryDataPool pool;

    // Create a data space to use for the data. This is the space which will
    // be filled. Initialise room for this data set in de data pool. Actually
    // adding data is an option at this moment.
    std::vector<size_t> timeSteps;
    timeSteps.push_back(1);
    timeSteps.push_back(10);
    timeSteps.push_back(1);
    DataSpace space;
    space.addDimension(Dimension(Time, timeSteps));
    size_t nrRows = 3;
    size_t nrCols = 2;
    double cellSize = 15.0;
    double north = 8.0;
    double west = 9.0;
    TypeId typeId = TI_REAL4;
    MemoryRasterData data(space, typeId, nrRows, nrCols, cellSize,
         north, west);
    BOOST_CHECK(!pool.rasterExists("dem", space));
    pool.add("dem", data);

    // Check for availabiliby of the raster.
    BOOST_CHECK(pool.rasterExists("dem", space));
    data = pool.raster("dem", space);
    BOOST_CHECK(!data.exists());

    REAL4 t1[6] = {  1.0,  2.0,  3.0,  4.0,  5.0,  6.0 };
    // REAL4 t5[6] = {  7.0,  8.0,  9.0, 10.0, 11.0, 12.0 };
    // REAL4 t8[6] = { 13.0, 14.0, 15.0, 16.0, 17.0, 18.0 };

    // Create a raster to use at a specific location in the space. Add it to
    // the pool.
    std::vector<boost::any> values;
    values.push_back(static_cast<REAL4*>(t1));
    DataSpaceAddress address(space.address());
    address.setCoordinate<size_t>(0, 1);

    // Not needed, but dumps:
    // MemoryRasterData(values, space, typeId, nrRows, nrCols, cellSize, north, west);
    // pool.add("dem", MemoryRasterData(values, space, typeId, nrRows, nrCols,
    //      cellSize, north, west), address);

    // Check for availabiliby of the added raster.
    // BOOST_CHECK(pool.rasterExists("dem", space));
    // data = pool.raster("dem", space);
    // BOOST_CHECK(data.exists());
    // BOOST_CHECK(data.exists(address));

    // Create and add another raster at another location.

    // Check for availability of the added rasters.

    // Create and add another raster at an occupied location.

    // Check whether overwriting of data succeeded.

    // Remove a raster at a specific location.

    // Check current contents of the pool.

    // Remove a raster at a specific location.

    // Check current contents of the pool.

    // Remove the whole data set.

    // Check whether pool is empty.
  }

  {
    // Check co-existence of data sets with the same name but different spaces.
  }
}

} // namespace dal

