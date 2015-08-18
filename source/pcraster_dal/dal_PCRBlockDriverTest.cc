#ifndef INCLUDED_DAL_PCRBLOCKDRIVERTEST
#include "dal_PCRBlockDriverTest.h"
#define INCLUDED_DAL_PCRBLOCKDRIVERTEST
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
#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_PCRBLOCKDRIVER
#include "dal_PCRBlockDriver.h"
#define INCLUDED_DAL_PCRBLOCKDRIVER
#endif



/*!
  \file
  This file contains the implementation of the PCRBlockDriverTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PCRBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*PCRBlockDriverTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PCRBlockDriverTest> instance(new PCRBlockDriverTest());

  suite->add(BOOST_CLASS_TEST_CASE(&PCRBlockDriverTest::testOpen, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PCRBlockDriverTest::testRead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&PCRBlockDriverTest::testWrite, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PCRBLOCKDRIVER MEMBERS
//------------------------------------------------------------------------------

//! ctor
PCRBlockDriverTest::PCRBlockDriverTest()
{
}



//! setUp
void PCRBlockDriverTest::setUp()
{
  // Write some block data to have something to play with in the tests.
  // Yes, bootstrapping problem. We need write to be able to test open and
  // read, but write might not be ok. Assume that when all tests succeed all
  // is fine.
  size_t nrRows = 3;
  size_t nrCols = 2;
  double cellSize = 1.0;
  double west = 4.0;
  double north = 5.0;

  d_blockDiscretisation = new Block(nrRows, nrCols, cellSize, west, north);
  d_blockDiscretisation->createCells();
  Raster* elevation = new Raster(nrRows, nrCols, cellSize, west, north,
         TI_REAL4);
  elevation->createCells();

  for(size_t i = 0; i < elevation->nrCells(); ++i) {
    elevation->cell<REAL4>(i) = REAL4(i);
    REAL4_VECTOR& stack(d_blockDiscretisation->cell<REAL4_VECTOR>(i));
    stack.insert(stack.end(), i, REAL4(i));
  }

  // elevation:
  // 0 1
  // 2 3
  // 4 5

  // thicknesses:
  // - [1]
  // [2,2] [3,3,3]
  // [4,4,4,4] [5,5,5,5,5]

  d_blockDiscretisation->setBaseElevation(elevation);

  PCRBlockDriver driver;
  static_cast<BlockDriver&>(driver).write(
         *d_blockDiscretisation, "discretisation.pcrblock");
}



//! tearDown
void PCRBlockDriverTest::tearDown()
{
  delete d_blockDiscretisation;
}



void PCRBlockDriverTest::testOpen()
{
  setUp();
  PCRBlockDriver driver;
  Block* block;

  // Not existing file.
  {
    block = dynamic_cast<Block*>(
         dynamic_cast<Driver&>(driver).open("notexisting"));
    BOOST_CHECK(!block);
  }

  // Block created in setUp()
  {
    block = dynamic_cast<Block*>(
         dynamic_cast<Driver&>(driver).open("discretisation.pcrblock"));

    BOOST_CHECK(block);
    BOOST_CHECK_EQUAL(block->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(block->nrCols(), size_t(2));
    BOOST_CHECK(dal::comparable(block->cellSize(), 1.0));
    BOOST_CHECK(dal::comparable(block->west(), 4.0));
    BOOST_CHECK(dal::comparable(block->north(), 5.0));

    delete block;
  }
  tearDown();
}



void PCRBlockDriverTest::testRead()
{
  setUp();
  PCRBlockDriver driver;
  Block* block;

  // Not existing file.
  {
    bool exceptionCaught = false;
    block = 0;

    try {
      block = static_cast<BlockDriver&>(driver).read("notexisting");
    }
    catch(Exception const& exception) {
      exceptionCaught = true;
      BOOST_CHECK_EQUAL(exception.message(),
         "Data source notexisting(block):\ncannot be opened");
    }

    BOOST_CHECK(exceptionCaught);
    BOOST_CHECK(!block);
  }

  // Block created in setUp()
  {
    block = static_cast<BlockDriver&>(driver).read("discretisation.pcrblock");
    BOOST_CHECK(block);

    BOOST_CHECK_EQUAL(block->nrRows(), size_t(3));
    BOOST_CHECK_EQUAL(block->nrCols(), size_t(2));
    BOOST_CHECK(dal::comparable(block->cellSize(), 1.0));
    BOOST_CHECK(dal::comparable(block->west(), 4.0));
    BOOST_CHECK(dal::comparable(block->north(), 5.0));

    BOOST_CHECK(block->containsDiscretisationInfo());
    BOOST_CHECK(!block->containsData());
    BOOST_CHECK(block->baseElevation());

    Raster const& elevation(*block->baseElevation());

    for(size_t i = 0; i < elevation.nrCells(); ++i) {
      BOOST_CHECK(dal::comparable(elevation.cell<REAL4>(i), REAL4(i)));

      REAL4_VECTOR& stack(block->cell<REAL4_VECTOR>(i));

      BOOST_CHECK_EQUAL(stack.size(), i);

      for(size_t j = 0; j < stack.size(); ++j) {
        BOOST_CHECK(dal::comparable(stack[j], REAL4(i)));
      }
    }

    delete block;
  }
  tearDown();
}



void PCRBlockDriverTest::testWrite()
{
  // setUp() uses write, if testOpen and testRead succeed, than write is ok
  // too.
}

} // namespace dal

