#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_TRANSIENTTEST
#include "calc_transienttest.h"
#define INCLUDED_CALC_TRANSIENTTEST
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_NUMERIC
#include <numeric>
#define INCLUDED_NUMERIC
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

// PCRaster library headers.
#ifndef INCLUDED_API
#include "api.h"
#define INCLUDED_API
#endif

#ifndef INCLUDED_CSFTYPES
#include "csftypes.h"
#define INCLUDED_CSFTYPES
#endif

// Module headers.
#ifndef INCLUDED_CALC
#include "calc.h"
#define INCLUDED_CALC
#endif



/*!
  \file
  This file contains the implementation of the TransientTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIFFUSE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::TransientTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<TransientTest> instance(new TransientTest());

  suite->add(BOOST_CLASS_TEST_CASE(&TransientTest::testFixedHead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&TransientTest::testBudget, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF DIFFUSE MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::TransientTest::TransientTest()
{
}

// Test if fixed head cells stay constant.
void calc::TransientTest::testFixedHead()
{
  size_t nrRows = 3;
  size_t nrCols = 3;
  size_t nrCells = nrRows * nrCols;
  double cellSize = 15.0;
  CSF_PT projection = PT_YINCT2B;

  BootTestApi(cellSize, projection == PT_YINCT2B);

  // Set cell values.
  REAL4 resultElevationCells[9];
  REAL4 elevationCells[]           = { 1.0F, 2.0F, 3.0F,
                                       4.0F, 5.0F, 6.0F,
                                       7.0F, 8.0F, 9.0F };
  REAL4 rechargeCells[]            = { 1.0F };
  REAL4 transmissivityCells[]      = { 1.0F };
  INT4 flowConditionCells[]        = { 2 };      // Fixed head.
  REAL4 storageCoefficientCells[]  = { 1.0F };
  REAL4 intervalCells[]            = { 1.0F };
  REAL4 toleranceCells[]           = { 0.001F };

  // Create maps.
  MAP_REAL8* elevation = InitMapREAL8(nrRows, nrCols,
                   elevationCells, TRUE, CR_REAL4);
  MAP_REAL8* resultElevation = InitMapREAL8(nrRows, nrCols,
                   resultElevationCells, TRUE, CR_REAL4);
  MAP_REAL8* recharge = InitMapREAL8(nrRows, nrCols,
                   rechargeCells, FALSE, CR_REAL4);
  MAP_REAL8* transmissivity = InitMapREAL8(nrRows, nrCols,
                   transmissivityCells, FALSE, CR_REAL4);
  MAP_INT4* flowCondition = InitMapINT4(nrRows, nrCols,
                   flowConditionCells, FALSE, CR_INT4);
  MAP_REAL8* storageCoefficient = InitMapREAL8(nrRows, nrCols,
                   storageCoefficientCells, FALSE, CR_REAL4);
  MAP_REAL8* interval = InitMapREAL8(nrRows, nrCols,
                   intervalCells, FALSE, CR_REAL4);
  MAP_REAL8* tolerance = InitMapREAL8(nrRows, nrCols,
                   toleranceCells, FALSE, CR_REAL4);

  // Call function.
  void* out[]       = { resultElevation };
  const void* in[]  = { elevation, recharge, transmissivity, flowCondition,
                   storageCoefficient, interval, tolerance };
  BOOST_CHECK(!Transient(out, in, 7));

  // Delete maps.
  DeleteInternalMAP_REAL8(resultElevation);
  DeleteInternalMAP_REAL8(elevation);
  DeleteInternalMAP_REAL8(recharge);
  DeleteInternalMAP_REAL8(transmissivity);
  DeleteInternalMAP_INT4(flowCondition);
  DeleteInternalMAP_REAL8(storageCoefficient);
  DeleteInternalMAP_REAL8(interval);
  DeleteInternalMAP_REAL8(tolerance);

  // Check result.
  BOOST_CHECK(std::equal(elevationCells, elevationCells + nrCells,
                   resultElevationCells));
}



void calc::TransientTest::testBudget()
{
  size_t nrRows = 3;
  size_t nrCols = 3;
  size_t nrCells = nrRows * nrCols;
  double cellSize = 100.0;
  CSF_PT projection = PT_YINCT2B;

  BootTestApi(cellSize, projection == PT_YINCT2B);

  // Set cell values.
  REAL4 resultElevationCells[9];
  REAL4 elevationCells[]           = { 0.0F };    // Flat surface.
  REAL4 rechargeCells[]            = { 0.0F };    // No discharge / recharge.
  REAL4 transmissivityCells[]      = { 300.0F };
  INT4 flowConditionCells[]        = { 1 };      // Calculate head.
  REAL4 storageCoefficientCells[]  = { 0.002F };
  REAL4 intervalCells[]            = { 0.001F };
  REAL4 toleranceCells[]           = { 0.001F };

  // Create maps.
  MAP_REAL8* elevation = InitMapREAL8(nrRows, nrCols,
                   elevationCells, FALSE, CR_REAL4);
  MAP_REAL8* resultElevation = InitMapREAL8(nrRows, nrCols,
                   resultElevationCells, TRUE, CR_REAL4);
  MAP_REAL8* recharge = InitMapREAL8(nrRows, nrCols,
                   rechargeCells, FALSE, CR_REAL4);
  MAP_REAL8* transmissivity = InitMapREAL8(nrRows, nrCols,
                   transmissivityCells, FALSE, CR_REAL4);
  MAP_INT4* flowCondition = InitMapINT4(nrRows, nrCols,
                   flowConditionCells, FALSE, CR_INT4);
  MAP_REAL8* storageCoefficient = InitMapREAL8(nrRows, nrCols,
                   storageCoefficientCells, FALSE, CR_REAL4);
  MAP_REAL8* interval = InitMapREAL8(nrRows, nrCols,
                   intervalCells, FALSE, CR_REAL4);
  MAP_REAL8* tolerance = InitMapREAL8(nrRows, nrCols,
                   toleranceCells, FALSE, CR_REAL4);

  // Prepare arguments.
  void* out[1];
  const void* in[7];
  out[0] = resultElevation;
  in[0] = elevation;
  in[1] = recharge;
  in[2] = transmissivity;
  in[3] = flowCondition;
  in[4] = storageCoefficient;
  in[5] = interval;
  in[6] = tolerance;

  // Call function.
  BOOST_CHECK(!Transient(out, in, 7));

  // Nothing should have changed.
  for(size_t i = 0; i < nrCells; ++i) {
    BOOST_CHECK(resultElevationCells[i] == elevationCells[0]);
  }

  //----------------------------------------------------------------------------
  rechargeCells[0] = 0.2F;         // Add something, everywhere.
  DeleteInternalMAP_REAL8(recharge);
  recharge = InitMapREAL8(nrRows, nrCols, rechargeCells, FALSE, CR_REAL4);
  in[1] = recharge;

  // Call function.
  BOOST_CHECK(!Transient(out, in, 7));

  // No flow: equal amount of stuff on all cells.
  BOOST_CHECK(std::count_if(resultElevationCells, resultElevationCells + nrCells,
         std::bind2nd(std::equal_to<REAL4>(), resultElevationCells[0])));

  // Test amount added to all cells.
  // yepyep

/*
 *  std::cout << endl;
 *  std::cout << elevationCells[0] << endl;
 * std::copy(resultElevationCells, resultElevationCells + nrCells,
 *                 std::ostream_iterator<REAL4>(std::cout, "\t"));
 * std::cout << endl;
 *  cout << rechargeCells[0] * cellSize * cellSize * intervalCells[0] << endl;
 *
 * std::cout << std::accumulate(elevationCells, elevationCells + nrCells, 0.0) << endl;
 * std::cout << std::accumulate(resultElevationCells, resultElevationCells + nrCells, 0.0) << endl;
 * double volumeBefore = std::accumulate(elevationCells,
 *                  elevationCells + nrCells, 0.0);
 * double volumeAfter = std::accumulate(resultElevationCells,
 *                  resultElevationCells + nrCells, 0.0);
 * double volumeAdded = rechargeCells[0] * intervalCells[0];
 *
 * cout << volumeAfter - volumeBefore << '\t' << volumeAdded << endl;
 */


  // Delete maps.
  DeleteInternalMAP_REAL8(resultElevation);
  DeleteInternalMAP_REAL8(elevation);
  DeleteInternalMAP_REAL8(recharge);
  DeleteInternalMAP_REAL8(transmissivity);
  DeleteInternalMAP_INT4(flowCondition);
  DeleteInternalMAP_REAL8(storageCoefficient);
  DeleteInternalMAP_REAL8(interval);
  DeleteInternalMAP_REAL8(tolerance);

/*
  // Check result.
  BOOST_CHECK(std::accumulate(elevationCells, elevationCells + nrCells, 0.0) ==
         std::accumulate(resultElevationCells, resultElevationCells + nrCells, 0.0));
*/
}
