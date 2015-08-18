#include "ag_AguilaGuiTest.h"

// External headers.
#include <boost/filesystem.hpp>

// Project headers.
#include "dal_Exception.h"

// Module headers.
#include "ag_Aguila.h"
#include "ag_Viewer.h"



/*!
  \file
  This file contains the implementation of the AguilaGuiTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC AGUILAGUITEST MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF AGUILAGUITEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
AguilaGuiTest::AguilaGuiTest()

  :
    // Cursor Window
    //   CursorView
    //   Data source table
    d_nrVisualisationsPerCursorDialog(3),
    // Map window
    //   Map
    //     Legend view
    //     Map view
    d_nrVisualisationsPerMapWindow(4),
    // Plot window
    //   Plot
    //     Legend view
    //     Plot view
    d_nrVisualisationsPerTimePlotWindow(4),
    d_nrVisualisationsPerAnimationDialog(1)

{
}



void AguilaGuiTest::initTestCase()
{
}



void AguilaGuiTest::cleanupTestCase()
{
}



void AguilaGuiTest::init()
{
}



void AguilaGuiTest::cleanup()
{
  // Viewer::resetInstance();
}



void AguilaGuiTest::testNotExisting(
         std::string const& name)
{
  int argc = 2;
  char const* argv[2] = { "aguila", name.c_str() };

  QVERIFY(!boost::filesystem::exists(name));

  Aguila aguila(argc, const_cast<char**>(argv));
  bool rightExceptionCaught;

  try {
    rightExceptionCaught = false;
    aguila.setup();
  }
  catch(dal::Exception const& exception) {
    rightExceptionCaught = true;
    QCOMPARE(exception.message(),
         std::string("Data source " + name + ":\ncannot be opened"));
  }
  catch(...) {
  }

  QVERIFY(rightExceptionCaught);

  Viewer const& viewer(aguila.viewer());
  QCOMPARE(viewer.nrVisualisations(), static_cast<size_t>(0));

  QVERIFY(!boost::filesystem::exists(name));

  /// Viewer::resetInstance();
}



void AguilaGuiTest::testNotExisting()
{
  testNotExisting("DoesNotExist.map");
  testNotExisting("DoesNotExist");
}



void AguilaGuiTest::testRasterMinMaxEqual()
{
  int argc = 2;
  char const* argv[2] = { "aguila",
         "MinMaxEqual.map" };

  Aguila aguila(argc, const_cast<char**>(argv));
  aguila.setup();
  Viewer const& viewer(aguila.viewer());
  QCOMPARE(viewer.nrVisualisations(), static_cast<size_t>(
         /* d_nrVisualisationsPerCursorDialog + */
         d_nrVisualisationsPerMapWindow));
}



void AguilaGuiTest::testDataset1()
{
  {
    int argc = 2;
    char const* argv[2] = { "aguila",
         "dataset1/aap/scalar_10" };

    Aguila aguila(argc, const_cast<char**>(argv));
    aguila.setup();
    Viewer const& viewer(aguila.viewer());
    QCOMPARE(viewer.nrVisualisations(), size_t(
         /* d_nrVisualisationsPerCursorDialog + */
         d_nrVisualisationsPerMapWindow));
  }

  {
    int argc = 4;
    char const* argv[4] = { "aguila",
         "--scenarios", "{dataset1/aap, dataset1/noot, dataset1/mies}",
         "scalar_10" };

    Aguila aguila(argc, const_cast<char**>(argv));
    aguila.setup();
    Viewer const& viewer(aguila.viewer());
    QCOMPARE(viewer.nrVisualisations(), size_t(
         /* d_nrVisualisationsPerCursorDialog + */
         3 * d_nrVisualisationsPerMapWindow));
  }

  {
    int argc = 6;
    char const* argv[6] = { "aguila",
         "--scenarios", "{dataset1/aap, dataset1/noot, dataset1/mies}",
         "--timesteps", "[1, 30]",
         "scalar" };

    Aguila aguila(argc, const_cast<char**>(argv));
    aguila.setup();
    Viewer const& viewer(aguila.viewer());
    QCOMPARE(viewer.nrVisualisations(), size_t(
         /* d_nrVisualisationsPerCursorDialog + */
         3 * d_nrVisualisationsPerMapWindow // +
         /* d_nrVisualisationsPerAnimationDialog */));
  }

  {
    int argc = 8;
    char const* argv[8] = { "aguila",
         "--multi", "2x2",
         "--scenarios", "{dataset1/aap, dataset1/noot, dataset1/mies}",
         "--timesteps", "[1, 30]",
         "scalar" };

    Aguila aguila(argc, const_cast<char**>(argv));
    aguila.setup();
    Viewer const& viewer(aguila.viewer());
    // MultiMap window
    //   MultiMap
    //     Legend view
    //     MultiMap view
    //     4 * Map view
    QCOMPARE(viewer.nrVisualisations(), size_t(
         /* d_nrVisualisationsPerCursorDialog + */ 8 // +
         /* d_nrVisualisationsPerAnimationDialog */));
  }
}



void AguilaGuiTest::testMultipleViews()
{
  // {
  //   int argc = 7;
  //   char const* argv[7] = { "aguila",
  //        "--timesteps", "[1, 250]",
  //        "--timeGraph", "dem",
  //        "--mapView", "dem"};

  //   Aguila aguila(argc, const_cast<char**>(argv));
  //   aguila.setup();
  //   Viewer const& viewer(aguila.viewer());
  //   // MultiMap window
  //   //   MultiMap
  //   //     Legend view
  //   //     MultiMap view
  //   //     4 * Map view
  //   QCOMPARE(viewer.nrVisualisations(), size_t(
  //        d_nrVisualisationsPerCursorDialog +
  //        d_nrVisualisationsPerAnimationDialog +
  //        d_nrVisualisationsPerTimePlotWindow +
  //        d_nrVisualisationsPerMapWindow));
  // }
}

} // namespace ag
