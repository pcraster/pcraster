#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKERTEST
#include "com_labeledprogresstrackertest.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKERTEST
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
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

// Module headers.
#ifndef INCLUDED_COM_LABELEDPROGRESSTRACKER
#include "com_labeledprogresstracker.h"
#define INCLUDED_COM_LABELEDPROGRESSTRACKER
#endif

#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif



/*!
  \file
  This file contains the implementation of the LabeledProgressTrackerTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC LABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::LabeledProgressTrackerTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<LabeledProgressTrackerTest> instance(new LabeledProgressTrackerTest());

  suite->add(BOOST_CLASS_TEST_CASE(&LabeledProgressTrackerTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF LABELEDPROGRESSTRACKER MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::LabeledProgressTrackerTest::LabeledProgressTrackerTest()
{
}



//! setUp
void com::LabeledProgressTrackerTest::setUp()
{
}



//! tearDown
void com::LabeledProgressTrackerTest::tearDown()
{
}



void com::LabeledProgressTrackerTest::test()
{
  std::stringstream stream;
  std::string result;

  ProgressBar progressBar(15, 10, stream);
  LabeledProgressTracker<ProgressBar> labeledTracker(progressBar, "task", 6);
  BOOST_CHECK(labeledTracker.labelWidth() == 6);
  BOOST_CHECK(labeledTracker.width() == 7);
  BOOST_CHECK(labeledTracker.nrSteps() == 15);
  BOOST_CHECK(labeledTracker.nrFinishedSteps() == 0);

  labeledTracker.init();
  for(size_t step = 0; step < labeledTracker.nrSteps(); ++step) {
    labeledTracker.finishedStep();
    BOOST_CHECK(labeledTracker.nrFinishedSteps() == step + 1);
    result = "task   ";
    BOOST_CHECK(stream.str().substr(0, labeledTracker.labelWidth() + 1) ==
         result);
  }

  BOOST_CHECK(labeledTracker.nrFinishedSteps() == 15);
  BOOST_CHECK(labeledTracker.finished());
}
