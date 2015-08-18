#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PROGRESSBARTEST
#include "com_progressbartest.h"
#define INCLUDED_COM_PROGRESSBARTEST
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
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
#ifndef INCLUDED_COM_PROGRESSBAR
#include "com_progressbar.h"
#define INCLUDED_COM_PROGRESSBAR
#endif



/*!
  \file
  This file contains the implementation of the ProgressBarTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROGRESSBAR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::ProgressBarTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ProgressBarTest> instance(new ProgressBarTest());

  suite->add(BOOST_CLASS_TEST_CASE(&ProgressBarTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PROGRESSBAR MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::ProgressBarTest::ProgressBarTest()
{
}



//! setUp
void com::ProgressBarTest::setUp()
{
}



//! tearDown
void com::ProgressBarTest::tearDown()
{
}



void com::ProgressBarTest::test()
{
  std::stringstream stream;
  std::string result;

  ProgressBar progressBar(10, 12, stream);
  BOOST_CHECK(progressBar.nrSteps() == 10);
  BOOST_CHECK(progressBar.nrFinishedSteps() == 0);
  BOOST_CHECK(progressBar.width() == 12);

  progressBar.init();
  for(size_t step = 0; step < 10; ++step) {
    progressBar.finishedStep();
    BOOST_CHECK(progressBar.nrFinishedSteps() == step + 1);
    result = (boost::format("[%1%%2%]")
         % std::string(step + 1, '#')
         % std::string(10 - (step + 1), ' ')).str();
    BOOST_CHECK(stream.str().substr(stream.str().find_last_of('\b') + 1) ==
         result);
  }

  BOOST_CHECK(progressBar.nrFinishedSteps() == 10);
  BOOST_CHECK(progressBar.finished());
}
