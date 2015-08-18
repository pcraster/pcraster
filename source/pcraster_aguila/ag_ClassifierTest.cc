#include "ag_ClassifierTest.h"

// External headers.
#include <boost/shared_ptr.hpp>
#include <boost/test/test_tools.hpp>
#include <boost/test/unit_test_suite.hpp>

// Project headers.

// Module headers.
#include "com_classifier.h"



/*!
  \file
  This file contains the implementation of the ClassifierTest class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASSIFIERTEST MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* ClassifierTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<ClassifierTest> instance(new ClassifierTest());
  suite->add(BOOST_CLASS_TEST_CASE(
         &ClassifierTest::test, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASSIFIERTEST MEMBERS
//------------------------------------------------------------------------------

//! ctor
ClassifierTest::ClassifierTest()
{
}



void ClassifierTest::test()
{
  // If min and max cutoffs are equal, the classifier must contain one class.
  com::Classifier classifier;
  classifier.installLin();
  classifier.setNrClasses(100);
  classifier.setExtremes(5, 5);
  classifier.setCutoffs(5, 5);
  classifier.classify();

  BOOST_CHECK_EQUAL(classifier.nrClasses(), size_t(0));
  BOOST_CHECK_EQUAL(classifier.nrBorders(), size_t(0));
}

} // namespace ag

