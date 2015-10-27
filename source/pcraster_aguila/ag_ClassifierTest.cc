#define BOOST_TEST_MODULE pcraster aguila classifier
#include <boost/test/unit_test.hpp>
#include "com_classifier.h"


BOOST_AUTO_TEST_CASE(test)
{
  using namespace com;

  // If min and max cutoffs are equal, the classifier must contain one class.
  Classifier classifier;
  classifier.installLin();
  classifier.setNrClasses(100);
  classifier.setExtremes(5, 5);
  classifier.setCutoffs(5, 5);
  classifier.classify();

  BOOST_CHECK_EQUAL(classifier.nrClasses(), size_t(0));
  BOOST_CHECK_EQUAL(classifier.nrBorders(), size_t(0));
}
