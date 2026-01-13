#define BOOST_TEST_MODULE pcraster com labeled_progress_tracker
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_labeledprogresstracker.h"
#include "com_progressbar.h"
#include <sstream>

template <typename T> class LabeledProgressTrackerWrapper : public com::LabeledProgressTracker<T>
{
public:
  LabeledProgressTrackerWrapper(T &tracker, std::string const &label, size_t labelWidth)
      : com::LabeledProgressTracker<T>(tracker, label, labelWidth)
  {
  }

  bool finished() const
  {
    return com::LabeledProgressTracker<T>::finished();
  }
};

BOOST_AUTO_TEST_CASE(test)
{
  using namespace com;

  std::stringstream stream;
  std::string result;

  ProgressBar progressBar(15, 10, stream);
  LabeledProgressTrackerWrapper<ProgressBar> labeledTracker(progressBar, "task", 6);
  BOOST_TEST(labeledTracker.labelWidth() == 6);
  BOOST_TEST(labeledTracker.width() == 7);
  BOOST_TEST(labeledTracker.nrSteps() == 15);
  BOOST_TEST(labeledTracker.nrFinishedSteps() == 0);

  labeledTracker.init();
  for (size_t step = 0; step < labeledTracker.nrSteps(); ++step) {
    labeledTracker.finishedStep();
    BOOST_TEST(labeledTracker.nrFinishedSteps() == step + 1);
    result = "task   ";
    BOOST_TEST(stream.str().substr(0, labeledTracker.labelWidth() + 1) == result);
  }

  BOOST_TEST(labeledTracker.nrFinishedSteps() == 15);
  BOOST_TEST(labeledTracker.finished());
}
