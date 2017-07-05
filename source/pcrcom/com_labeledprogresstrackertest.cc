#define BOOST_TEST_MODULE pcraster com labeled_progress_tracker
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <sstream>
#include "com_labeledprogresstracker.h"
#include "com_progressbar.h"


template<typename T>
class LabeledProgressTrackerWrapper : public com::LabeledProgressTracker<T> {
public:
   LabeledProgressTrackerWrapper(T& tracker, std::string const& label,size_t labelWidth)
   : com::LabeledProgressTracker<T>(tracker, label, labelWidth)
   {
   }

   bool finished() const {
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
