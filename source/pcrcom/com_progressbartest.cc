#define BOOST_TEST_MODULE pcraster com progress_bar
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <sstream>
#include <boost/format.hpp>
#include "com_progressbar.h"

class ProgressBarWrapper : public com::ProgressBar {
public:
  ProgressBarWrapper(size_t nrSteps, size_t width, std::ostream& stream)
    : com::ProgressBar(nrSteps, width, stream) {
  }

  size_t width() const {
    return ProgressBar::width();
  }

  bool finished() const {
    return ProgressBar::finished();
  }
};


BOOST_AUTO_TEST_CASE(test)
{
  using namespace com;

  std::stringstream stream;
  std::string result;

  ProgressBarWrapper progressBar(10, 12, stream);
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
