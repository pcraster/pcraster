#define BOOST_TEST_MODULE pcraster func summary_statistics
#include <boost/test/unit_test.hpp>
#include "dal_MathUtils.h"
#include "func_summarystatistics.h"


BOOST_AUTO_TEST_CASE(mean_)
{
  using namespace func;

  {
    float sources[] = { 100.0f, 200.0f, 300.0f };

    {
      double result;

      mean<float, double>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<double>(result, 200.0));
    }

    {
      float result;

      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 200.0f));

      pcr::setMV(sources[0]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 250.0f));

      pcr::setMV(sources[1]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(dal::comparable<float>(result, 300.0f));

      pcr::setMV(sources[2]);
      mean<float, float>(&sources[0], 3, result);
      BOOST_CHECK(pcr::isMV(result));
    }
  }

  {
    std::vector<float> sources;
    sources.push_back(100.0f);
    sources.push_back(200.0f);
    sources.push_back(300.0f);

    {
      double result;

      mean<std::vector<float>::const_iterator, double>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<double>(result, 200.0));
    }

    {
      float result;

      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 200.0f));

      pcr::setMV(sources[0]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 250.0f));

      pcr::setMV(sources[1]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(dal::comparable<float>(result, 300.0f));

      pcr::setMV(sources[2]);
      mean<std::vector<float>::const_iterator, float>(
         sources.begin(), sources.end(), result);
      BOOST_CHECK(pcr::isMV(result));
    }
  }
}
