#ifndef INCLUDED_DAL_DATASPACEITERATORTEST
#define INCLUDED_DAL_DATASPACEITERATORTEST



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACE
#include "dal_DataSpace.h"
#define INCLUDED_DAL_DATASPACE
#endif



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // DataSpaceIterator declarations.
}



namespace dal {



//! This class implements the unit tests for the DataSpaceIterator class.
class DataSpaceIteratorTest
{

private:

  Dimension        d_scenarios,
                   d_quantiles,
                   d_samples,
                   d_timeSteps,
                   d_raster,
                   d_feature;

  DataSpace        d_space;

public:

                   DataSpaceIteratorTest();

  void             setUp               ();

  void             tearDown            ();

  void             testEmptySpace      ();

  void             testScenarios       ();

  void             testRangeOfCumProbabilities();

  void             testSamples         ();

  void             testTime            ();

  void             testSpace           ();

  void             testFeatureSpace    ();

  void             testScenariosSamples();

  void             testScenarioCumProbabilities();

  void             testSpaceWithEmptyDimensions();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
