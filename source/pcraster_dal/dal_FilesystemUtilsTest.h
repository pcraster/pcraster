#ifndef INCLUDED_DAL_FILESYSTEMUTILSTEST
#define INCLUDED_DAL_FILESYSTEMUTILSTEST



// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

#ifndef INCLUDED_BOOST_FUNCTION
#include <boost/function.hpp>
#define INCLUDED_BOOST_FUNCTION
#endif

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace dal {
  // FilesystemUtilsTest declarations.
}



namespace dal {

//! This class implements the unit tests for the FilesystemUtils class.
class FilesystemUtilsTest
{

private:

  // typedef boost::filesystem::path (*TimeStepPathVariant) (
  //                                       boost::filesystem::path const&,
  //                                       size_t);

  typedef boost::function<boost::filesystem::path (boost::filesystem::path const&, size_t)> TimeStepPathVariant;

  void             testTimeStepPathVariant(TimeStepPathVariant variant);

public:

                   FilesystemUtilsTest           ();

  void             setUp               ();

  void             tearDown            ();

  void             testTestPathIsReadable();

  void             testTestPathIsWritable();

  // void             testTestNativePathname();

  void             testTimeStepPath83  ();

  void             testTimeStepPath    ();

  void             testPathForDataSpaceAddress();

  void             testPathForScenarioQuantileSampleTime();

  void             testPathForScenarioQuantileSample();

  void             testPathForScenarioQuantileTime();

  void             testPathForScenarioQuantile();

  void             testPathForScenarioSampleTime();

  void             testPathForScenarioSample();

  void             testPathForScenarioTime();

  void             testPathForScenario ();

  void             testPathForQuantileSampleTime();

  void             testPathForQuantileSample();

  void             testPathForQuantileTime();

  void             testPathForQuantile ();

  void             testPathForSampleTime();

  void             testPathForSample   ();

  void             testPathForTime     ();

  void             testPathFor         ();

  void             testAddExtensionIfNeeded();

  void             testOldStackName2NameSpaceTuple();

  static boost::unit_test::test_suite* suite();

};

} // namespace dal

#endif
