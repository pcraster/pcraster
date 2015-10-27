#define BOOST_TEST_MODULE pcraster dal filesystem_utils
#include <boost/test/unit_test.hpp>
#include <boost/date_time.hpp>
#include <boost/function.hpp>
#include "dal_Exception.h"
#include "dal_FilesystemUtils.h"


BOOST_AUTO_TEST_CASE(path_is_readable)
{
  using namespace dal;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_is_writable)
{
  using namespace dal;

  bool testImplemented = false;
  BOOST_WARN(testImplemented);
}



// void FilesystemUtilsTest::testTestNativePathname()
// {
// #ifdef WIN32
//   // windows, valid ------------------------------------------------------------
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("c:"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("c:\\"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("c:\\bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("c:\\bla\\bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("bla\\bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("/"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative(".bla"));
// 
//   // windows, invalid ----------------------------------------------------------
//   BOOST_CHECK_THROW(testPathnameIsNative(""), Exception);
//   BOOST_CHECK_THROW(testPathnameIsNative("bla."), Exception);
// #else
//   // linux, valid --------------------------------------------------------------
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("/"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("/bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("/bla/bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("bla/bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative(".bla"));
//   BOOST_CHECK_NO_THROW(testPathnameIsNative("bla."));
// 
//   // linux, invalid ------------------------------------------------------------
//   BOOST_CHECK_THROW(testPathnameIsNative(""), Exception);
// #endif
// }


typedef boost::function<boost::filesystem::path
    (boost::filesystem::path const&, size_t)> TimeStepPathVariant;


//! Tests \a variant.
/*!
  \param     variant A timeStepPath variant.

  This function contains tests which were written for the original version of
  timeStepPath (now called timeStepPath83). A newer version exists which is
  called timeStepPath. All tests written for timeStepPath83 should also run
  with timeStepPath and are collected in this function.
*/
void testTimeStepPathVariant(
    TimeStepPathVariant variant)
{
  {
    boost::filesystem::path path("n");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "n0000000.005");
  }

  {
    boost::filesystem::path path("n");
    path = variant(path, 50);
    BOOST_CHECK_EQUAL(path.string(), "n0000000.050");
  }

  {
    boost::filesystem::path path("n");
    path = variant(path, 500);
    BOOST_CHECK_EQUAL(path.string(), "n0000000.500");
  }

  {
    boost::filesystem::path path("n");
    path = variant(path, 5000);
    BOOST_CHECK_EQUAL(path.string(), "n0000005.000");
  }

  {
    boost::filesystem::path path("n");
    path = variant(path, 50000);
    BOOST_CHECK_EQUAL(path.string(), "n0000050.000");
  }
  {
    boost::filesystem::path path("name");
    path = variant(path, 50000000);
    BOOST_CHECK_EQUAL(path.string(), "name0000.000");
  }

  {
    boost::filesystem::path path("name");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "name0000.005");
  }

  {
    boost::filesystem::path path("name.nam");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "name.nam");
  }

  {
    boost::filesystem::path path("namename");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namename.005");
  }

  {
    boost::filesystem::path path("namename.n");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namename.n05");
  }

  {
    boost::filesystem::path path("namename.na");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namename.na5");
  }

  {
    boost::filesystem::path path("namename.nam");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namename.nam");
  }

  {
    boost::filesystem::path path("namename.name");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namename.name");
  }

  {
    boost::filesystem::path path("namenamename");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namenamename");
  }

  {
    boost::filesystem::path path("namenamename.nam");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "namenamename.nam");
  }

  {
    boost::filesystem::path path("n.amenamename.nam");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "n.amenamename.nam");
  }

#ifdef WIN32
  {
    boost::filesystem::path path("C:\\elev");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "C:\\elev0000.005");
  }
  {
    boost::filesystem::path path("adirname\\tmp_s");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "adirname\\tmp_s000.005");
  }
  {
    boost::filesystem::path path("adirname/tmp_s");
    path = variant(path, 5);
    BOOST_CHECK_EQUAL(path.string(), "adirname\\tmp_s000.005");
  }
#endif
  {
    boost::filesystem::path path("n.n");
    path = variant(path, 50000);
    BOOST_WARN_EQUAL(path.string(), "n.n");
  }

}


// I don't believe there is a reason for this function to contain
// individual tests. Put them all in testTimeStepPathVariant.
BOOST_AUTO_TEST_CASE(time_step_path_83)
{
  using namespace dal;

  // Select correct overload.
  boost::filesystem::path (*variant)(boost::filesystem::path const&, size_t) =
      timeStepPath83;
  testTimeStepPathVariant(variant);
}



// boost::filesystem::path f(boost::filesystem::path const& path, size_t time)
// {
//   return timeStepPath(path, time, DALConvention);
// }


BOOST_AUTO_TEST_CASE(time_step_path)
{
  using namespace dal;

  // Check whether the original behaviour of timeStepPath (timeStepPath83) is
  // still supported by timeStepPath.
  // testTimeStepPathVariant(f);

  boost::filesystem::path path;

  {
    boost::gregorian::date date(1971, 4, 17);

    // Default.
    path = timeStepPath("kor", date);
    BOOST_CHECK_EQUAL(path.string(), "kor_19710417");

    // DAL.
    path = timeStepPath("kor", date, DALConvention);
    BOOST_CHECK_EQUAL(path.string(), "kor_19710417");

    // PCR.
    path = timeStepPath("kor", date, PCRConvention);
    BOOST_CHECK_EQUAL(path.string(), "kor19710.417");
  }

  {
    boost::gregorian::date date(1971, 4, 17);

    // DAL.
    path = timeStepPath("name", date, DALConvention);
    BOOST_CHECK_EQUAL(path.string(), "name_19710417");

    // PCR, one character of the timestep is sacreficed in favor of the name.
    path = timeStepPath("name", date, PCRConvention);
    BOOST_CHECK_EQUAL(path.string(), "name9710.417");
  }

  {
    BOOST_CHECK_EQUAL(timeStepPath("bla.png", 3).string(), "bla_3.png");
    BOOST_CHECK_EQUAL(timeStepPath("bla.png", 3, DALConvention).string(), "bla_3.png");
  }

  // See first lines of this function.
  bool timeStepPathVariantTested = false;
  BOOST_WARN(timeStepPathVariantTested);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_quantile_sample_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioQuantileSampleTime("", "aap", 0.75f, 5, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_quantile_sample)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioQuantileSample("", "aap", 0.75f, 5);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_quantile_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioQuantileTime("", "aap", 0.75f, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_quantile)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioQuantile("", "aap", 0.75f);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_sample_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioSampleTime("", "aap", 5, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_sample)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioSample("", "aap", 5);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenarioTime("", "aap", 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_scenario)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForScenario("", "aap");
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // Empty scenario name.
  try {
    exceptionThrown = false;
    pathForScenario("data", "");
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // Directories.
  {
    BOOST_CHECK_EQUAL(pathForScenario("./flow", "aap").generic_string(),
         "./aap/flow");
    BOOST_CHECK_EQUAL(pathForScenario("/flow", "aap").generic_string(),
         "/aap/flow");
    BOOST_CHECK_EQUAL(pathForScenario("dira/flow", "aap").generic_string(),
         "dira/aap/flow");
    BOOST_CHECK_EQUAL(pathForScenario("./dira/flow", "aap").generic_string(),
         "./dira/aap/flow");
    BOOST_CHECK_EQUAL(pathForScenario("../flow", "aap").generic_string(),
         "../aap/flow");
  }

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_quantile_sample_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForQuantileSampleTime("", 0.75f, 5, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_quantile_sample)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForQuantileSample("", 0.75f, 5);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_quantile_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  BOOST_CHECK_EQUAL(pathForQuantileTime("bla", 0.75f, 5).string(),
         "bla_5_0.75");

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForQuantileTime("", 0.75f, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_quantile)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForQuantile("", 0.75f);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_sample_time)
{
  using namespace dal;

  // typedef boost::filesystem::path path;

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForSampleTime("", 5, 33);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_sample)
{
  using namespace dal;

  typedef boost::filesystem::path path;

  BOOST_CHECK(pathForSample("bla", 5) == path("5/bla"));

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathForSample("", 5);
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // Directories.
  {
    BOOST_CHECK_EQUAL(pathForSample("./flow", 5).generic_string(),
         "./5/flow");
    BOOST_CHECK_EQUAL(pathForSample("/flow", 5).generic_string(),
         "/5/flow");
    BOOST_CHECK_EQUAL(pathForSample("dira/flow", 5).generic_string(),
         "dira/5/flow");
    BOOST_CHECK_EQUAL(pathForSample("./dira/flow", 5).generic_string(),
         "./dira/5/flow");
    BOOST_CHECK_EQUAL(pathForSample("../flow", 5).generic_string(),
         "../5/flow");
  }

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for_time)
{
  using namespace dal;

  using namespace boost::filesystem;

  // TODO
  // Default.
  // BOOST_WARN(pathForTime("bla",    5), path("bla_5"));
  // BOOST_WARN(pathForTime("bla", 5555), path("bla_5555"));

  // Dal convention.
  BOOST_CHECK(pathForTime("bla",    5, DALConvention) == path("bla_5"));
  BOOST_CHECK(pathForTime("bla", 5555, DALConvention) == path("bla_5555"));

  // PCRaster convention.
  BOOST_CHECK(pathForTime("bla",    5, PCRConvention) == path("bla00000.005"));
  BOOST_CHECK(pathForTime("bla", 5555, PCRConvention) == path("bla00005.555"));

  // Fieldwidth determines number of positions used for the time step. Time
  // step is prepended with zero's. No extension dot is added.
  // Fieldwidth zero is illegal, fall back to required width.
  // BOOST_WARN(pathForTime("bla", 5, 0), path("bla_5"));
  // BOOST_WARN(pathForTime("bla", 5, 3), path("bla_005"));
  // BOOST_WARN(pathForTime("bla", 5, 8), path("bla_00000005"));

  // When an extension is present in de name of the dataset, it is saved in
  // the result. This limits the freedom the user has when naming files.
  // Dal default.
  BOOST_CHECK(pathForTime("bla.map"    , 5, DALConvention) ==
         path("bla_5.map"));
  BOOST_CHECK(pathForTime("bla.png"    , 5, DALConvention) ==
         path("bla_5.png"));
  BOOST_CHECK(pathForTime("bla.x"      , 5, DALConvention) ==
         path("bla_5.x"));
  // TODO
  // BOOST_CHECK(pathForTime("bla."       , 5, DALConvention) == path("bla_5"));
  BOOST_CHECK(pathForTime("bla.map.png", 5, DALConvention) ==
         path("bla.map_5.png"));

  // The PCRaster convention does not support formatting a path name for
  // a time step, when the name already has an extension of more than 2
  // characters. One or two characters is possible.
  // This is implemented in timeStepPath83 as a feature.
  // Since this feature conflicts with the general ideas of Dal extensions are
  // checked by pathForTime and an assertion fires if they are too big.
  // BOOST_CHECK(pathForTime("bla.map"    , 5, PCRConvention) == path("bla.map"));
  // BOOST_CHECK(pathForTime("bla.png"    , 5, PCRConvention) == path("bla.png"));
  BOOST_CHECK(pathForTime("bla.x" , 5, PCRConvention) == path("bla.x000.005"));
  // TODO
  // BOOST_CHECK(pathForTime("bla."       , 5, PCRConvention) == path("bla.005"));
  // BOOST_CHECK(pathForTime("bla.map.png", 5, PCRConvention) ==
  //        path("bla.map.png"));

  // Using fieldwidth.
  // BOOST_WARN(pathForTime("bla.map", 5, 0) == path("bla_5.map"));
  // BOOST_WARN(pathForTime("bla.map", 5, 3) == path("bla_005.map"));
  // BOOST_WARN(pathForTime("bla.map", 5, 8) == path("bla_00000005.map"));
  // BOOST_WARN(pathForTime("bla.png", 5, 3) == path("bla_005.png"));
  // BOOST_WARN(pathForTime("bla.x"  , 5, 3) == path("bla_005.x"));
  // BOOST_WARN(pathForTime("bla."   , 5, 3) == path("bla_005."));

  bool exceptionThrown;

  // Empty dataset name.
  try {
    exceptionThrown = false;
    pathFor("");
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // TODO
  // Invalid path name.
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);
}


BOOST_AUTO_TEST_CASE(path_for)
{
  using namespace dal;

  BOOST_CHECK(boost::filesystem::path("data") == pathFor("data"));
  BOOST_CHECK(boost::filesystem::path("data.col") == pathFor("data.col"));
  BOOST_CHECK(boost::filesystem::path("12345") == pathFor("12345"));

  bool exceptionThrown;

  try {
    exceptionThrown = false;
    pathFor("");
  }
  catch(Exception const& exception) {
    exceptionThrown = true;
    BOOST_CHECK_EQUAL(exception.message(), "Pathname '': Empty");
  }

  BOOST_CHECK(exceptionThrown);

  // try {
  //   exceptionThrown = false;
  //   pathFor("verzin iets illegaals");
  // }
  // catch(Exception const& exception) {
  //   exceptionThrown = true;
  //   BOOST_CHECK_EQUAL(exception.message(), "Pathname '...': Not valid on the current platform");
  // }

  // BOOST_CHECK(exceptionThrown);
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Add more tests here.
*/
BOOST_AUTO_TEST_CASE(path_for_data_space_address)
{
  using namespace dal;

  using namespace boost::filesystem;

// #ifdef WIN32
//   bool testPathForDataSpaceAddressFails=false;
//   BOOST_WARN(testPathForDataSpaceAddressFails);
// #endif

  DataSpace space;
  DataSpaceAddress address;

  {
    // Empty space and address.
    BOOST_CHECK(pathForDataSpaceAddress("soil", space, address) ==
         path("soil"));
  }

  {
    // Data space with scenarios dimension added. Select one scenario in
    // address.

    // space.addDimension(Dimension(NumericalCoordinates, Time,
    //      RegularDiscretisation, timeSteps));

  }

  // Same for: only sample, only time, only quantile

  // Then all combinations of dimensions.

  // Quantiles.
  {
    std::vector<float> quantiles;
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);
    Dimension cumProbabilities(CumulativeProbabilities, quantiles);
    DataSpace space;
    space.addDimension(cumProbabilities);

    DataSpaceAddress address = space.address();

    address.setCoordinate<float>(0, 0.01f);
    BOOST_CHECK(pathForDataSpaceAddress("nox", space, address) ==
         path("nox_0.01"));

    address.setCoordinate<float>(0, 0.09f);
    BOOST_CHECK(pathForDataSpaceAddress("nox", space, address) ==
         path("nox_0.09"));

    address.setCoordinate<float>(0, 0.90f);
    BOOST_CHECK(pathForDataSpaceAddress("nox", space, address) ==
         path("nox_0.9"));

    address.setCoordinate<float>(0, 0.99f);
    BOOST_CHECK(pathForDataSpaceAddress("nox", space, address) ==
         path("nox_0.99"));
  }

  // Quantiles, timesteps.
  {
    std::vector<float> quantiles;
    quantiles.push_back(0.01f);
    quantiles.push_back(0.99f);
    quantiles.push_back(0.01f);

    std::vector<size_t> timeSteps;
    timeSteps.push_back(1);
    timeSteps.push_back(10);
    timeSteps.push_back(1);

    DataSpace space;
    space.addDimension(Dimension(Time, timeSteps));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSpaceAddress address = space.address();

    address.setCoordinate<size_t>(0, 1);
    address.setCoordinate<float>(1, 0.01f);

    BOOST_CHECK(pathForDataSpaceAddress("nox", space, address, DALConvention) ==
         path("nox_1_0.01"));
    // TODO
    // BOOST_CHECK(pathForDataSpaceAddress("nox", space, address, PCRConvention) ==
    //      path("nox_0.01._01"));

    BOOST_CHECK(pathForDataSpaceAddress("nox.map", space, address,
         DALConvention) == path("nox_1_0.01.map"));
    // TODO
    // BOOST_CHECK(pathForDataSpaceAddress("nox.map", space, address,
    //      PCRConvention) == path("nox.map"));
  }

  // Scenarios, quantiles.
  {
    std::set<std::string> scenarios;
    scenarios.insert("/home/kor/notinbackup/apmosphere/postprocessed/ok");
    scenarios.insert("/home/kor/notinbackup/apmosphere/postprocessed/uk");

    std::vector<float> quantiles;
    quantiles.push_back(0.001f);
    quantiles.push_back(0.999f);
    quantiles.push_back(0.001f);

    DataSpace space;
    space.addDimension(Dimension(Scenarios, scenarios));
    space.addDimension(Dimension(CumulativeProbabilities, quantiles));

    DataSpaceAddress address = space.address();
    address.setCoordinate<std::string>(0,
         "/home/kor/notinbackup/apmosphere/postprocessed/ok");
    address.setCoordinate<float>(1, 0.001f);

    BOOST_CHECK(
         pathForDataSpaceAddress("so2", space, address, DALConvention) ==
         path("/home/kor/notinbackup/apmosphere/postprocessed/ok/so2_0.001"));
    BOOST_CHECK(
         pathForDataSpaceAddress("so2.map", space, address, DALConvention) ==
         path("/home/kor/notinbackup/apmosphere/postprocessed/ok/so2_0.001.map"));
  }

  // Invalid path name.
  // Dal convention.
  // PCRCalc convention.
  // TODO
  // bool testImplemented = false;
  // BOOST_WARN(testImplemented);

  {
    std::string name = "soil";
    DataSpace space;
    std::vector<size_t> timeSteps;
    timeSteps.push_back(size_t(10));
    timeSteps.push_back(size_t(100));
    timeSteps.push_back(size_t(1));
    space.addDimension(Dimension(Time, timeSteps));
    DataSpaceAddress address = space.address();
    address.setCoordinate<size_t>(0, 10);
    BOOST_CHECK_EQUAL(pathForDataSpaceAddress(
         name, space, address, PCRConvention).string(),
         "soil0000.010");
  }
}


BOOST_AUTO_TEST_CASE(add_extension_if_needed)
{
  using namespace dal;

  std::string name;
  std::string extension;
  boost::filesystem::path path;
  {
    // Extension absent.
    name = "name";
    extension = ".extension";
    path = addExtensionIfNeeded(name, extension);
    BOOST_CHECK_EQUAL(path.string(), "name.extension");
  }

#ifndef WIN32
  // Empty extension is not supported on WIN32.
  {
    // Extension empty.
    name = "name.";
    extension = ".extension";
    path = addExtensionIfNeeded(name, extension);
    BOOST_CHECK_EQUAL(path.string(), "name.extension");
  }
#endif

  {
    // Extension present.
    name = "name.bla";
    extension = ".extension";
    path = addExtensionIfNeeded(name, extension);
    BOOST_CHECK_EQUAL(path.string(), "name.bla");
  }

  {
    bool addExtensionIfNeededOnEmptyBaseNameWorks = false;
    BOOST_WARN(addExtensionIfNeededOnEmptyBaseNameWorks);
    // name = ".";
    // extension = ".eas";
    // path = addExtensionIfNeeded(name, extension);
    // BOOST_CHECK_EQUAL(path.string(), ".eas");
  }
}


BOOST_AUTO_TEST_CASE(old_stack_name_2_name_space_tuple)
{
  using namespace dal;

#ifdef WIN32
  boost::tuple<std::string, DataSpace> result;
  std::string name;

  {
    name = "..\\dem.map";
    result = oldStackName2NameSpaceTuple(name);
    BOOST_CHECK_EQUAL(boost::get<0>(result), name);
    BOOST_CHECK_EQUAL(boost::get<1>(result).rank(), 0);
  }

  {
    name = "..\\data\\dem.map";
    result = oldStackName2NameSpaceTuple(name);
    BOOST_CHECK_EQUAL(boost::get<0>(result), name);
    BOOST_CHECK_EQUAL(boost::get<1>(result).rank(), 0);
  }
#endif
}
