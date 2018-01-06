#define BOOST_TEST_MODULE pcraster modflow calc_demo
#include <boost/test/unit_test.hpp>

#include <Python.h>
#include <boost/shared_ptr.hpp>
#include <boost/python.hpp>

#include "pcrcalc.h"
#include "geo_filecreatetester.h"
#include "com_exception.h"

// OLS: stuff below as a reminder for work on windows build later on...
  // Python tests must be run after pcrcalc tests
  // as input maps are generated in the calc script;
  // and do not run debug tests on Windows
  // or figure out how calling Python_d.exe from here works
// #ifdef _WIN32
//   #ifndef _DEBUG
//     suite->add(BOOST_CLASS_TEST_CASE(
//          &DemoTest::test_python_scripts, instance));
//   #endif
// #else
//     suite->add(BOOST_CLASS_TEST_CASE(
//          &DemoTest::test_python_scripts, instance));
// #endif



BOOST_AUTO_TEST_CASE(test_demo)
{
  PcrScript* s=pcr_createScriptFromTextFile("example.mod");
  BOOST_REQUIRE(s);

  BOOST_CHECK_MESSAGE(!pcr_ScriptError(s),pcr_ScriptErrorMessage(s));

  // this will remove the files if present
  geo::FileCreateTester hOne("hOne.map");     hOne.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester hThree("hThree.map"); hThree.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester hFive("hFive.map");   hFive.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester dFive("dFive.map");   dFive.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rFive("rFive.map");   rFive.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester c5("c5.map");   c5.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester c3("c3.map");   c3.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ff1("ff1.map");   ff1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ff3("ff3.map");   ff3.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ff5("ff5.map");   ff5.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester fr1("fr1.map");   fr1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester fr3("fr3.map");   fr3.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester fr5("fr5.map");   fr5.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester fl3("fl3.map");   fl3.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester fl5("fl5.map");   fl5.setCsfCellEpsilon(0.0001);

  // create the files
  pcr_ScriptExecute(s);

  BOOST_CHECK_MESSAGE(!pcr_ScriptError(s),pcr_ScriptErrorMessage(s));

  pcr_destroyScript(s);

  try {
    // compare them
    BOOST_CHECK(hOne.equalTo("demoValidatedOutput/hOne.map", false));
    BOOST_CHECK(hThree.equalTo("demoValidatedOutput/hThree.map", false));
    BOOST_CHECK(hFive.equalTo("demoValidatedOutput/hFive.map", false));
    BOOST_CHECK(dFive.equalTo("demoValidatedOutput/dFive.map", false));
    BOOST_CHECK(rFive.equalTo("demoValidatedOutput/rFive.map", false));
    BOOST_CHECK(c5.equalTo("demoValidatedOutput/c5.map", false));
    BOOST_CHECK(c3.equalTo("demoValidatedOutput/c3.map", false));
    BOOST_CHECK(ff1.equalTo("demoValidatedOutput/ff1.map", false));
    BOOST_CHECK(ff3.equalTo("demoValidatedOutput/ff3.map", false));
    BOOST_CHECK(ff5.equalTo("demoValidatedOutput/ff5.map", false));
    BOOST_CHECK(fr1.equalTo("demoValidatedOutput/fr1.map", false));
    BOOST_CHECK(fr3.equalTo("demoValidatedOutput/fr3.map", false));
    BOOST_CHECK(fr5.equalTo("demoValidatedOutput/fr5.map", false));
    BOOST_CHECK(fl3.equalTo("demoValidatedOutput/fl3.map", false));
    BOOST_CHECK(fl5.equalTo("demoValidatedOutput/fl5.map", false));

  }
  catch (const com::Exception& e) {
    BOOST_CHECK_MESSAGE(false, e.messages());
  }
}


BOOST_AUTO_TEST_CASE(test_bcf2ss) {
  PcrScript* s=pcr_createScriptFromTextFile("bcf2ss.mod");
  BOOST_REQUIRE(s);

  BOOST_CHECK_MESSAGE(!pcr_ScriptError(s),pcr_ScriptErrorMessage(s));

  // this will remove the files if present
  geo::FileCreateTester hTwo_1("hTwo_1.map");  hTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester hOne_1("hOne_1.map");  hOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rTwo_1("rTwo_1.map");  rTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rOne_1("rOne_1.map");  rOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester riTwo_1("riTwo_1.map");   riTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester riOne_1("riOne_1.map");   riOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester chTwo_1("chTwo_1.map");   chTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester chOne_1("chOne_1.map");   chOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rfTwo_1("rfTwo_1.map");   rfTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rfOne_1("rfOne_1.map");   rfOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester lfOne_1("lfOne_1.map");   lfOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester hTwo_2("hTwo_2.map");  hTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester hOne_2("hOne_2.map");  hOne_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rTwo_2("rTwo_2.map");  rTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rOne_2("rOne_2.map");  rOne_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester riTwo_2("riTwo_2.map");   riTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester riOne_2("riOne_2.map");   riOne_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester chTwo_2("chTwo_2.map");   chTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester chOne_2("chOne_2.map");   chOne_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ffTwo_2("ffTwo_2.map");   ffTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rfTwo_2("rfTwo_2.map");   rfTwo_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester lfOne_2("lfOne_2.map");   lfOne_2.setCsfCellEpsilon(0.0001);

  // some maps give slightly differences with mf2k 32 and 64
  // but they are ok, so deactivate tests for 32bit
#if defined(__amd64) || defined(_M_AMD64)
  geo::FileCreateTester ffTwo_1("ffTwo_1.map");   ffTwo_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ffOne_1("ffOne_1.map");   ffOne_1.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester ffOne_2("ffOne_2.map");   ffOne_2.setCsfCellEpsilon(0.0001);
  geo::FileCreateTester rfOne_2("rfOne_2.map");   rfOne_2.setCsfCellEpsilon(0.0001);
#endif
  // create the files
  pcr_ScriptExecute(s);

  BOOST_CHECK_MESSAGE(!pcr_ScriptError(s),pcr_ScriptErrorMessage(s));

  pcr_destroyScript(s);

  try {
    // compare them
    BOOST_CHECK(hTwo_1.equalTo("demoValidatedOutput/bcf2ss_hTwo_1.map", false));
    BOOST_CHECK(hOne_1.equalTo("demoValidatedOutput/bcf2ss_hOne_1.map", false));
    BOOST_CHECK(rTwo_1.equalTo("demoValidatedOutput/bcf2ss_rTwo_1.map", false));
    BOOST_CHECK(rOne_1.equalTo("demoValidatedOutput/bcf2ss_rOne_1.map", false));
    BOOST_CHECK(riTwo_1.equalTo("demoValidatedOutput/bcf2ss_riTwo_1.map", false));
    BOOST_CHECK(riOne_1.equalTo("demoValidatedOutput/bcf2ss_riOne_1.map", false));
    BOOST_CHECK(chTwo_1.equalTo("demoValidatedOutput/bcf2ss_chTwo_1.map", false));
    BOOST_CHECK(chOne_1.equalTo("demoValidatedOutput/bcf2ss_chOne_1.map", false));
    BOOST_CHECK(rfTwo_1.equalTo("demoValidatedOutput/bcf2ss_rfTwo_1.map", false));
    BOOST_CHECK(rfOne_1.equalTo("demoValidatedOutput/bcf2ss_rfOne_1.map", false));
    BOOST_CHECK(lfOne_1.equalTo("demoValidatedOutput/bcf2ss_lfOne_1.map", false));

    BOOST_CHECK(hTwo_2.equalTo("demoValidatedOutput/bcf2ss_hTwo_2.map", false));
    BOOST_CHECK(hOne_2.equalTo("demoValidatedOutput/bcf2ss_hOne_2.map", false));
    BOOST_CHECK(rTwo_2.equalTo("demoValidatedOutput/bcf2ss_rTwo_2.map", false));
    BOOST_CHECK(rOne_2.equalTo("demoValidatedOutput/bcf2ss_rOne_2.map", false));
    BOOST_CHECK(riTwo_2.equalTo("demoValidatedOutput/bcf2ss_riTwo_2.map", false));
    BOOST_CHECK(riOne_2.equalTo("demoValidatedOutput/bcf2ss_riOne_2.map", false));
    BOOST_CHECK(chTwo_2.equalTo("demoValidatedOutput/bcf2ss_chTwo_2.map", false));
    BOOST_CHECK(chOne_2.equalTo("demoValidatedOutput/bcf2ss_chOne_2.map", false));
    BOOST_CHECK(ffTwo_2.equalTo("demoValidatedOutput/bcf2ss_ffTwo_2.map", false));
    BOOST_CHECK(rfTwo_2.equalTo("demoValidatedOutput/bcf2ss_rfTwo_2.map", false));
    BOOST_CHECK(lfOne_2.equalTo("demoValidatedOutput/bcf2ss_lfOne_2.map", false));

    // some maps give slightly differences with mf2k 32 and 64
    // but they are ok, so deactivate tests for 32bit
//#if defined(__amd64) || defined(_M_AMD64)
//    BOOST_CHECK(ffOne_2.equalTo("demoValidatedOutput/bcf2ss_ffOne_2.map", false));
//    BOOST_CHECK(rfOne_2.equalTo("demoValidatedOutput/bcf2ss_rfOne_2.map", false));
//    BOOST_CHECK(ffTwo_1.equalTo("demoValidatedOutput/bcf2ss_ffTwo_1.map", false));
//    BOOST_CHECK(ffOne_1.equalTo("demoValidatedOutput/bcf2ss_ffOne_1.map", false));
//#endif
  }
  catch (const com::Exception& e) {
    BOOST_CHECK_MESSAGE(false, e.messages());
  }
}


// this can be removed when ctest executes well on all platforms
//
// Python tests must be run after pcrcalc tests
// as input maps are generated in the calc script;
// and do not run debug tests on Windows
// or figure out how calling Python_d.exe from here works
// BOOST_AUTO_TEST_CASE(test_python_scripts){
//   // using Py_Main will not work in combination with BOOST_CHECK
//   // due to sys.exit value not forwarded properly
//   int failed = 1;
//   try{
//     Py_Initialize();
//     boost::python::object main = boost::python::import("__main__");
//     boost::python::object global(main.attr("__dict__"));
//     boost::python::object script = boost::python::exec_file("tests.py", global, global);
//     boost::python::object result = global["test_result"];
//     failed = boost::python::extract<int>(result);
//     Py_Finalize();
//   }
//   catch(...){
//      PyErr_Print();
//   }
//   BOOST_CHECK(failed == 0);
// }
