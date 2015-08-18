#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_PYTHON
#include <Python.h>
#define INCLUDED_PYTHON
#endif

#ifndef INCLUDED_BOOST_PYTHON
#include <boost/python.hpp>
#define INCLUDED_BOOST_PYTHON
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRCALCPY_PYTHONUNITTEST
#include "pcrcalcpy_pythonunittest.h"
#define INCLUDED_PCRCALCPY_PYTHONUNITTEST
#endif

/*!
  \file
  This file contains the implementation of the PythonUnitTest class.
*/

// NOTE use string failureExpected in files expected to fail, see style guide

namespace pcrcalcpy {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PYTHONUNIT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite* PythonUnitTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<PythonUnitTest> instance(new PythonUnitTest());

  // do not run debug tests on Windows
  // or figure out how calling Python_d.exe from here works
#ifdef _WIN32
  #ifndef _DEBUG
    suite->add(BOOST_CLASS_TEST_CASE(&PythonUnitTest::test, instance));
  #endif
  // dummy test inserted to shut up empty test tree failure on debug
  suite->add(BOOST_CLASS_TEST_CASE(&PythonUnitTest::dummy, instance));
#else
  suite->add(BOOST_CLASS_TEST_CASE(&PythonUnitTest::test, instance));
#endif

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF PYTHONUNIT MEMBERS
//------------------------------------------------------------------------------

//! ctor
PythonUnitTest::PythonUnitTest()
{
}


void PythonUnitTest::dummy()
{
  BOOST_CHECK(0 == 0);
}


void PythonUnitTest::test()
{
  int passed = 1;
  try{
    Py_Initialize();
    boost::python::object main = boost::python::import("__main__");
    boost::python::object global(main.attr("__dict__"));
    boost::python::object script = boost::python::exec_file("test.py", global, global);
    boost::python::object result = global["test_result"];
    passed = boost::python::extract<int>(result);
    Py_Finalize();
  }
  catch(...){
     PyErr_Print();
  }
  BOOST_CHECK(passed == 0);
}

} // namespace pcrcalcpy
