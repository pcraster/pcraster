#define BOOST_TEST_MODULE pcraster python tests
#include <boost/test/unit_test.hpp>

#include <Python.h>
#include <boost/python.hpp>


BOOST_AUTO_TEST_CASE(test_python)
{
  int failed = 1;
  try{
    Py_Initialize();
    boost::python::object main = boost::python::import("__main__");
    boost::python::object global(main.attr("__dict__"));
    boost::python::object script = boost::python::exec_file("test.py", global, global);
    boost::python::object result = global["test_result"];
    failed = boost::python::extract<int>(result);
    Py_Finalize();
  }
  catch(...){
     PyErr_Print();
  }
  BOOST_CHECK(failed == 0);
}

