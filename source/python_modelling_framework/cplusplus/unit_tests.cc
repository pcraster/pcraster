#define BOOST_TEST_MODULE python modelling framework
#include <boost/test/unit_test.hpp>

#include <boost/python.hpp>


BOOST_AUTO_TEST_CASE(modelling_framework)
{
  Py_Initialize();
  boost::python::object main = boost::python::import("__main__");
  boost::python::object global(main.attr("__dict__"));
  boost::python::object script = boost::python::exec_file("unitTests.py", global, global);
  boost::python::object result = global["test_result"];
  int passed = boost::python::extract<int>(result);
  Py_Finalize();
  BOOST_CHECK(passed == 0);
}
