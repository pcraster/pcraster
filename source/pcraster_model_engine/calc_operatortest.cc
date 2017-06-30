#define BOOST_TEST_MODULE pcraster model_engine operator
#include <boost/test/unit_test.hpp>
#include "calc_findsymbol.h"
#include "calc_operator.h"



BOOST_AUTO_TEST_CASE(testFirstFieldInput)
{
  using namespace calc;

  {
    const Operator *op(opName2op("lookupscalar"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 1);
  }
  {
    const Operator *op(opName2op("timeinputscalar"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 1);
  }
  {
    const Operator *op(opName2op("spread"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->firstFieldInput() == 0);
  }
}


BOOST_AUTO_TEST_CASE(testActualInput)
{
  using namespace calc;

  {
    const Operator *op(opName2op("spread"));
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
  }
  {
    const Operator *op(opName2op("lookupscalar"));
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
    BOOST_CHECK(op->actualInput(2)==1);
    BOOST_CHECK(op->actualInput(3)==1);
  }
  {
    const Operator *op(opName2op("argorderwithidarealimited"));
    BOOST_CHECK(op);
    BOOST_CHECK(op->actualInput(0)==0);
    BOOST_CHECK(op->actualInput(1)==1);
    BOOST_CHECK(op->actualInput(2)==2);
    BOOST_CHECK(op->actualInput(3)==0);
    BOOST_CHECK(op->actualInput(4)==1);
    BOOST_CHECK(op->actualInput(5)==2);
    BOOST_CHECK(op->actualInput(6)==0);
  }
}
