#define BOOST_TEST_MODULE pcraster model_engine findsymbol
#include <boost/test/unit_test.hpp>
#include "calc_findsymbol.h"
#include "calc_operator.h"

BOOST_AUTO_TEST_CASE(testOpName2op)
{
  using namespace calc;

  {
    const Operator *op(opName2op("max"));
    BOOST_TEST(op);
    BOOST_TEST(op->opCode() == OP_MAX);
  }
  {
    const Operator *op(opName2op("xxxxxxxxxxxxxxxxxxxxxxx"));
    BOOST_TEST(!op);
  }
  {
    const Operator *op(opName2op("*"));
    BOOST_TEST(op);
    BOOST_TEST(op->opCode() == OP_MUL);
  }
  {
    const Operator *op(opName2op("+"));
    BOOST_TEST(op);
    BOOST_TEST(op->opCode() == OP_BADD);
  }
  {
    const Operator *op(opName2op("+", 1));
    BOOST_TEST(op);
    BOOST_TEST(op->opCode() == OP_UADD);
  }
  {  // MRF inserted correct
    const Operator *op(opName2op("spread"));
    BOOST_TEST(op);
    BOOST_TEST(op->opCode() == OP_SPREAD);
  }
}
