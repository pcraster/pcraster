#define BOOST_TEST_MODULE pcraster model_engine astsymboltable
#include <boost/test/unit_test.hpp>
#include "calc_asttestfactory.h"
#include "calc_astscript.h"
#include "calc_globallibdefs.h"


#define EXPECT_ERROR_ID(msgId) \
  { std::unique_ptr<calc::ASTScript> script(calc::ASTTestFactory::createFromIdOrStr(msgId)); \
    TRY_TEST_MSG {                 \
       script->analyzeAndResolve();          \
    } CATCH_TEST_MSG(msgId);       \
  }

struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};

BOOST_FIXTURE_TEST_SUITE(astsymboltable, Fixture)


/// \todo move to ASTScriptTest
BOOST_AUTO_TEST_CASE(testResolve)
{
    EXPECT_ERROR_ID("pcrcalc213b");
    EXPECT_ERROR_ID("pcrcalc10b");
    EXPECT_ERROR_ID("pcrcalc289");
    EXPECT_ERROR_ID("pcrcalc381a");
    EXPECT_ERROR_ID("pcrcalc10c");
}

BOOST_AUTO_TEST_SUITE_END()
