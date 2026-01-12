#define BOOST_TEST_MODULE pcraster mathx dir_conv
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "mathx.h"


BOOST_AUTO_TEST_CASE(scale_deg)
{
    BOOST_TEST(ScaleDeg(45) == 45);
    BOOST_TEST(ScaleDeg(-300) == 60);

    BOOST_TEST(ScaleDeg(0) == 0);
    BOOST_TEST(ScaleDeg(-360) == 0);
    BOOST_TEST(ScaleDeg(360) == 0);

    // sin bug in pcrcalc Mon Aug 14 10:14:28 CEST 2000:
    BOOST_TEST(ScaleDeg(-80640) == 0);
}
