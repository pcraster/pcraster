#define BOOST_TEST_MODULE pcraster calc kinematic
#include <boost/test/unit_test.hpp>


extern "C" double IterateToQnew(
    double Qin, /* summed Q new in for all sub-cachments */
    double Qold,  /* current discharge */
    double q,
    double alpha,
    double beta,
    double deltaT,
    double deltaX,
    double epsilon);


//! some stuff that crashed
BOOST_AUTO_TEST_CASE(iterate1)
{
 // adapt initial estimator Qkx twice
 //  Qkx   = MAX(Qkx, 1e-30);
 double cmpEps(1E-12);
 double v = IterateToQnew(
                  /* Qin    */ 0.000201343,
                  /* Qold   */ 0.000115866,
                  /* q      */ -0.000290263,
                  /* alpha  */ 1.73684,
                  /* beta   */ 0.6,
                  /* deltaT */ 15,
                  /* deltaX */ 10,
                  /* epsilon */ 1E-12);
 // printf("\n %20.18f \n",v);
 double retV = 0.000031450866300937;
 BOOST_CHECK(v > (retV-cmpEps) && v < (retV+cmpEps) );
}

//! more stuff that crashed
BOOST_AUTO_TEST_CASE(iterate2)
{
 // does not terminate
 double v = IterateToQnew(
              /* Qin */ 0,
              /* Qold */  1.11659e-07,
              /* q */ -1.32678e-05,
              /* alpha */ 1.6808,
              /* beta */ 0.6,
              /* deltaT */ 15,
              /* deltaX */ 10,
              /* epsilon */ 1E-12);
 
 BOOST_CHECK(v == 1e-30);
}

/*
Executing timestep 1370
fQkx 8.57113e-05 Qkx 1e-30
Qin 0
Qold 1.11659e-07
q -1.32678e-05
alpha 1.6808
beta 0.6
deltaT 15
deltaX 10

Executing timestep 1388
fQkx 0.000195926 Qkx 1e-30
Qin 0
Qold 1.09887e-07
q -2.11137e-05
alpha 1.80895
beta 0.6
deltaT 15
deltaX 10

Executing timestep 1392
fQkx 7.69674e-05 Qkx 1e-30
Qin 0
Qold 6.27638e-08
q -9.52041e-06
alpha 1.37993
beta 0.6
deltaT 15
deltaX 10
Executing timestep 1397
fQkx 3.48154e-05 Qkx 1e-30
Qin 0
Qold 6.46732e-08
q -6.60769e-06
alpha 1.32366
beta 0.6
deltaT 15
deltaX 10
Executing timestep 1951
fQkx 5.76115e-05 Qkx 1e-30
Qin 7.27264e-08
Qold 9.94452e-08
q -9.95288e-06
alpha 1.45618
beta 0.6
deltaT 15
deltaX 10
Executing timestep 1973
fQkx 2.48142e-05 Qkx 1e-30
Qin 1.20408e-08
Qold 9.95529e-08
q -7.61784e-06
alpha 1.42127
beta 0.6
deltaT 15
deltaX 10
Executing timestep 1988
fQkx 8.88512e-05 Qkx 1e-30
Qin 3.42076e-09
Qold 1.1312e-07
q -1.37175e-05
alpha 1.72076
beta 0.6
deltaT 15
deltaX 14.1421
Executing timestep 1993
fQkx 2.44994e-05 Qkx 1e-30
Qin 7.01352e-09
Qold 5.35952e-08
q -5.34198e-06
alpha 1.2816
beta 0.6
deltaT 15
deltaX 10

*/
