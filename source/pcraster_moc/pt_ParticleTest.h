#ifndef INCLUDED_PT_PARTICLETEST
#define INCLUDED_PT_PARTICLETEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace pt {
  // Particle declarations.
}



namespace pt {



//! This class implements the unit tests for the Particle class.
class ParticleTest
{

public:

                   ParticleTest        ();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace pt

#endif
