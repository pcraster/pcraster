#ifndef INCLUDED_AG_GEOMETRYTEST
#define INCLUDED_AG_GEOMETRYTEST

// External headers.

// Project headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace ag {
  // GeometryTest declarations.
}



namespace ag {

//! This class implements the unit tests for the Geometry class.
class GeometryTest
{

private:

public:

                   GeometryTest        ();

  void             testPoint           ();

  void             testLine            ();

  void             testInterSection    ();

  static boost::unit_test::test_suite* suite();

};

} // namespace ag

#endif
