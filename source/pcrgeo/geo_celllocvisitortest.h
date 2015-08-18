#ifndef INCLUDED_GEO_CELLLOCVISITORTEST
#define INCLUDED_GEO_CELLLOCVISITORTEST





namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace geo {

class CellLocVisitorTest
{

public:

  static boost::unit_test::test_suite* suite();

  //! Constructor.
                   CellLocVisitorTest  ();

  void             setUp               ();

  void             tearDown            ();

  void             testAll             ();
  void             testDownstream      ();
};

} // namespace geo

#endif
