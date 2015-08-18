#ifndef INCLUDED_COM_UTILTEST
#define INCLUDED_COM_UTILTEST





namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {



//! This class implements the unit tests for the Util class.
class UtilTest
{

public:

                   UtilTest            ();

  void             setUp               ();

  void             tearDown            ();

  void             testSmallestDivisor ();

  void             testLargestDivisor  ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
