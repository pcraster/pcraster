#ifndef INCLUDED_COM_VECTORTEST
#define INCLUDED_COM_VECTORTEST





namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {



/*!
  \class VectorTest
  \brief This class implements the unit tests for the Vector class.
*/
//       1         2         3         4         5         6         7         8
class VectorTest
{

private:

public:

  //! Constructor.
                   VectorTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testSetElement      ();

  void             testScale           ();

  void             testSize            ();

  void             testMagnitude       ();

  void             testDot             ();

  void             testCross           ();

  void             testAdd             ();

  static boost::unit_test::test_suite* suite();

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
