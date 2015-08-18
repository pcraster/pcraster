#ifndef INCLUDED_COM_MATRIXTEST
#define INCLUDED_COM_MATRIXTEST





namespace boost {
  namespace unit_test {
    class test_suite;
  }
}



namespace com {



/*!
  \class MatrixTest
  \brief This class implements the unit tests for the Matrix class.
*/
//       1         2         3         4         5         6         7         8
class MatrixTest
{

private:

public:

  //! Constructor.
                   MatrixTest          ();

  void             setUp               ();

  void             tearDown            ();

  //! Tests the equality operator.
  void             testEquality        ();

  //! Tests the inequality operator.
  void             testInequality      ();

  //! Tests the creation of an identity matrix.
  void             testIdentity        ();

  //! Tests the creation of a inverse matrix.
  void             testInvert          ();

  //! Tests the creation of a transpose matrix.
  void             testTranspose       ();

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
