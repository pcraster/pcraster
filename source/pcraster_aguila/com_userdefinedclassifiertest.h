#ifndef INCLUDED_COM_USERDEFINEDCLASSIFIERTEST
#define INCLUDED_COM_USERDEFINEDCLASSIFIERTEST



// Library headers.

// PCRaster library headers.

// Module headers.



namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {
  // UserDefinedClassifier declarations.
}



namespace com {



//! This class implements the unit tests for the UserDefinedClassifier class.
class UserDefinedClassifierTest
{

private:

public:

                   UserDefinedClassifierTest();

  void             setUp               ();

  void             tearDown            ();

  void             test                ();

  static boost::unit_test::test_suite* suite();

};

} // namespace com

#endif
