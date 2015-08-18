#ifndef INCLUDED_COM_CSFTEST
#define INCLUDED_COM_CSFTEST


namespace boost {
  namespace unit_test {
    class test_suite;
  }
}

namespace com {



/*!
  \class CSFCellTest
  \brief This class implements the unit tests for the com::csfcell.cc module.
*/
class CSFCellTest
{

public:

  static boost::unit_test::test_suite* suite();

  //! Constructor.
                   CSFCellTest         ();

  void             setUp               ();

  void             tearDown            ();

  void             testCastAndCopyCells();

  void             testCsfSizes        ();

  void             testSetMV           ();
  void             testGetMinMax       ();
  void             testAlterToStdMV    ();
  void             testFromStdMV       ();
  void             testEndianSwap      ();
  void             testCsfCellMax      ();
  void             testIsType          ();
  void             testLessMV          ();
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
