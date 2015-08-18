#ifndef INCLUDED_GEO_CSFMAPTEST
#define INCLUDED_GEO_CSFMAPTEST



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif

namespace boost {
  namespace unit_test {
    class test_suite;
  }
}


namespace geo {



/*!
  \class CSFMapTest
  \brief This class implements the unit tests for the CSFMap class.
*/
//       1         2         3         4         5         6         7         8
class CSFMapTest
{

private:

  CSFMap *   d_map1;

public:

  static boost::unit_test::test_suite* suite();

  //! Constructor.
                   CSFMapTest          ();

  void             setUp               ();

  void             tearDown            ();

  void             testConstructor     ();

  void             testNrRows          ();

  void             testNrCols          ();

  void             testNrCells         ();
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



} // namespace geo

#endif
