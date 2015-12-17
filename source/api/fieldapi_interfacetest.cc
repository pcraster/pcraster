#define BOOST_TEST_MODULE pcraster fieldapi interface
#include <boost/test/unit_test.hpp>
#include "pcrtypes.h"
#include "geo_celllocvisitor.h"
#include "fieldapi_interface.h"
#include "fieldapi_testfield.h"


BOOST_AUTO_TEST_CASE(non_mv)
{
  using namespace fieldapi;

  // 1st row all nonMV
  // 2nd row allways some MV
     UINT1  l1[ ] = { 0, 1, 2 , 3, MV_UINT1, 5 };
     UINT1  l2[ ] = { 0, 1, 2 , 3, MV_UINT1, MV_UINT1 };
     REAL4  l3[ ] = { 0, 1, 2 , 3, 70,       80 };
     pcr::setMV(l3[3]);

     TestField<UINT1,2,3> d1(l1);
     TestField<UINT1,2,3> d2(l2);
     TestField<REAL4,2,3> d3(l3);

     ReadWriteData<UINT1,UINT1> t1(TEST_FIELD_INIT(d1));
     ReadWriteData<UINT1,UINT1> t2(TEST_FIELD_INIT(d2));
     ReadWriteData<REAL8,REAL4> t3(TEST_FIELD_INIT(d3));
     ReadOnlyNonSpatial<INT4>   t4(123,t1.nrRows(),t1.nrCols());

     std::vector<const Common*> all;
     all.push_back(&t1);
     all.push_back(&t2);
     all.push_back(&t3);
     all.push_back(&t4);
     for(geo::CellLocVisitor v(t1.nrRows(),t1.nrCols());
          v.valid(); ++v) {
        if ((*v).row() == 0)
          BOOST_CHECK(nonMV(all,*v));
        else
          BOOST_CHECK(!nonMV(all,*v));
     }
}
