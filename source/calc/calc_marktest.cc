#define BOOST_TEST_MODULE pcraster calc mark
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "api.h"
#include "csftypes.h"
#include "calc.h"
#include <iostream>


class MarkTestPrivate {

  size_t d_nrRows;
  size_t d_nrCols;
  size_t d_nrCells;
  UINT1  d_resultCells[6];
  REAL4  d_orderCells[6];
  REAL4  d_amountCells[6];
  REAL4  d_tresholdValue[1];

  MAP_UINT1* d_result;
  MAP_REAL8 *d_order, *d_amount,*d_treshold;


 // Delete maps.
  public:
   MarkTestPrivate() {
     d_nrRows = 3;
     d_nrCols = 2;
     d_nrCells = d_nrRows * d_nrCols;
     double cellSize = 15.0;
     CSF_PT projection = PT_YINCT2B;

     BootTestApi(cellSize, projection == PT_YINCT2B);

     d_result  = InitMapUINT1(d_nrRows, d_nrCols, d_resultCells,  TRUE, CR_UINT1);
     d_order   = InitMapREAL8(d_nrRows, d_nrCols, d_orderCells,  TRUE, CR_REAL4);
     d_amount  = InitMapREAL8(d_nrRows, d_nrCols, d_amountCells, TRUE, CR_REAL4);
     d_treshold= 0;
   }
   ~MarkTestPrivate() {
     DeleteInternalMAP_UINT1(d_result);
     DeleteInternalMAP_REAL8(d_order);
     DeleteInternalMAP_REAL8(d_amount);
     DeleteInternalMAP_REAL8(d_treshold);
   }

   bool execCheckLe(const UINT1 *expectedResult) {
       // Call function.
      int r= MarkWhileSumLe(d_result,d_order,d_amount,d_treshold);
      POSTCOND(!r);
      bool t= std::equal(d_resultCells, d_resultCells + d_nrCells, expectedResult);
      if (!t) {
        std::cout << "result ";
        for (size_t i=0;i < d_nrCells; i++)
          std::cout << (int)d_resultCells[i];
        std::cout << "\n";
      }
      return t;
   }
   bool execCheckGe(const UINT1 *expectedResult) {
       // Call function.
      int r= MarkUntilSumGe(d_result,d_order,d_amount,d_treshold);
      POSTCOND(!r);
      bool t= std::equal(d_resultCells, d_resultCells + d_nrCells, expectedResult);
      if (!t) {
        std::cout << "result ";
        for (size_t i=0;i < d_nrCells; i++)
          std::cout << (int)d_resultCells[i];
        std::cout << "\n";
      }
      return t;
   }

   void setOrder(const REAL4 *order) {
         std::copy(order,order+d_nrCells,d_orderCells);
   }
   void setAmount(const REAL4 *amount) {
         std::copy(amount,amount+d_nrCells,d_amountCells);
   }
   void setTreshold(REAL4 t) {
         // non spatial is copied!
         d_tresholdValue[0]=t;
         if (d_treshold)
           DeleteInternalMAP_REAL8(d_treshold);
         d_treshold= InitMapREAL8(d_nrRows, d_nrCols, d_tresholdValue, FALSE, CR_REAL4);
   }
 };


#define SET_ORDER(o0,o1,o2,o3,o4,o5)  \
   REAL4 orderCells[6]           = {o0,o1,o2,o3,o4,o5 }; \
   data.setOrder(orderCells);
#define SET_AMOUNT(o0,o1,o2,o3,o4,o5)  \
   REAL4 amountCells[6]           = {o0,o1,o2,o3,o4,o5 }; \
   data.setAmount(amountCells);


BOOST_AUTO_TEST_CASE(le)
{
  MarkTestPrivate data;
  { // treshold high for all
   SET_ORDER( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(100000);
   UINT1 result1[6]= {1,1,1,1,1,1};

   BOOST_CHECK(data.execCheckLe(result1));
  }
  { // treshold low
   SET_ORDER( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(-100000);
   UINT1 result1[6]= {0,0,0,0,0,0};

   BOOST_CHECK(data.execCheckLe(result1));
  }
  { //  no spill, treshold exactly
   SET_ORDER( 11.0, 12.0, 13.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(15);
   UINT1 result1[6]= {0,0,0,1,1,1};

   BOOST_CHECK(data.execCheckLe(result1));
  }
  { //  spill, treshold exceeded in cell
   SET_ORDER( 11.0, 12.0, 13.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(10);
   UINT1 result1[6]= {0,0,0,1,1,0};

   BOOST_CHECK(data.execCheckLe(result1));
  }
}


BOOST_AUTO_TEST_CASE(ge)
{
  MarkTestPrivate data;
  { // treshold high for all
   SET_ORDER( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(100000);
   UINT1 result1[6]= {1,1,1,1,1,1};

   BOOST_CHECK(data.execCheckGe(result1));
  }
  { // treshold low
   SET_ORDER( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(-100000);
   UINT1 result1[6]= {0,0,0,0,0,0};

   BOOST_CHECK(data.execCheckGe(result1));
  }
  { //  no spill, treshold exactly
   SET_ORDER( 11.0, 12.0, 13.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(15);
   UINT1 result1[6]= {0,0,0,1,1,1};

   BOOST_CHECK(data.execCheckGe(result1));
 }
 { //  spill, treshold exceeded in cell
   SET_ORDER( 11.0, 12.0, 13.0, 4.0, 5.0, 6.0);
   SET_AMOUNT( 1.0, 2.0, 3.0, 4.0, 5.0, 6.0);
   data.setTreshold(10);
   UINT1 result1[6]= {0,0,0,1,1,1};

   BOOST_CHECK(data.execCheckGe(result1));
  }
}
