#define BOOST_TEST_MODULE pcraster model_engine maskpacking
#include <boost/test/unit_test.hpp>
#include "calc_globallibdefs.h"
#include "com_algorithm.h"
#include "com_csfcell.h"
#include "com_new.h"
#include "geo_csfmap.h"
#include "geo_rasterspace.h"
#include "geo_filecreatetester.h"
#include "calc_asispacking.h"
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "calc_unpackedsrc.h"
#include "calc_unpackedcreation.h"
#include "calc_modelbuilder.h"

#define private public
#include "calc_maskpacking.h"

struct Fixture
{

    Fixture()
    {
        calc::globalInit();
    }


    ~Fixture()
    {
        calc::globalEnd();
    }

};


BOOST_FIXTURE_TEST_SUITE(maskpacking, Fixture)

BOOST_AUTO_TEST_CASE(testSpatialPacking)
{
  using namespace calc;

 geo::RasterDim rs(2,3);

 {
  UINT1 maskInit[6]= { 1, 1, 1, 1, 1, 1 };
  // Test both being equal with full mask
  std::vector<bool> mask(6);
  for(size_t i=0; i < 6; ++i)
    mask[i]=maskInit[i]==1;
  MaskPacking mp(rs,mask);
  AsIsPacking ap(rs);

  BOOST_CHECK(mp.d_evenIsValueRL);
  BOOST_CHECK(mp.d_rlIndex.size()==2);
  BOOST_CHECK(mp.d_rlIndex[0]==0);
  BOOST_CHECK(mp.d_rlIndex[1]==6);

  SpatialPacking *cs[2] = { &mp, &ap };

  for (size_t i=0; i < 2; i++) {
    BOOST_CHECK(cs[i]->rasterDim() == rs);

    float values[6];
    // 11,12,13,14,15,16
    std::generate_n(values,6,com::SeqInc<float>(11));

    Spatial in(VS_S,values,6);

    BOOST_CHECK(cs[i]->nrFieldCells()    == 6);
    BOOST_CHECK(cs[i]->toRasterId(0) == 0);
    BOOST_CHECK(cs[i]->toRasterId(2) == 2);
    BOOST_CHECK(cs[i]->toRasterId(5) == 5);
    BOOST_CHECK(cs[i]->toFieldId(0)  == 0);
    BOOST_CHECK(cs[i]->toFieldId(2)  == 2);
    BOOST_CHECK(cs[i]->toFieldId(5)  == 5);

    Field   *b=cs[i]->createSpatial(VS_B);
    BOOST_CHECK(dynamic_cast<Spatial *>(b));
    BOOST_CHECK(b->isSpatial());
    BOOST_CHECK(b->vs()==VS_B);
    BOOST_CHECK(b->nrValues()==6);
    delete b;


    // TEST UnpackedSrc

    const Field *f=cs[i]->unpack(&in);
    // unpacked src spatial
    UnpackedSrc usS(*cs[i],&in);

    NonSpatial  ns(VS_N, 4);
    // unpacked src nonspatial
    UnpackedSrc usN(*cs[i],&ns);

    BOOST_CHECK(usN.src() == &ns);
    BOOST_CHECK(usN.src()->vs() == VS_N);
    BOOST_CHECK(usN.src()->src_4()[0] == 4);

    BOOST_CHECK(f->nrValues() == 6);
    BOOST_CHECK(usS.src()->nrValues() == 6);
    // needed for MSVC release mode, equals fails otherwise:
    const float *src=f->src_f();
    BOOST_CHECK(std::equal(values,values+6,src));
    BOOST_CHECK(std::equal(values,values+6,usS.src()->src_f()));
    if (cs[i] == &mp) {
      BOOST_CHECK(f != &in);
      delete f;
      BOOST_CHECK(usS.src() != &in);
    } else {
      BOOST_CHECK(f == &in);
      BOOST_CHECK(usS.src() == &in);
    }

    { // test SpatialPacking::pack
      Field *packed = cs[i]->pack(&in);
      BOOST_CHECK(packed->nrValues() == 6);
      BOOST_CHECK(std::equal(values,values+6,packed->src_f()));
      if (cs[i] == &mp) {
        BOOST_CHECK(packed != &in);
        delete packed;
      } else
        BOOST_CHECK(packed == &in);
    }

    // TEST UnpackedCreation

    { // automatic clean up
      UnpackedCreation uc(*cs[i],VS_N);
      BOOST_CHECK(uc.unpacked()->nrValues()==6);
      BOOST_CHECK(uc.unpacked()->vs()==VS_N);
    }

    { //  clean up by delete
      UnpackedCreation uc(*cs[i],VS_N);
      BOOST_CHECK(uc.unpacked()->nrValues()==6);
      BOOST_CHECK(uc.unpacked()->vs()==VS_N);
      Field *rPacked = uc.releasePacked();
      BOOST_CHECK(rPacked->nrValues()==6);
      BOOST_CHECK(rPacked->vs()==VS_N);
      delete rPacked;
    }
   } // eofor
 }
 {
  UINT1 maskInit[6]   = { 1 , 1, MV_UINT1, 1,     0,     1};
  // rlIndex              0 -v-  2, -m-    3, -v- 4, -m- 5 -v- 6
  std::vector<bool> mask(6);
  for(size_t i=0; i < 6; ++i)
    mask[i]=maskInit[i]==1;
  MaskPacking mp(rs,mask);

  BOOST_CHECK(mp.d_evenIsValueRL);
  BOOST_CHECK(mp.d_rlIndex.size()==6);
  BOOST_CHECK(mp.d_rlIndex[0]==0);
  BOOST_CHECK(mp.d_rlIndex[1]==2);
  BOOST_CHECK(mp.d_rlIndex[2]==3);
  BOOST_CHECK(mp.d_rlIndex[3]==4);
  BOOST_CHECK(mp.d_rlIndex[4]==5);
  BOOST_CHECK(mp.d_rlIndex[5]==6);

  BOOST_CHECK(mp.nrFieldCells() == 4);
  BOOST_CHECK(mp.toRasterId(0) == 0);
  BOOST_CHECK(mp.toFieldId(0)  == 0);

  BOOST_CHECK(mp.toRasterId(2) == 3);
  BOOST_CHECK(mp.toFieldId(3)  == 2);

  BOOST_CHECK(mp.toRasterId(3) == 5);
  BOOST_CHECK(mp.toFieldId(5)  == 3);

  BOOST_CHECK(mp.toFieldId(2)  == IFieldRDConversion::invalidId());

  Field   *b=mp.createSpatial(VS_N);
  BOOST_CHECK(dynamic_cast<Spatial *>(b));
  BOOST_CHECK(b->isSpatial());
  BOOST_CHECK(b->vs()==VS_N);
  BOOST_CHECK(b->nrValues()==4);
  std::generate_n(b->dest_4(),4,com::SeqInc<INT4>(21));


  // TEST unpack/UnpackedSrc
  const Field *f=mp.unpack(b);
  // unpacked src spatial
  UnpackedSrc usS(mp,b);
  BOOST_CHECK(f->isSpatial());
  BOOST_CHECK(f->vs()==VS_N);
  BOOST_CHECK(f->nrValues()==6);
  INT4 bu[6]       = { 21 , 22, MV_INT4, 23, MV_INT4, 24};

  // needed for MSVC release mode, equals fails otherwise:
  const INT4 *src=f->src_4();
  BOOST_CHECK(std::equal(bu,bu+6,src));
  BOOST_CHECK(std::equal(bu,bu+6,usS.src()->src_4()));

  BOOST_CHECK(b!=f);
  delete f;
  delete b;

  REAL4 values[6]     = { 11,12,13       ,14,15,16};
  REAL4 packedVals[4] = { 11,12,          14,   16};
  Spatial in(VS_S,values,6);

  { // test SpatialPacking::pack
    Field *packed = mp.pack(&in);
    BOOST_CHECK(packed->nrValues() == 4);
    BOOST_CHECK(std::equal(packedVals,packedVals+4,packed->src_f()));
    BOOST_CHECK(packed != &in);
    delete packed;
  }

  // TEST UnpackedCreation

  { // automatic clean up
    UnpackedCreation uc(mp,VS_N);
    BOOST_CHECK(uc.unpacked()->nrValues()==6);
    BOOST_CHECK(uc.unpacked()->vs()==VS_N);
  }

  { //  clean up by delete
    UnpackedCreation uc(mp,VS_S);
    BOOST_CHECK(uc.unpacked()->nrValues()==6);
    BOOST_CHECK(uc.unpacked()->vs()==VS_S);
    std::generate_n(uc.unpacked()->dest_f(),6,com::SeqInc<float>(11));
    Field *rPacked = uc.releasePacked();
    BOOST_CHECK(rPacked->nrValues()==4);
    BOOST_CHECK(rPacked->vs()==VS_S);
    // needed for MSVC release mode, equals fails otherwise:
    const float* src=rPacked->src_f();
    BOOST_CHECK(std::equal(packedVals,packedVals+4,src));
    delete rPacked;
  }
 }
}

BOOST_AUTO_TEST_CASE(testScript)
{
  using namespace calc;

  geo::RasterSpace rs(80,50);
  /* make boolean mask with:
   *  - triangle of 1, where mask is defined
   *  - triangle of MV
   *  - lower part of 1
   * - set boolean mask als areamap
   * TODO fill assumes all equal on non-spatial need
   *      for masking?
   */

  UINT1 **mask = com::new2d<UINT1>(rs.nrRows(),rs.nrCols());
  REAL4 **result = com::new2d<REAL4>(rs.nrRows(),rs.nrCols());
  REAL4 **area = com::new2d<REAL4>(rs.nrRows(),rs.nrCols());
  REAL4 count=1;
  for(size_t r=0; r< rs.nrRows(); r++)
   for(size_t c=0; c< rs.nrCols(); c++) {
     mask[r][c] = (r < c ) ? MV_UINT1 : (r < rs.nrCols());
     pcr::setMV(result[r][c]);
     if (mask[r][c]==1)
       result[r][c]=count++;
   }
   std::fill_n(area[0],rs.nrCells(), count-1);

  {
   geo::CSFMap mFile(std::string("mvComprMask.map"),rs,VS_BOOLEAN);
   mFile.putCells(mask[0]);
  }
  {
   geo::CSFMap mFile(std::string("resultExpected.map"),rs,VS_SCALAR);
   mFile.putCells(result[0]);
  }
  {
   geo::CSFMap mFile(std::string("areaExpected.map"),rs,VS_SCALAR);
   mFile.putCells(area[0]);
  }

  {
    geo::FileCreateTester mt("resultComputed.map");
    geo::FileCreateTester ma("areaComputed.map");

    ModelBuilder mb;
    mb.setClone("mvComprMask.map");
    /* Will return other results on setMVCompression(false)
     * since the statements has no spatial input!:
     */
    mb.setMVCompression(true);

    mb.setModel("resultComputed.map = uniqueid(1);"
                "areaComputed.map = maparea(resultComputed.map)");

    mb.execute();

    BOOST_CHECK(mt.equalTo("resultExpected.map",true));
    BOOST_CHECK(ma.equalTo("areaExpected.map",true));

  }

  com::delete2d<UINT1>(mask);
  com::delete2d<REAL4>(result);
  com::delete2d<REAL4>(area);
}

BOOST_AUTO_TEST_SUITE_END()
