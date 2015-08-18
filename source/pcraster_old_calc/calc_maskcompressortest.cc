#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_MASKCOMPRESSORTEST
#include "calc_maskcompressortest.h"
#define INCLUDED_CALC_MASKCOMPRESSORTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
#ifndef INCLUDED_COM_CSFCELL
#include "com_csfcell.h"
#define INCLUDED_COM_CSFCELL
#endif
#ifndef INCLUDED_COM_NEW
#include "com_new.h"
#define INCLUDED_COM_NEW
#endif
#ifndef INCLUDED_GEO_CSFMAP
#include "geo_csfmap.h"
#define INCLUDED_GEO_CSFMAP
#endif
#ifndef INCLUDED_GEO_FILECREATETESTER
#include "geo_filecreatetester.h"
#define INCLUDED_GEO_FILECREATETESTER
#endif
// Module headers.
#ifndef INCLUDED_CALC_MASKCOMPRESSOR
#include "calc_maskcompressor.h"
#define INCLUDED_CALC_MASKCOMPRESSOR
#endif
#ifndef INCLUDED_CALC_NULLCOMPRESSOR
#include "calc_nullcompressor.h"
#define INCLUDED_CALC_NULLCOMPRESSOR
#endif
#ifndef INCLUDED_CALC_COMPRESSIONINPUT
#include "calc_compressioninput.h"
#define INCLUDED_CALC_COMPRESSIONINPUT
#endif
#ifndef INCLUDED_CALC_SPATIAL
#include "calc_spatial.h"
#define INCLUDED_CALC_SPATIAL
#endif
#ifndef INCLUDED_CALC_DECOMPRESSEDDATA
#include "calc_decompresseddata.h"
#define INCLUDED_CALC_DECOMPRESSEDDATA
#endif
#ifndef INCLUDED_CALC_MODELBUILDER
#include "calc_modelbuilder.h"
#define INCLUDED_CALC_MODELBUILDER
#endif

/*!
  \file
  This file contains the implementation of the MaskCompressorTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC MASKCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::MaskCompressorTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<MaskCompressorTest> instance(new MaskCompressorTest());

  suite->add(BOOST_CLASS_TEST_CASE(&MaskCompressorTest::testCompressor, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MaskCompressorTest::testScript, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&MaskCompressorTest::test0Option, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF MASKCOMPRESSOR MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::MaskCompressorTest::MaskCompressorTest()
{
}



//! setUp
void calc::MaskCompressorTest::setUp()
{
}

//! tearDown
void calc::MaskCompressorTest::tearDown()
{
}




void calc::MaskCompressorTest::testCompressor()
{
 {
  geo::RasterSpace rs(2,3);
  UINT1 mask[6]= { 1, 1, 1, 1, 1, 1 };
  MaskCompressor mc(rs,mask);
  NullCompressor nc(rs);
  Compressor *cs[2] = { &mc, &nc };

  for (size_t i=0; i < 2; i++) {
    REAL4 *values = new REAL4[6];
    std::generate_n(values,6,com::SeqInc<REAL4>(1));
    // 1,2,3,4,5,6

    BOOST_CHECK(cs[i]->nrCellsCompressed()    == 6);
    BOOST_CHECK(cs[i]->toDecompressedIndex(0) == 0);
    BOOST_CHECK(cs[i]->toDecompressedIndex(2) == 2);
    BOOST_CHECK(cs[i]->toDecompressedIndex(5) == 5);

    CompressionInput ci(VS_S, values, *(cs[i]));
    Spatial *s=cs[i]->createSpatial(ci);
    BOOST_CHECK(s->nrValues() == cs[i]->nrCellsCompressed());
    ci.detachData(); // do not call [] delete => stack data
    REAL4 *compressed= (REAL4 *)s->srcValue();
    BOOST_CHECK(compressed[0]==1);
    BOOST_CHECK(compressed[2]==3);
    BOOST_CHECK(compressed[5]==6);

    DecompressedData dd(VS_S);
    cs[i]->decompress(dd,s->srcValue());
    REAL4 *dCopy = (REAL4 *)dd.decompressed();
    BOOST_CHECK(dCopy[0]==1);
    BOOST_CHECK(dCopy[2]==3);
    BOOST_CHECK(dCopy[4]==5);
    BOOST_CHECK(dCopy[5]==6);

    delete s;
  }
 }
 {
  geo::RasterSpace rs(2,3);
  UINT1 mask[6]= { 1, 1, MV_UINT1, 1, 0, 1 };
  REAL4 values[6] = { 1,2,3,4,5,6 };
  MaskCompressor mc(rs,mask);

  BOOST_CHECK(mc.nrCellsCompressed() == 4);
  BOOST_CHECK(mc.toDecompressedIndex(0) == 0);
  BOOST_CHECK(mc.toDecompressedIndex(2) == 3);
  BOOST_CHECK(mc.toDecompressedIndex(3) == 5);

  CompressionInput ci(VS_S, values, mc);
  Spatial *s=mc.createSpatial(ci);
  BOOST_CHECK(s->nrValues() == mc.nrCellsCompressed());
  ci.detachData(); // do not call [] delete => stack data
  REAL4 *compressed= (REAL4 *)s->srcValue();
  BOOST_CHECK(compressed[0]==1);
  BOOST_CHECK(compressed[2]==4);
  BOOST_CHECK(compressed[3]==6);

  DecompressedData dd(VS_S);
  mc.decompress(dd,s->srcValue());
  REAL4 *dCopy = (REAL4 *)dd.decompressed();
  BOOST_CHECK(dCopy[0]==1);
  BOOST_CHECK(pcr::isMV(dCopy[2]));
  BOOST_CHECK(pcr::isMV(dCopy[4]));
  BOOST_CHECK(dCopy[5]==6);

  delete s;
 }
}

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif


void calc::MaskCompressorTest::testScript()
{
  geo::RasterSpace rs(80,50);
  /* make boolean mask with:
   *  - triangle of 1, where mask is defined
   *  - triangle of MV
   *  - lower part if 1
   * - set boolean mask als areamap
   * TODO fill assumes all equal on non-spatial need
   *       for masking?
   */
  UINT1 **mask   = com::new2d<UINT1>(rs.nrRows(),rs.nrCols());
  REAL4 **result = com::new2d<REAL4>(rs.nrRows(),rs.nrCols());
  REAL4 **area   = com::new2d<REAL4>(rs.nrRows(),rs.nrCols());
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
   geo::CSFMap mFile(std::string("mvComprResult.map"),rs,VS_SCALAR);
   mFile.putCells(result[0]);
  }
  {
   geo::CSFMap mFile(std::string("mvComprArea.map"),rs,VS_SCALAR);
   mFile.putCells(area[0]);
  }

  bool succes=true;
  try {

    geo::FileCreateTester mt("resultMvComprScript.map");
    geo::FileCreateTester ma("areaMvComprScript.map");

    ModelBuilder mb;
    mb.setClone("mvComprMask.map");
    mb.setMVCompression(true);
    mb.addStatement("resultMvComprScript.map = order((-ycoordinate(1))+10)");
    mb.addStatement("areaMvComprScript.map = maparea(resultMvComprScript.map)");

    mb.execute();

    BOOST_CHECK(mt.equalTo("mvComprResult.map",false));
    BOOST_CHECK(ma.equalTo("mvComprArea.map",false));

  } catch (const com::Exception& e) {
    std::cerr << e.messages();
    succes=false;
  }
  BOOST_CHECK(succes);

  com::delete2d<UINT1>(mask);
  com::delete2d<REAL4>(result);
  com::delete2d<REAL4>(area);

}

void calc::MaskCompressorTest::test0Option()
{
  geo::RasterSpace rs(8,8);
  UINT1 **mask   = com::new2d<UINT1>(rs.nrRows(),rs.nrCols());
  REAL4 **result = com::new2d<REAL4>(rs.nrRows(),rs.nrCols());

  for(size_t r=0; r< rs.nrRows(); r++)
   for(size_t c=0; c< rs.nrCols(); c++) {
     mask[r][c] = (r < c ) ? MV_UINT1 : (r < rs.nrCols());
     pcr::setMV(result[r][c]);
     if (mask[r][c]==1)
       result[r][c]=1;
   }

  {
   geo::CSFMap mFile(std::string("zeroComprMask.map"),rs,VS_BOOLEAN);
   mFile.putCells(mask[0]);
   com::delete2d<UINT1>(mask);
  }
  {
   geo::CSFMap mFile(std::string("zeroComprResult.map"),rs,VS_SCALAR);
   mFile.putCells(result[0]);
   com::delete2d<REAL4>(result);
  }

  bool succes=true;
  try {

    geo::FileCreateTester mt("zeroCompr.map");

    ModelBuilder mb;
    mb.setClone("zeroComprMask.map");
    mb.setMVCompression(true);
    mb.set0Compression(true);
    mb.addStatement("X = scalar(0)");
    mb.addStatement("zeroCompr.map = X + spatial(1)");

    mb.execute();

    BOOST_CHECK(mt.equalTo("zeroComprResult.map",true));

  } catch (const com::Exception& e) {
    std::cerr << e.messages();
    succes=false;
  }
  BOOST_CHECK(succes);
}
