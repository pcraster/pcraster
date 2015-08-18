#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_INTABLESTREAMTEST
#include "com_intablestreamtest.h"
#define INCLUDED_COM_INTABLESTREAMTEST
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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.
#ifndef INCLUDED_COM_INTABLESTREAM
#include "com_intablestream.h"
#define INCLUDED_COM_INTABLESTREAM
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
/*!
  \file
  This file contains the implementation of the InTableStreamTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::InTableStreamTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<InTableStreamTest> instance(new InTableStreamTest());

  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testRead, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testRead2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&InTableStreamTest::testFormatErrors, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF INTABLESTREAM MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::InTableStreamTest::InTableStreamTest()
{
}



//! setUp
void com::InTableStreamTest::setUp()
{
}

//! tearDown
void com::InTableStreamTest::tearDown()
{
}

void com::InTableStreamTest::testRead()
{
  const char *files[2] = {"zinc.unix.eas","zinc.dos.eas"};
  for(size_t i=0; i < 2; i++) {
    InTableStream tab(files[i]);
    BOOST_CHECK(tab.layout() == InTableStream::GEO_EAS);
    std::vector<double> d;
    tab >> d;
    BOOST_CHECK(d.size()==3);
    BOOST_CHECK(d[0]==181072);
    BOOST_CHECK(d[1]==333611);
    BOOST_CHECK(d[2]==1022);
  }

 PathName pn("inTableStream.txt");
 std::vector<double> d;

   const char *testSimples[3] = {
     //simple and some extra new lines
     "1\n2\n \n3\n",
    // no new line at end
     "1\n2\n3",
     "1\n2\n3 \n 4" };
   for(size_t i=0; i < 3; i++) {
     write(testSimples[i],pn);
     InTableStream tab(pn);
     BOOST_CHECK(tab.layout() == InTableStream::PLAIN_TXT);
     size_t v=1;
     while (tab >> d) {
      BOOST_CHECK(d.size()==1);
      BOOST_CHECK(d[0]==v++);
     }
     BOOST_CHECK(i < 2 ? v==4 : v==5);
   }
 { write("\n2\n\n0",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.lineNrStartLastRead()==0);
   BOOST_CHECK(tab.layout() == InTableStream::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(tab.lineNrStartLastRead()==2);
   BOOST_CHECK(d.size()==1&&d[0]==2);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(tab.lineNrStartLastRead()==4);
   BOOST_CHECK(d.size()==1&&d[0]==0);
   BOOST_CHECK(!(tab >> d));
 }
 { // geo eas with no data
   write("\n2\n\ntitle2 ",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::GEO_EAS);
   BOOST_CHECK(tab.nrColumns() == 2);
   BOOST_CHECK(tab.columnNames()[0].empty());
   BOOST_CHECK(tab.columnNames()[1]=="title2");
   BOOST_CHECK(!(tab >> d));
   BOOST_CHECK(d.empty());
 }
 { // geo eas with numeric descriptions and a single line of data
   write("1\n2\n0\n0\n2 3",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::GEO_EAS);
   BOOST_CHECK(tab.nrColumns() == 2);
   BOOST_CHECK(tab.columnNames()[0]=="0");
   BOOST_CHECK(tab.columnNames()[1]=="0");
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(tab.lineNrStartLastRead()==5);
   BOOST_CHECK(d.size()==2&&d[0]==2 && d[1]==3);
   BOOST_CHECK(!(tab >> d));
 }
}

void com::InTableStreamTest::testRead2()
{
 PathName pn("inTableStream2.txt");
 std::vector<double> d;
 { // text with single line of data
   write("0",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::PLAIN_TXT);
   BOOST_CHECK(tab.nrColumns() == 1);
   BOOST_CHECK(tab.columnNames()[0].empty());
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(tab.lineNrStartLastRead()==1);
   BOOST_CHECK(d.size()==1&&d[0]==0);
   BOOST_CHECK(!(tab >> d));
 }
 { // text with 2 lines of data but irregular
   write("0\n1 2",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::PLAIN_TXT);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(d.size()==1&&d[0]==0);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(d.size()==2&&d[0]==1 && d[1]==2);
   BOOST_CHECK(!(tab >> d));
 }
 { // file with only space
   write(" \t \n  ",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::EMPTY);
   BOOST_CHECK(tab.nrColumns() == 0);
   BOOST_CHECK(!(tab >> d));
   // pretty useles and incorrect:
   // BOOST_CHECK(tab.lineNrStartLastRead()==1);
   BOOST_CHECK(d.empty());
 }
 { // empty file, 0 bytes
   write("",pn);
   BOOST_CHECK(size(pn)==0);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::EMPTY);
   BOOST_CHECK(tab.nrColumns() == 0);
   BOOST_CHECK(!(tab >> d));
   BOOST_CHECK(d.empty());
 }
 { // geoEas with incorrect data (1 record too long)
   com::write("geoEas d\n2\nt1\nt2\n1 2 3\n4 5 \n",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == TableInfo::GEO_EAS);
   BOOST_CHECK(tab.nrColumns() == 2);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(d.size()==3);
   BOOST_CHECK(tab >> d);
   BOOST_CHECK(d.size()==2);
   BOOST_CHECK(!(tab >> d));
 }
}

void com::InTableStreamTest::testFormatErrors()
{
 { // not a table file
   PathName pn("spawnScript");
   bool catched(false);
   try {
     InTableStream tab(pn);
   } catch (const FileFormatError& e) {

     BOOST_CHECK(e.messages().find("Format of file")!=std::string::npos);
     catched=true;
   }
   BOOST_CHECK(catched);
 }
 PathName pn("inTableStream.txt");

 { // geo eas but short on column names
   write("description\n3\ntitle1\ntitle2\n",pn);
   bool catched(false);
   try {
     InTableStream tab(pn);
   } catch (const FileFormatError& e) {
     BOOST_CHECK(e.messages().find("Format of file")!=std::string::npos);
     catched=true;
   }
   BOOST_CHECK(catched);
 }

 { // illegal format
   write("1\n2\n3\n4\n ac\n3\n",pn);
   InTableStream tab(pn);
   BOOST_CHECK(tab.layout() == InTableStream::PLAIN_TXT);
   std::vector<double> d;
   bool catched(false);
   try {
    while (tab >> d);
   } catch (const FilePositionError& e) {
     BOOST_CHECK(e.messages().find("ac")!=std::string::npos);
     BOOST_CHECK(e.lineNr()==5);
     BOOST_CHECK(e.columnNr()==2);
     catched=true;
   }
   BOOST_CHECK(catched);
 }
}
