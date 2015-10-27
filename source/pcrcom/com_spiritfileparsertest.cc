#define BOOST_TEST_MODULE pcraster com spirit_file_parser
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_pathname.h"
#include "com_file.h"
#include "com_exception.h"
#include "com_spiritfileparser.h"


//! test the idea of saving the current position to resume later
BOOST_AUTO_TEST_CASE(current)
{
  using namespace com;

  com::PathName pn("testparser.txt");
  com::write("1 2 3 4\n \n 5 A6",pn);
  std::vector<int> parsed;

  SpiritFileParser sfp(pn);

  BOOST_CHECK(sfp.begin()==sfp.current());
  using namespace boost::spirit::classic;
  sfp.pi= parse(sfp.begin(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
  BOOST_CHECK(!sfp.pi.full);
  BOOST_CHECK(parsed.size()==2);
  sfp.advance();

  sfp.pi= parse(sfp.current(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
  sfp.advance();
  BOOST_CHECK(parsed.size()==4);
  BOOST_CHECK(parsed[3]==4);

  bool catched(false);
  try {
   sfp.pi= parse(sfp.current(),sfp.end(),
         int_p[append(parsed)] >> int_p[append(parsed)], space_p);
   sfp.errorAtStop();
  } catch (const com::FilePositionError& cpe) {
    BOOST_CHECK(cpe.lineNr()==3);
    BOOST_CHECK(cpe.columnNr()==4);
    BOOST_WARN_MESSAGE(cpe.messages().find("A6") != std::string::npos,
     std::string("EXPECT A6 to be part of message ")+cpe.messages());
    catched=true;
  }
  BOOST_CHECK(catched);
}


BOOST_AUTO_TEST_CASE(file_map_too_large)
{
  using namespace com;

#ifdef WIN32
  com::PathName big("E:\\gam_allXL.xyz");
#else
  com::PathName big("/home/cees/tmp/gam_allXL.xyz");
#endif
  if (com::exists(big)) {
   bool catched(false);
   try {
    SpiritFileParser n(big);
   } catch(const com::OpenFileError& c) {
     BOOST_CHECK(c.messages().find("large") != std::string::npos);
     catched=true;
   }
   BOOST_CHECK(catched);
  }
}
