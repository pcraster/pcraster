#define BOOST_TEST_MODULE pcraster model_engine reportvisitor
#include <boost/test/unit_test.hpp>
#include "calc_reportvisitor.h"
#include "calc_stringparser.h"
#include "calc_reporttable.h"
#include "calc_astpar.h"


BOOST_AUTO_TEST_CASE(testPos)
{
  using namespace calc;

  struct M {
   private:
     ASTNode    *d_code;
   public:
     ReportPars  d_rps;
     M(const std::string& code, bool reportLastAssOfEverySymbol):
       d_code(StringParser::createCodeAsNode(code))
     {
       ReportTable emptyRT;
       ReportVisitor rv(
         reportLastAssOfEverySymbol,
         emptyRT,Timer());
       d_code->accept(rv);
       d_rps = rv.reportPars();
     }
     ~M() {
       delete d_code;
     }
     bool posEqual(std::string const& name, std::string const& pos) const {
     if (!d_rps.count(name))
         return false;
     ReportPars::const_iterator i=d_rps.find(name);
     std::string parPos=i->second.d_par->shortPosText();
     return parPos.compare(pos)==0;
    }
  };

  { // explicit report, first reported
      M m("report p=inp1s.map+0;p=p+2;",false);
      //   1245678901234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:8'"));
  }
  { // explicit report BUT last ass reported
      M m("report p=inp1s.map+0;p=p+2;",true);
      //   12345678901234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:22'"));
  }
  { // no explicit set, and No  last ass reported
      M m("p=inp1s.map+0;p=p+2;",false);
      BOOST_CHECK(!m.d_rps.count("p"));
  }
  { // no explicit set, but last ass reported
      M m("p=inp1s.map+0;p=p+2;",true);
      //   1234567890123456
      BOOST_CHECK(m.d_rps.count("p"));
      BOOST_CHECK(m.posEqual("p","line '1:15'"));
  }
}
