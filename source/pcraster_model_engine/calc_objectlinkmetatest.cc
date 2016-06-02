#define BOOST_TEST_MODULE pcraster model_engine objectlinkmeta
#include <boost/test/unit_test.hpp>

#define private public
#include "calc_objectlinkmeta.h"



BOOST_AUTO_TEST_CASE(testCtor)
{
  using namespace calc;

  ObjectLinkMeta olm("mldd",0);
  //! ctor
  olm.pushBack("",false,VS_STRING,ST_NON);
  olm.pushBack("",false,VS_S,ST_EITHER);
  //! method
  olm.pushBack("twoInputOneResult",false,VS_N,ST_SPATIAL);
  olm.pushBack("twoInputOneResult",false,VS_S,ST_EITHER);
  olm.pushBack("twoInputOneResult",true, VS_O,ST_SPATIAL);

  BOOST_CHECK(olm.d_methods.size()==2);
  //! ctor
  ObjectLinkMeta::MethodMap::iterator m=olm.d_methods.find("");
  BOOST_CHECK(m!=olm.d_methods.end());
  BOOST_CHECK(m->second.d_name.empty());
  BOOST_CHECK(m->second.d_input.size()==2);
  BOOST_CHECK(m->second.d_input[0].vs==VS_STRING);
  BOOST_CHECK(m->second.d_input[0].st==ST_NON);
  BOOST_CHECK(m->second.d_input[1].vs==VS_S);
  BOOST_CHECK(m->second.d_input[1].st==ST_EITHER);
  BOOST_CHECK(m->second.d_result.size()==1);
  BOOST_CHECK(m->second.d_result[0].vs==VS_OBJECT);

  //! twoInputOneResult
  m=olm.d_methods.find("twoInputOneResult");
  BOOST_CHECK(m!=olm.d_methods.end());
  BOOST_CHECK(m->second.d_name=="twoInputOneResult");
  BOOST_CHECK(m->second.d_input.size()==2);
  BOOST_CHECK(m->second.d_input[0].vs==VS_N);
  BOOST_CHECK(m->second.d_input[0].st==ST_SPATIAL);
  BOOST_CHECK(m->second.d_input[1].vs==VS_S);
  BOOST_CHECK(m->second.d_input[1].st==ST_EITHER);
  BOOST_CHECK(m->second.d_result.size()==1);
  BOOST_CHECK(m->second.d_result[0].vs==VS_O);
  BOOST_CHECK(m->second.d_result[0].st==ST_SPATIAL);

}
