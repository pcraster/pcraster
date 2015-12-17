#define BOOST_TEST_MODULE pcraster old_calc io_csf_field_strategy
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "com_pathinfo.h"
#include "calc_iocsffieldstrategy.h"
#include "calc_rundirectory.h"
#include "calc_stackreader.h"


// BOOST_AUTO_TEST_CASE(get_stack_reader_default)
// {
//   using namespace calc;
// 
//  {
//   IoCsfFieldStrategy s;
//   RunDirectory rd;
//   std::string stackName("tmp");
//   const StackReader *sr = s.createStackReader(rd,stackName);
//   std::string result("tmp00000.001");
//   BOOST_CHECK(s.makeStackItemName(stackName,1) == result);
//   BOOST_CHECK(sr->itemName(1) == result);
//   BOOST_CHECK(sr->stackName() == stackName);
//   delete sr;
//  }
// 
//  {
//   IoCsfFieldStrategy s;
//   RunDirectory rd;
//   com::PathName stackName("./stackReader/tmp00000.001");
//   BOOST_CHECK(com::pathExists(stackName.toString()));
//   const StackReader *sr = s.createStackReader(rd,stackName.toString());
//   com::PathName result("./stackReader/tmp00000.001");
//   BOOST_CHECK(s.makeStackItemName(stackName.toString(),1) == result.toString());
//   BOOST_CHECK(sr->itemName(1) == result);
//   BOOST_CHECK(sr->stackName() == stackName.toString());
//   delete sr;
//  }
// }


BOOST_AUTO_TEST_CASE(get_stack_reader_path)
{
  using namespace calc;

  IoCsfFieldStrategy s;
  RunDirectory rd;
  com::PathName path("stackReader");

  rd.setRunDirectory(path.toString(),"");
  path.makeAbsolute();
  std::string stackName("tmp");
  const StackReader *sr = s.createStackReader(rd,stackName);
  std::string item1("tmp00000.001");
  com::PathName result(path+item1);
  BOOST_CHECK(s.makeStackItemName(stackName,1) == item1);
//PRINT_VAR(sr->itemName(1))
//PRINT_VAR(result.toString());
#ifdef WIN32
  bool onWin32=true;
  BOOST_WARN( (onWin32 && sr->itemName(1) == result.toString()));
  BOOST_CHECK( (onWin32 && sr->stackName() == (path+stackName)) );
#endif

  delete sr;
}
