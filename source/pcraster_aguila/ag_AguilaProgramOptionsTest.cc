#define BOOST_TEST_MODULE pcraster aguila program_options
#include <boost/test/unit_test.hpp>
#include "AguilaXSD.h"
#include "com_exception.h"
#include "ag_AguilaProgramOptions.h"


BOOST_AUTO_TEST_CASE(view_plus_syntax_to_view_constructor)
{
  using namespace ag;

  {
    // + is a string on its own
    // if viewValues is   a + b c + d + e f
    // then r should become
    // r[0][0] = a
    // r[0][1] = b
    // r[1][0] = c
    // r[1][1] = d
    // r[1][2] = e
    // r[2][0] = f
    std::vector<std::string> arguments;
    arguments.push_back("a");
    arguments.push_back("+");
    arguments.push_back("b");
    arguments.push_back("c");
    arguments.push_back("+");
    arguments.push_back("d");
    arguments.push_back("+");
    arguments.push_back("e");
    arguments.push_back("f");

    std::vector<std::vector<std::string> > result;
    result = AguilaProgramOptions::viewPlusSyntaxToViewCtor(arguments);

    BOOST_CHECK(result.size() == 3);
    BOOST_CHECK(result[0].size() == 2);
    BOOST_CHECK(result[0][0] == "a");
    BOOST_CHECK(result[0][1] == "b");
    BOOST_CHECK(result[1].size() == 3);
    BOOST_CHECK(result[1][0] == "c");
    BOOST_CHECK(result[1][1] == "d");
    BOOST_CHECK(result[1][2] == "e");
    BOOST_CHECK(result[2].size() == 1);
    BOOST_CHECK(result[2][0] == "f");
  }

  {
    // if viewValues is a+b c + d+e f
    // then r should become
    // r[0][0] = a+b
    // r[1][0] = c
    // r[1][1] = d+e
    // r[2][0] = f
    std::vector<std::string> arguments;
    arguments.push_back("a+b");
    arguments.push_back("c");
    arguments.push_back("+");
    arguments.push_back("d+e");
    arguments.push_back("f");

    std::vector<std::vector<std::string> > result;
    result = AguilaProgramOptions::viewPlusSyntaxToViewCtor(arguments);

    BOOST_CHECK(result.size() == 3);
    BOOST_CHECK(result[0].size() == 1);
    BOOST_CHECK(result[0][0] == "a+b");
    BOOST_CHECK(result[1].size() == 2);
    BOOST_CHECK(result[1][0] == "c");
    BOOST_CHECK(result[1][1] == "d+e");
    BOOST_CHECK(result[2].size() == 1);
    BOOST_CHECK(result[2][0] == "f");
  }

  {
    // volcano/volcano0.090+volcano/lava0000.090
    // results in
    // r[0][0] = volcano/volcano0.090+volcano/lava0000.090
    std::vector<std::string> arguments;
    arguments.push_back("volcano/volcano0.090+volcano/lava0000.090");

    std::vector<std::vector<std::string> > result;
    result = AguilaProgramOptions::viewPlusSyntaxToViewCtor(arguments);

    BOOST_CHECK(result.size() == 1);
    BOOST_CHECK(result[0].size() == 1);
    BOOST_CHECK(result[0][0] == "volcano/volcano0.090+volcano/lava0000.090");
  }
}


BOOST_AUTO_TEST_CASE(boost_options)
{
  using namespace ag;

  // char c1[7] = "aguila";
  // char c2[10] = "--mapView";
  // char c3[13] = "volcano0.090";
  // char c4[10] = "--mapView";
  // char c5[13] = "lava0000.080";

  // char *argv[]= { c1, c2, c3, c4, c5 };

  // AguilaProgramOptions apo(5, argv);

  // // Should create 2 map view elements:
  // pcrxml::VisualisationGroup const& group(
  //      apo.configuration().visualisationGroup());
  // pcrxml::VisualisationGroup::view_sequence const& views(group.view());

  // BOOST_CHECK_EQUAL(views.size(), size_t(2));
  // BOOST_CHECK(views[0].map().present());
  // BOOST_CHECK(views[1].map().present());
}


BOOST_AUTO_TEST_CASE(boost_options_to_xml)
{
  using namespace ag;

  {
    char c1[7] = "aguila";
    char c2[10] = "--mapView";
    char c3[13] = "volcano0.090";
    char *argv[]= { c1, c2, c3 };

    AguilaProgramOptions apo(3, argv);

    // should create 1 map view element:
    BOOST_CHECK(apo.configuration().visualisationGroup().view().size()==1);
    BOOST_CHECK(apo.configuration().visualisationGroup().view()[0].map().present());
  }

  {
    char c1[7] = "aguila";
    char c2[12] = "--drapeView";
    char c3[13] = "volcano0.090";
    char *argv[]= { c1, c2, c3 };

    AguilaProgramOptions apo(3, argv);

    // should create 1 map view element:
    BOOST_CHECK(apo.configuration().visualisationGroup().view().size()==1);
    BOOST_CHECK(apo.configuration().visualisationGroup().view()[0].drape().present());
    // choice is mutally exclusive
    BOOST_CHECK(!apo.configuration().visualisationGroup().view()[0].map().present());
  }

}


BOOST_AUTO_TEST_CASE(stackname_fix)
{
  using namespace ag;

  {
    char c1[7] = "aguila";
    char c2[17] = "volcano0.001+090";
    char *argv[]= { c1, c2 };

    AguilaProgramOptions apo(2, argv);
    BOOST_CHECK(apo.configuration().visualisationGroup().view().size()==1);
    BOOST_CHECK(apo.configuration().visualisationGroup().searchSpace().present());

    pcrxml::DataSpace const& s(
        apo.configuration().visualisationGroup().searchSpace().get());
    BOOST_CHECK(s.timesteps().size()==1);
    BOOST_CHECK(s.timesteps()[0].range().present());
    BOOST_CHECK(s.timesteps()[0].range()->begin()==1);
    BOOST_CHECK(s.timesteps()[0].range()->end()==90);
    BOOST_CHECK(s.timesteps()[0].range()->increment()==1);
  }

  {
    char c1[7] = "aguila";
    char c2[8] = "dem.map";
    char *argv[]= { c1, c2 };

    AguilaProgramOptions apo(2, argv);
    BOOST_CHECK(!apo.configuration().visualisationGroup().searchSpace().present());
  }

  {
    char c1[7] = "aguila";
    char c2[13] = "volcano0.090";
    char *argv[]= { c1, c2 };

    AguilaProgramOptions apo(2, argv);
    BOOST_CHECK(apo.configuration().visualisationGroup().view().size()==1);
    BOOST_CHECK(apo.configuration().visualisationGroup().searchSpace().present());

    pcrxml::DataSpace const& s(
        apo.configuration().visualisationGroup().searchSpace().get());
    BOOST_CHECK(s.timesteps().size()==1);
    BOOST_CHECK(s.timesteps()[0].range().present());
    BOOST_CHECK(s.timesteps()[0].range()->begin()==90);
    BOOST_CHECK(s.timesteps()[0].range()->end()==90);
    BOOST_CHECK(s.timesteps()[0].range()->increment()==1);
  }
}


BOOST_AUTO_TEST_CASE(drape_syntax)
{
  using namespace ag;

  {
    char c1[7]  = "aguila";
    char c2[2] = "a";
    char c3[2]  = "+";
    char c4[2] = "b";

    char *argv[]= { c1, c2, c3, c4 };

    AguilaProgramOptions apo(4, argv);
    BOOST_CHECK_EQUAL(
         apo.configuration().visualisationGroup().view().size(), size_t(1));
  }

  {
    char c1[7]  = "aguila";
    char c2[10] = "--mapView";
    char c3[2] = "a";
    char c4[2]  = "+";
    char c5[2] = "b";

    char *argv[]= { c1, c2, c3, c4, c5 };

    AguilaProgramOptions apo(5, argv);
    BOOST_CHECK_EQUAL(
         apo.configuration().visualisationGroup().view().size(), size_t(1));
  }

/*
  {
    char *argv[]= {
      "aguila",
      "b+b",
    };

    AguilaProgramOptions apo(2, argv);
    BOOST_CHECK(apo.configuration().visualisationGroup().view().size()==1);
  }

  {
    char *argv[]= {
      "aguila",
      "a/b+b",
    };
    try {
      exceptionThrown = false;
      AguilaProgramOptions apo(2, argv);
    }
    catch(com::Exception const& exception) {
      exceptionThrown = true;
      BOOST_CHECK(exception.messages() ==
         "Data source a/b+b in /:\nwrong format for stack name");
    }

    BOOST_CHECK(exceptionThrown);
  }
*/

  // aguila a/b+b
  // aguila a/b+a/b

}


BOOST_AUTO_TEST_CASE(multiple_views)
{
  using namespace ag;

  // {
  //   char c1[7]  = "aguila";
  //   char c2[12] = "--timesteps";
  //   char c3[9]  = "[1, 250]";
  //   char c4[10] = "--mapView";
  //   char c5[4]  = "dem";
  //   char c6[8]  = "volcano";
  //   char c7[12] = "--timeGraph";
  //   char c8[4]  = "dem";

  //   char *argv[]= { c1, c2, c3, c4, c5, c6, c7, c8 };

  //   AguilaProgramOptions apo(8, argv);

  //   pcrxml::VisualisationGroup const& group(
  //        apo.configuration().visualisationGroup());
  //   pcrxml::VisualisationGroup::view_sequence const& views(group.view());
  //   BOOST_CHECK_EQUAL(views.size(), size_t(2));
  //   // Two names in the map view.
  //   BOOST_CHECK(views[0].map().present()); 
  //   BOOST_CHECK_EQUAL(views[0].map().get().item().size(), size_t(2)); 
  //   // One name in the time graph view.
  //   BOOST_CHECK(views[1].timeGraph().present()); 
  //   BOOST_CHECK_EQUAL(views[1].timeGraph().get().item().size(), size_t(1)); 

  //   BOOST_CHECK(group.searchSpace().present());
  //   pcrxml::DataSpace const& space(group.searchSpace().get());
  //   BOOST_CHECK_EQUAL(space.timesteps().size(), size_t(1));
  //   BOOST_CHECK(space.timesteps()[0].range().present());
  //   BOOST_CHECK_EQUAL(space.timesteps()[0].range()->begin(), size_t(1));
  //   BOOST_CHECK_EQUAL(space.timesteps()[0].range()->end(), size_t(250));
  //   BOOST_CHECK_EQUAL(space.timesteps()[0].range()->increment(), size_t(1));
  // }
}
