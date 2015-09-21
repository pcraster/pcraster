#define BOOST_TEST_MODULE pcraster command_line_application
#include <sstream>
#include <boost/test/unit_test.hpp>
#define protected public  // Testing protected members.
#include "dev_CommandLineApplication.h"


BOOST_AUTO_TEST_CASE(command_line_application_)
{
  using namespace dev;
  namespace po = boost::program_options;

  {
    char arg0[8] = "goforit";
    char* argv[] = { arg0 };
    unsigned short int argc = 1;
    CommandLineApplication application(argc, argv);

    application.genericOptions().add_options()
         ("aaa", "aaa aaa aaa")
         ("bbb", "bbb bbb bbb");
    application.hiddenOptions().add_options()
         ("ccc", po::value<std::vector<std::string> >());
    application.addPositionalOption(
         "ccc", -1, "ccc ccc ccc");

    std::ostringstream stream;
    application.usage(stream);
    stream << std::ends;

    std::ostringstream result;
    result <<
         "goforit options ccc ...\n"
         "\n"
         "options:\n"
         "  --help                Produce help message.\n"
         "  --version             Show version.\n"
         "  --aaa                 aaa aaa aaa\n"
         "  --bbb                 bbb bbb bbb\n"
         "\n"
         "ccc: ccc ccc ccc\n"
         ;
    result << std::ends;

    BOOST_CHECK_EQUAL(stream.str(), result.str());
  }
}
