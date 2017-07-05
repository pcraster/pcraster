#define BOOST_TEST_MODULE pcraster command_line_application
#include "dev_CommandLineApplication.h"
#include <boost/test/unit_test.hpp>
#include <sstream>


class CommandLineApplicationWrapper : public dev::CommandLineApplication {
public:
  CommandLineApplicationWrapper(unsigned short argc, char** argv) : CommandLineApplication( argc, argv){};

  boost::program_options::options_description& genericOptions() {
    return CommandLineApplication::genericOptions();
  };

  boost::program_options::options_description& hiddenOptions() {
    return CommandLineApplication::hiddenOptions();
  };

  void usage(std::ostream& stream) const {
    CommandLineApplication::usage(stream);
  };

  void addPositionalOption(std::string const& name, short maxCount, std::string description){
    CommandLineApplication::addPositionalOption(name, maxCount, description);
  };
};


BOOST_AUTO_TEST_CASE(command_line_application_)
{
  using namespace dev;
  namespace po = boost::program_options;

  {
    char arg0[8] = "goforit";
    char* argv[] = { arg0 };
    unsigned short int argc = 1;
    CommandLineApplicationWrapper application(argc, argv);

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
