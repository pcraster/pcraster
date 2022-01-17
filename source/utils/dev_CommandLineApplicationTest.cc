#define BOOST_TEST_MODULE pcraster command_line_application
#include "dev_CommandLineApplication.h"
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <utility>


class CommandLineApplicationWrapper : public dev::CommandLineApplication {
public:
  CommandLineApplicationWrapper(unsigned short argc, char** argv) : CommandLineApplication( argc, argv){};

  clipp::group& genericOptions() {
    return CommandLineApplication::genericOptions();
  };

  clipp::group& hiddenOptions() {
    return CommandLineApplication::hiddenOptions();
  };

  clipp::group& positionalOptions() {
    return CommandLineApplication::positionalOptions();
  };

  void usage(std::ostream& stream) const {
    CommandLineApplication::usage(stream);
  };

  void addPositionalOption(std::string const& name, short maxCount, std::string description){
    CommandLineApplication::addPositionalOption(name, maxCount, std::move(description));
  };
};


BOOST_AUTO_TEST_CASE(command_line_application_)
{
  using namespace dev;

  {
    char arg0[8] = "goforit";
    char* argv[] = { arg0 };
    unsigned short int argc = 1;
    std::vector<std::string> files;
    CommandLineApplicationWrapper application(argc, argv);

    application.genericOptions() = application.genericOptions().push_back(
      clipp::option("--aaa").doc("aaa aaa aaa"),
      clipp::option("--bbb").doc("bbb bbb bbb")
    );

    application.positionalOptions() = clipp::group(clipp::values("ccc", files).doc("ccc ccc ccc"));

    std::ostringstream stream;
    application.usage(stream);
    stream << std::ends;

    std::ostringstream result;
    result <<
         "goforit options <ccc>...\n"
         "\n"
         "options:\n"
         "  --help                Produce help message.\n"
         "  --version             Show version.\n"
         "  --aaa                 aaa aaa aaa\n"
         "  --bbb                 bbb bbb bbb\n"
         "\n"
         "<ccc>...                ccc ccc ccc\n"
         ;
    result << std::ends;

    BOOST_CHECK_EQUAL(stream.str(), result.str());
  }
}
