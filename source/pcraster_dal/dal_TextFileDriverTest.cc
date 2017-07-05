#define BOOST_TEST_MODULE pcraster dal text_file_driver
#include <boost/test/unit_test.hpp>
#include "dal_TextFileDriver.h"


class TextFileDriverWrapper : public dal::TextFileDriver {
public:
  TextFileDriverWrapper() : dal::TextFileDriver() {
  }
  void             determineTypeId     (
                        std::vector<std::string> const& row,
                        dal::TypeId& typeId) const {
    dal::TextFileDriver::determineTypeId(row, typeId);
  }
};

// Idea came from aguila dal/testdata/int2mv0.hdr command. We need
// to implement StringType class and update Types class to support
// string values (see constructor).
BOOST_AUTO_TEST_CASE(determine_type_id)
{
  using namespace dal;

  TextFileDriverWrapper driver;

  {
    std::vector<std::string> row;
    row.push_back("NROWS");

    TypeId typeId = TI_NR_TYPES;
    driver.determineTypeId(row, typeId);
    BOOST_WARN_EQUAL(typeId, TI_STRING);
  }

  {
    std::vector<std::string> row;
    row.push_back("NROWS");
    row.push_back("4");

    TypeId typeId = TI_NR_TYPES;
    driver.determineTypeId(row, typeId);
    BOOST_WARN_EQUAL(typeId, TI_STRING);
  }
}
