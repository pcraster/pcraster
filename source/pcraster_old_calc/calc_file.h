#ifndef INCLUDED_CALC_FILE
#define INCLUDED_CALC_FILE

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace calc {

class File  {
  const std::string d_name;
#  ifdef BORLANDC
  void testCaseOfName() const;
#  endif
public:
  static bool d_testCaseTypeOnExistingName;
  File(const std::string& name): d_name(name) {};
  //! check if name can be used as an output file name
  void validateOutputName() const;
  void validateExisting() const;
};

}

#endif
