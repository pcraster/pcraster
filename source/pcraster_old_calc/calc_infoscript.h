#ifndef INCLUDED_CALC_INFOSCRIPT
#define INCLUDED_CALC_INFOSCRIPT

#ifndef INCLUDED_FSTREAM
#include <fstream>
#define INCLUDED_FSTREAM
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif


namespace calc {

class InfoScript {
  std::ofstream d_str;

public:
   InfoScript(const std::string& htmlFile);
  ~InfoScript();

  std::ofstream& stream ();
  void           parTag (const std::string& parName);
};

}

#endif
