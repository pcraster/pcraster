#ifndef INCLUDED_CALC_INFOSCRIPT
#define INCLUDED_CALC_INFOSCRIPT

#include <fstream>
#include <string>


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
