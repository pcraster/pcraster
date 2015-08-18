#include "stddefx.h"

#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif

calc::InfoScript::InfoScript(const std::string& htmlFile):
  d_str(htmlFile.c_str())
{
   POSTCOND(d_str.is_open());
   d_str << "<HTML>\n";
   d_str << "<SCRIPT> function showtype(msg) { status=msg; } </SCRIPT>\n";
}

calc::InfoScript::~InfoScript()
{ 
  d_str << "</HTML>\n";
}

std::ofstream& calc::InfoScript::stream()
{
  return d_str;
}

void calc::InfoScript::parTag(const std::string& parName)
{
  d_str << "<A HREF=\"#" << parName << "\">" << parName << "</A>\n";
}
