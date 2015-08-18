#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#include "PCRasterXSD.h"

pcrxml::LinkInExecuteInput strToLinkInExecuteInput(std::string const& xml);
pcrxml::LinkInCheckInput   strToLinkInCheckInput(std::string const& xml);
std::string linkInCheckResultToStr(pcrxml::LinkInCheckResult const& l);
std::string wrapToLinkInExecuteResult(std::string const& errorMsg);
