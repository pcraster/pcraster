#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif

pcrxml::LinkInExecuteInput strToLinkInExecuteInput(std::string const& xml);
pcrxml::LinkInCheckInput   strToLinkInCheckInput(std::string const& xml);
std::string linkInCheckResultToStr(pcrxml::LinkInCheckResult const& l);
std::string wrapToLinkInExecuteResult(std::string const& errorMsg);
