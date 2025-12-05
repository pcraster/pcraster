#include "PCRasterXSD.h"

#include <string>


pcrxml::LinkInExecuteInput strToLinkInExecuteInput(std::string const& xml);
pcrxml::LinkInCheckInput   strToLinkInCheckInput(std::string const& xml);
std::string linkInCheckResultToStr(pcrxml::LinkInCheckResult const& l);
std::string wrapToLinkInExecuteResult(std::string const& errorMsg);
