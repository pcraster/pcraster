#ifndef INCLUDED_MODFLOW_CALC_XML
#define INCLUDED_MODFLOW_CALC_XML

#include "PCRasterXSD.h"

#include <string>

pcrxml::LinkInExecuteInput strToLinkInExecuteInput(std::string const& xml);
pcrxml::LinkInCheckInput   strToLinkInCheckInput(std::string const& xml);
std::string linkInCheckResultToStr(pcrxml::LinkInCheckResult const& l);
std::string wrapToLinkInExecuteResult(std::string const& errorMsg);

#endif // INCLUDED_MODFLOW_CALC_XML