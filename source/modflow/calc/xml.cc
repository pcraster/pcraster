#ifndef INCLUDED_XML
#include "xml.h"
#define INCLUDED_XML
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_XERCESC_UTIL_PLATFORMUTILS
#include <xercesc/util/PlatformUtils.hpp>
#define INCLUDED_XERCESC_UTIL_PLATFORMUTILS
#endif

struct Xercesc {
  Xercesc() {
    try {
      xercesc::XMLPlatformUtils::Initialize();
    } catch (const xercesc::XMLException& toCatch) {
      std::cerr << toCatch.getMessage()  << std::endl;
    // Do your failure processing here
      throw;
    }
  }
  ~Xercesc() {
    xercesc::XMLPlatformUtils::Terminate();
  }
};


static xml_schema::namespace_infomap namespaceInfoMap() {
  xml_schema::namespace_infomap map;
  map["pcr"].name = "http://www.pcraster.nl/pcrxml";
  map["pcr"].schema = "PCRaster.xsd";
  return map;
}


pcrxml::LinkInExecuteInput strToLinkInExecuteInput(std::string const& xml){
  Xercesc call;
  std::istringstream iss(xml);
#ifdef DEBUG_DEVELOP
  return *pcrxml::linkInExecuteInput(iss);
#else
  return *pcrxml::linkInExecuteInput(iss, xml_schema::flags::dont_validate);
#endif
}


pcrxml::LinkInCheckInput strToLinkInCheckInput(std::string const& xml){
  Xercesc call;
  std::istringstream iss(xml);
#ifdef DEBUG_DEVELOP
  return *pcrxml::linkInCheckInput(iss);
#else
  return *pcrxml::linkInCheckInput(iss, xml_schema::flags::dont_validate);
#endif
}


std::string wrapToLinkInExecuteResult(std::string const& errorMsg){
  pcrxml::LinkInExecuteResult r;
  r.error(errorMsg);
  std::ostringstream s;
  pcrxml::linkInExecuteResult(s,r,namespaceInfoMap());
  return s.str();
}


std::string linkInCheckResultToStr(pcrxml::LinkInCheckResult const& l){
  std::ostringstream s;
  pcrxml::linkInCheckResult(s,l,namespaceInfoMap());
  return s.str();
}
