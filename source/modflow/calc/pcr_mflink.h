#ifndef INCLUDED_PCRMODFLOWLINK
#define INCLUDED_PCRMODFLOWLINK

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_MAP
#include <map>
#define INCLUDED_MAP
#endif

#ifndef INCLUDED_ASSERT
#include <assert.h>
#define INCLUDED_ASSERT
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRLINKIN
#include "pcrlinkin.h"
#define INCLUDED_PCRLINKIN
#endif

#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

#ifndef INCLUDED_XML
#include "xml.h"
#define INCLUDED_XML
#endif

#ifndef INCLUDED_PCRMODFLOW
#include "pcrmodflow.h"
#define INCLUDED_PCRMODFLOW
#endif

// Module headers.



static std::string xmlResultBuffer;

class PCR_DLL_CLASS ModflowLink {
  private:
    PCRModflow *d_pcrmf;

 public:
  typedef std::map<std::string,ModflowLink *> Objects;
  static  Objects objects;

  static void construct(pcrxml::LinkInExecuteInput const& l) // LinkInTransferArray linkInTransferArray
  {
    std::string objName=std::string(l.callPoint().object()->objectName());
    Objects::iterator i=objects.find(objName);
    // it is only allows to construct one modflow object
    if(objects.size() > 0){
      std::cout << "Warning: only one PCRasterModflow extension object should be used, found additional object '" << objName << "'. Previous object will be deleted." << std::endl;
    }
    // instance already exists, constructing again means deleting the old one
    if (i!=objects.end()){
      delete i->second;
    }
    ModflowLink *c = new ModflowLink(l);
    objects.insert(std::make_pair(objName,c));
  }


  static void callMethod(
			 pcrxml::LinkInExecuteInput const& l,
			 LinkInTransferArray linkInTransferArray)
    {
      std::string objName=std::string(l.callPoint().object()->objectName());
      Objects::iterator i=objects.find(objName);
      assert(i!=objects.end());

      std::string methodName=std::string(l.callPoint().object()->methodName().get());

      if(methodName == "createBottomLayer") {
	i->second->createBottom(linkInTransferArray);
      }
      else if(methodName == "addLayer") {
	i->second->addLayer(linkInTransferArray);
      }
      else if(methodName == "addConfinedLayer") {
	i->second->addConfinedLayer(linkInTransferArray);
      }
      else if(methodName=="run") {
	i->second->run();
      }
      else if (methodName=="setBoundary") {
	i->second->setBoundary(linkInTransferArray);
      }
      else if (methodName=="setInitialHead") {
	i->second->setHead(linkInTransferArray);
      }
      else if (methodName=="getHeads") {
	i->second->getHead(linkInTransferArray);
      }
      else if (methodName=="getRiverLeakage") {
	i->second->getRivLeak(linkInTransferArray);
      }
      else if (methodName=="setRiver") {
	i->second->setRiver(linkInTransferArray);
      }
      else if(methodName=="setDISParameter"){
	i->second->setDISParams(linkInTransferArray);
      }
      else if(methodName=="setNoFlowHead"){
	i->second->setHNOFLO(linkInTransferArray);
      }
      else if(methodName=="setHorizontalAnisotropy"){
	i->second->setTRPY(linkInTransferArray);
      }
      else if(methodName=="setDryHead"){
	i->second->setHDRY(linkInTransferArray);
      }
      else if(methodName=="setWettingParameter"){
	i->second->setWettingParameter(linkInTransferArray);
      }
      else if(methodName=="setWetting"){
	i->second->setWetting(linkInTransferArray);
      }
      else if(methodName=="setStorage"){
	i->second->setStorage(linkInTransferArray);
      }
      else if(methodName=="setRecharge"){
	i->second->setRecharge(linkInTransferArray);
      }
      else if(methodName=="getRecharge"){
	i->second->getRecharge(linkInTransferArray);
      }
      else if(methodName=="setDrain"){
	i->second->setDrain(linkInTransferArray);
      }
      else if(methodName=="getDrain"){
	i->second->getDrain(linkInTransferArray);
      }
      else if(methodName=="setPCG"){
	i->second->setPCG(linkInTransferArray);
      }
      else if(methodName=="setSIP"){
	i->second->setSIP(linkInTransferArray);
      }
      else if(methodName=="setSOR"){
	i->second->setSOR(linkInTransferArray);
      }
      else if(methodName=="setDSP"){
	i->second->setDSP(linkInTransferArray);
      }
      else if(methodName == "setWell"){
	i->second->setWell(linkInTransferArray);
      }
      else if(methodName == "setIndicatedRecharge"){
	i->second->setIndicatedRecharge(linkInTransferArray);
      }
      else if(methodName == "setConductivity"){
	i->second->setConductivity(linkInTransferArray);
      }
    else if(methodName == "getStorage"){
      i->second->getStorage(linkInTransferArray);
    }
    else if(methodName == "getConstantHead"){
      i->second->getConstantHead(linkInTransferArray);
    }
    else if(methodName == "getRightFace"){
      i->second->getRightFace(linkInTransferArray);
    }
    else if(methodName == "getFrontFace"){
      i->second->getFrontFace(linkInTransferArray);
    }
    else if(methodName == "getLowerFace"){
      i->second->getLowerFace(linkInTransferArray);
    }    
    else{
      std::cout << "Programming error: Can not assign " << methodName << std::endl;
    }
  }
  ~ModflowLink();
  ModflowLink(pcrxml::LinkInExecuteInput const& l);
  void createBottom(LinkInTransferArray linkInTransferArray);
  void addLayer(LinkInTransferArray linkInTransferArray);
  void addConfinedLayer(LinkInTransferArray linkInTransferArray);
  void setWetting(LinkInTransferArray linkInTransferArray);
  void setTRPY(LinkInTransferArray linkInTransferArray);
  void setHDRY(LinkInTransferArray linkInTransferArray);
  void setWettingParameter(LinkInTransferArray linkInTransferArray);
  void setStorage(LinkInTransferArray linkInTransferArray);
  void setDISParams(LinkInTransferArray linkInTransferArray);
  void setHNOFLO(LinkInTransferArray linkInTransferArray);
  void run();
  void setBoundary(LinkInTransferArray linkInTransferArray);
  void setHead(LinkInTransferArray linkInTransferArray);
  void setConductivity(LinkInTransferArray linkInTransferArray);
  void getHead(LinkInTransferArray linkInTransferArray);
  void getRivLeak(LinkInTransferArray linkInTransferArray);
  void setRiver(LinkInTransferArray linkInTransferArray);
  void setRiverHeads(LinkInTransferArray linkInTransferArray);
  void setRiverBottom(LinkInTransferArray linkInTransferArray);
  void setRiverCond(LinkInTransferArray linkInTransferArray);

  // BCF package
  void getStorage(LinkInTransferArray linkInTransferArray);
  void getConstantHead(LinkInTransferArray linkInTransferArray);
  void getRightFace(LinkInTransferArray linkInTransferArray);
  void getFrontFace(LinkInTransferArray linkInTransferArray);
  void getLowerFace(LinkInTransferArray linkInTransferArray);

  // RCH package
  void setRecharge(LinkInTransferArray linkInTransferArray);
  void setIndicatedRecharge(LinkInTransferArray linkInTransferArray);
  void getRecharge(LinkInTransferArray linkInTransferArray);

  // DRN package
  void setDrain(LinkInTransferArray linkInTransferArray);
  void getDrain(LinkInTransferArray linkInTransferArray);

  // WEL package
  void setWell(LinkInTransferArray linkInTransferArray);

  // Solver packages
  void setSOR(LinkInTransferArray linkInTransferArray);
  void setPCG(LinkInTransferArray linkInTransferArray);
  void setSIP(LinkInTransferArray linkInTransferArray);
  void setDSP(LinkInTransferArray linkInTransferArray);

};

ModflowLink::Objects ModflowLink::objects;



PCR_DLL_FUNC (const char *) pcr_LinkInExecute(const char *xml,
                                              LinkInTransferArray linkInTransferArray){
  
  try {
    pcrxml::LinkInExecuteInput l(strToLinkInExecuteInput(xml));

    if (l.callPoint().function()) {
    } else if(l.callPoint().object()){
      assert(l.callPoint().object());
      if (l.callPoint().object()->constructor()) {
	if (l.callPoint().object()->className()=="initialise")
	  ModflowLink::construct(l);
      } else {
	assert(l.callPoint().object()->methodName());
	if (l.callPoint().object()->className()=="initialise")
	  ModflowLink::callMethod(l,linkInTransferArray);
      }
    }
    else{
      std::cout << "No function or object " << std::endl;
    }
  }
  catch (xml_schema::parsing const& ){
    //std::cerr << "xml err: " << e << std::endl;
  }
  catch(std::exception const& e) {
    std::cout << e.what() << std::endl;
    xmlResultBuffer= wrapToLinkInExecuteResult(e.what());
    return xmlResultBuffer.c_str();
  } 
  catch(...) {
    xmlResultBuffer= wrapToLinkInExecuteResult("unknown exception");
    std::cerr << "unknown exception" << std::endl;
    return xmlResultBuffer.c_str();
  }
  // no error
  return 0;
}


PCR_DLL_FUNC (const char *) pcr_LinkInCheck(const char *xml){
  pcrxml::LinkInCheckResult returnResult;
  try {
    pcrxml::LinkInCheckInput  input(strToLinkInCheckInput(xml));
    // default just copy the input info
    returnResult.result(input.result());
    returnResult.argument(input.argument());

    // and a modified returnResult
    if(   input.callPoint().object()
          && input.callPoint().object()->className()=="initialise"
          && input.callPoint().object()->methodName()   // method call
          && input.callPoint().object()->methodName().get()=="setLayer") {

      returnResult.result()[0].spatialType(returnResult.argument()[0].spatialType());
    }

  }
  catch(std::exception const& e) {
    std::cout << e.what() << std::endl;
    returnResult.error(e.what());
  } 
  catch(...) {
    returnResult.error("unknown exception");
  }
  xmlResultBuffer= linkInCheckResultToStr(returnResult);
  return xmlResultBuffer.c_str();
}



#endif // INCLUDED_PCRMODFLOWLINK
