/*
 C++ LinkIn API example for the PCRaster Model Engine

 Note that for development (DEBUG_DEVELOP) of a library the files
 PCRaster.xsd and commonTypes.xsd must be located in the directory
 where the pcrScript is executed, otherwise the validation of the
 library xml manifest file fails.
 In release mode this requirement is not applicable.
*/
#include "stddefx.h"
#include "pcrlinkin.h"
#include "pcrtypes.h"
#include "xml.h"
#include <cassert>
#include <cstddef>
#include <string>
#include <map>
#include <stdexcept>
#include <iostream>

static std::string xmlResultBuffer;

class  Class1 {
  float    firstConstructorArgument;

public:
  typedef std::map<std::string,Class1 *> Objects;
  static  Objects objects;

  static void construct(
                         pcrxml::LinkInExecuteInput const& l,
                         LinkInTransferArray linkInTransferArray)
  {
    std::string objName=std::string(l.callPoint().object()->objectName());
    auto i=objects.find(objName);
      // instance already exists, constructing again means deleting the old one
    if (i!=objects.end())
      delete i->second;
    auto *c = new Class1(l,linkInTransferArray);
    objects.insert(std::make_pair(objName,c));
  }

  static void callMethod(
                          pcrxml::LinkInExecuteInput const& l,
                          LinkInTransferArray linkInTransferArray)
  {
    std::string objName=std::string(l.callPoint().object()->objectName());
    auto i=objects.find(objName);
    assert(i!=objects.end());

    std::string methodName=std::string(l.callPoint().object()->methodName().get());
    if (methodName=="operation") {
      i->second->operation(l,linkInTransferArray);
    }
  }

  Class1([[maybe_unused]] pcrxml::LinkInExecuteInput const& l,
         LinkInTransferArray linkInTransferArray)
  {
    assert(l.argument().size()==1);
    firstConstructorArgument = ((float *)linkInTransferArray[0])[0];
  }

  static void operationArgumentCheck(std::string const& op) {
    if (op.empty())
      throw std::domain_error("string-argument missing, select add or div");
    if (op != "add" && op != "div" )
      throw std::domain_error("unknown operation in string-argument, select add or div");
  }

  void operation(pcrxml::LinkInExecuteInput const& l,
                 LinkInTransferArray linkInTransferArray)
  {
      // check error
    assert(l.stringArgument()); // should be checked in pcr_LinkInCheck
    std::string op = std::string(l.stringArgument().get());
    assert(op=="div" || op=="add"); // should be checked in pcr_LinkInCheck

      // run time error voor div
    if (op=="div" && firstConstructorArgument==0.0F)
      throw std::domain_error("attempting division by 0");

    auto       *result      = (float *)linkInTransferArray[0];
    const auto *leftOperand = (const float *)linkInTransferArray[1];
    size_t       len=1; // NonSpatial
    if (l.argument()[0].spatialType() == pcrxml::SpatialType::Spatial)
      len = static_cast<size_t>(l.context().areaMap().nrRows() *
            l.context().areaMap().nrCols());
    for(size_t i=0; i < len; ++i) {
      if (pcr::isMV(leftOperand[i]))
        pcr::setMV(result);
      else {
        if (op=="add")
          result[i]=leftOperand[i]+firstConstructorArgument;
        else
          result[i]=leftOperand[i]/firstConstructorArgument;
      }
    }
  }
};

Class1::Objects Class1::objects;

static void checkerBoard(
                          pcrxml::LinkInExecuteInput const& l,
                          LinkInTransferArray linkInTransferArray)
{
  assert(l.result().size()==1);

  auto *result=(unsigned char *)linkInTransferArray[0];

  unsigned char value = 0;
  size_t nrRows=l.context().areaMap().nrRows();
  size_t nrCols=l.context().areaMap().nrCols();
  size_t cell=0;
  for(size_t r=0;r<nrRows; r++) {
    value=r%2;
    for(size_t c=0;c<nrCols; c++) {
      result[cell]=value;
      cell++;
      value=!value;
    }
  }
}

PCR_DLL_FUNC (const char *) pcr_LinkInExecute(
                                               const char *xml,
                                               LinkInTransferArray linkInTransferArray)
{
  try {
    pcrxml::LinkInExecuteInput l(strToLinkInExecuteInput(xml));

    if (l.callPoint().function()) {
      if (l.callPoint().function()->name()=="checkerBoard")
        checkerBoard(l,linkInTransferArray);
    } else {
      assert(l.callPoint().object());
      if (l.callPoint().object()->constructor()) {
        if (l.callPoint().object()->className()=="Class1")
          Class1::construct(l,linkInTransferArray);
      } else {
        assert(l.callPoint().object()->methodName());
        if (l.callPoint().object()->className()=="Class1")
          Class1::callMethod(l,linkInTransferArray);
      }
    }
  }
  catch(xml_schema::parsing const& e){
    std::cerr <<  e << '\n';
    xmlResultBuffer = wrapToLinkInExecuteResult(e.what());
    return xmlResultBuffer.c_str();
  } catch(std::exception const& e) {
    xmlResultBuffer= wrapToLinkInExecuteResult(e.what());
    return xmlResultBuffer.c_str();
  } catch(...) {
    xmlResultBuffer= wrapToLinkInExecuteResult("unknown exception");
    return xmlResultBuffer.c_str();
  }
  // no error
  return nullptr;
}

PCR_DLL_FUNC (const char *) pcr_LinkInCheck(
                                             const char *xml)
{
  pcrxml::LinkInCheckResult returnResult;

  try {
    pcrxml::LinkInCheckInput  input(strToLinkInCheckInput(xml));

   // default just copy the input info
    returnResult.result(input.result());
    returnResult.argument(input.argument());

   // only Class1::operation needs checking
   // and a modified returnResult
    if(   input.callPoint().object()
          && input.callPoint().object()->className()=="Class1"
          && input.callPoint().object()->methodName()   // method call
          && input.callPoint().object()->methodName().get()=="operation") {

     // must have string argument with value div or add
            std::string op;
            if (input.stringArgument())
              op = std::string(input.stringArgument().get());
            Class1::operationArgumentCheck(op);

     // spatialType of result equals spatialType of argument
            returnResult.result()[0].spatialType(returnResult.argument()[0].spatialType());
          }
  }
  catch(xml_schema::parsing const& e){
    std::cerr << e << '\n';
    xmlResultBuffer = wrapToLinkInExecuteResult(e.what());
    return xmlResultBuffer.c_str();

  } catch(std::exception const& e) {
    returnResult.error(e.what());
  } catch(...) {
    returnResult.error("unknown exception");
  }
  xmlResultBuffer= linkInCheckResultToStr(returnResult);
  return xmlResultBuffer.c_str();
}
