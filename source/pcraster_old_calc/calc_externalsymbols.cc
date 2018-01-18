#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_EXTERNALSYMBOLS
#include "calc_externalsymbols.h"
#define INCLUDED_CALC_EXTERNALSYMBOLS
#endif

// Library headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

#ifndef INCLUDED_CSTDLIB
#include <cstdlib>
#define INCLUDED_CSTDLIB
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif

// Module headers.
#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif

#ifndef INCLUDED_CALC_MODELLINK
#include "calc_modellink.h"
#define INCLUDED_CALC_MODELLINK
#endif

#ifndef INCLUDED_CALC_QUOTE
#include "calc_quote.h"
#define INCLUDED_CALC_QUOTE
#endif



/*!
  \file
  This file contains the implementation of the ExternalSymbols class.
*/



//------------------------------------------------------------------------------

calc::ModelLinkProxy::ModelLinkProxy(const std::string& name,
                   PCR_EXTERNAL_MODELLINK_CREATOR creator)
  : d_name(name), d_creator(creator)
{
  PRECOND(creator);
}



calc::ModelLinkProxy::~ModelLinkProxy()
{
}



const std::string& calc::ModelLinkProxy::name() const
{
  return d_name;
}



const calc::PCR_EXTERNAL_MODELLINK_CREATOR
                   calc::ModelLinkProxy::creator() const
{
  return d_creator;
}

//------------------------------------------------------------------------------
// DEFINITION OF STATIC EXTERNALSYMBOLS MEMBERS
//------------------------------------------------------------------------------

calc::ExternalSymbols* calc::ExternalSymbols::d_instance = 0;



calc::ExternalSymbols* calc::ExternalSymbols::instance()
{
  if(!d_instance) {
    d_instance = new ExternalSymbols();
  }

  return d_instance;
}



//! Deletes the layered instance. Information about external symboles is lost.
/*!
*/
void calc::ExternalSymbols::clear()
{
  if(d_instance) {
    delete d_instance;
    d_instance=0;
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF EXTERNALSYMBOLS MEMBERS
//------------------------------------------------------------------------------

calc::ExternalSymbols::ExternalSymbols()
{
}



//! dtor
calc::ExternalSymbols::~ExternalSymbols()
{
  for(size_t i = 0; i < d_libraries.size(); i++) {
    delete d_libraries[i];
  }
}



void calc::ExternalSymbols::addFunction(const PCR_EXTERNAL_FUNCTION_SYNOPSIS& fd)
{
  // Check if an external function with the same name doesn't already exist.
  const char* functionName = fd.name;
  if(find(functionName)) {
    std::ostringstream str;
    str << "function"
        << quote(functionName)
        << ", already a known function name";
    throw com::Exception(str.str());
  }


  d_table.push_back(
   ExternalFunction(
    Operator(fd.name,
      (MAJOR_CODE)(nrInternalOpCodes()+d_table.size()),
             // <-- CW must become size of other + current
      SYNTAX_FUNC,
      VS_FIELD,      // vs is determined when calling the resultTypes function
      ST_DERIVED,    // idem
      fd.nrArgs,EXEC_EXTERN,
      d_table.size(), // minor
      CG_PLAIN),  // or special of new type ?
      fd.resultTypes, fd.algorithm));

  for(int i=0; i < std::abs(fd.nrArgs); i++)
    d_table.back().pushBackArg(
      fd.argTypes[i].vs,
      fd.argTypes[i].st);
}



void calc::ExternalSymbols::addModelLink(
                   const PCR_EXTERNAL_MODELLINK_SYNOPSIS& synopsis)
{
  // Check if a link with the same name doesn't already exist.
  const char* modelLinkName = synopsis.name;
  if(findModelLinkProxy(modelLinkName)) {
    std::ostringstream str;
    str << "model link "
        << quote(modelLinkName)
        << ", already a known model link name";
    throw com::Exception(str.str());
  }

  d_modelLinkProxies.push_back(
   ModelLinkProxy(synopsis.name, synopsis.creator));
}



struct name_is {
  std::string value;
  name_is(const std::string& val) : value(val) {}
  bool operator()(const calc::ExternalFunction& x) const {
      return x.name() == value;
    }
};

namespace calc {

struct linkNameIs {

  std::string d_name;

  linkNameIs(const std::string& name) : d_name(name) {}

  bool operator()(const ModelLinkProxy& proxy) const {
    return proxy.name() == d_name;
  }
};

}



const calc::Operator* calc::ExternalSymbols::find(const std::string& name) const
{
  std::vector<calc::ExternalFunction>::const_iterator i =
    std::find_if(d_table.begin(),d_table.end(),name_is(name));
  if (i != d_table.end())
    return  &(*i);
  return 0;
}



const calc::ModelLinkProxy* calc::ExternalSymbols::findModelLinkProxy(
                   const std::string& name) const
{
  std::vector<ModelLinkProxy>::const_iterator it =
                   std::find_if(d_modelLinkProxies.begin(),
                   d_modelLinkProxies.end(), linkNameIs(name));

  if (it != d_modelLinkProxies.end()) {
    return &(*it);
  }

  return 0;
}



/* load a library
 * the library is kept open
 * there is no clean up code to close the library
 * return 1 in case of serious error
 * 0 if succesfull loaded or non serious error
 * \todo still uses libmisc Warning printing to stderr!
 */
void calc::ExternalSymbols::loadDynamicLibrary(const std::string& extLibNoExt)
{
  try {

    // Open library.
    com::DynamicLibrary* lib = new com::DynamicLibrary(extLibNoExt);
    d_libraries.push_back(lib);

    typedef PCR_EXTERNAL_FUNCTION_LIST * (*GET_LIST_PTR)(void);
    typedef PCR_EXTERNAL_MODELLINK_LIST * (*MODELLINK_LIST_PTR)(void);

    try {

      // Retrieve information about functions from the lib.
      if(lib->hasSymbol("GetFunctionList")) {

        GET_LIST_PTR getListPtr =
                   (GET_LIST_PTR)lib->loadSymbol("GetFunctionList");
        PCR_EXTERNAL_FUNCTION_LIST funcList(*((*getListPtr)()));

        for(int i=0; i < funcList.nrFunctions; i++) {
          addFunction(funcList.functionSynopsisList[i]);
        }
      }

      // Retrieve information about modellinks from the lib.
      if(lib->hasSymbol("GetModelLinkList")) {

        MODELLINK_LIST_PTR modelLinkListPtr =
                   (MODELLINK_LIST_PTR)lib->loadSymbol("GetModelLinkList");
        PCR_EXTERNAL_MODELLINK_LIST modelLinkList(*((*modelLinkListPtr)()));

        for(int i=0; i < modelLinkList.nrModelLinks; i++) {
          addModelLink(modelLinkList.modelLinkSynopsisList[i]);
        }
      }
    }
    catch(com::Exception& e) {
      std::ostringstream str;
      str << "dynamic library "
          << quote(extLibNoExt)
          << ", error while loading ";
      e.prepend(str.str());
      throw;
    }
  }
  catch ( const com::DynamicLibraryException& err) {
    Warning(err.messages().c_str());
  }
}



size_t calc::ExternalSymbols::nrLibrariesLoaded() const
{
  return d_libraries.size();
}



//! Returns the number of model link proxies created.
/*!
  \return    Number of model link proxies.

  For each model link in the shared libraries read a proxy is created.
*/
size_t calc::ExternalSymbols::nrModelLinkProxies() const
{
  return d_modelLinkProxies.size();
}



size_t calc::ExternalSymbols::nrExternalFunctions() const
{
  return d_table.size();
}



//! Returns the model link proxy at index \a i.
/*!
  \param     i Index in the model link proxy list.
  \return    Model link proxy.
  \warning   i must be a valid index < nrModelLinkProxies().
*/
const calc::ModelLinkProxy&
                   calc::ExternalSymbols::modelLinkProxy(size_t i) const
{
  PRECOND(i < d_modelLinkProxies.size());
  return d_modelLinkProxies[i];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



