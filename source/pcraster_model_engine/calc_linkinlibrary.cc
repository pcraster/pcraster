#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_LINKINLIBRARY
#include "calc_linkinlibrary.h"
#define INCLUDED_CALC_LINKINLIBRARY
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif
// PCRaster library headers.

#ifndef INCLUDED_PCRLINKIN
#include "pcrlinkin.h"
#define INCLUDED_PCRLINKIN
#endif

#ifndef INCLUDED_COM_DYNAMICLIBRARY
#include "com_dynamiclibrary.h"
#define INCLUDED_COM_DYNAMICLIBRARY
#endif
#ifdef WIN32
#ifndef INCLUDED_COM_WIN32
#include "com_win32.h"
#define INCLUDED_COM_WIN32
#endif
#include <windows.h>
#include <excpt.h>
#endif
#ifndef INCLUDED_PCRASTERXSD
#include "PCRasterXSD.h"
#define INCLUDED_PCRASTERXSD
#endif
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif

// Module headers.
#ifndef INCLUDED_CALC_XMLDATATYPE
#include "calc_xmldatatype.h"
#define INCLUDED_CALC_XMLDATATYPE
#endif

/*!
  \file
  This file contains the implementation of the LinkInLibrary class.
*/



namespace calc {


//------------------------------------------------------------------------------

class LinkInLibraryPrivate
{
#ifndef WIN32
#define LINKIN_CALL
#define TRY_LINKIN_CALL
#define EXCEPT_LINKIN_CALL
#else
  // NOTE that WIN32 exception handler
  // can only be done in a context where
  // obects needs unwinding.
  // hence we always do in a wrapper
  void throwExcep(DWORD e) const {
   std::ostringstream errorStream;
   errorStream << "ERROR System exception:\n "
               << com::win32ExceptionString(e)
               << std::endl;
   throw com::Exception(errorStream.str());
  }
#define LINKIN_CALL        __stdcall
#define TRY_LINKIN_CALL    __try
#define EXCEPT_LINKIN_CALL \
   __except(EXCEPTION_EXECUTE_HANDLER) { \
        throwExcep(GetExceptionCode());   \
  }
#endif

  typedef const char* (LINKIN_CALL *ExecuteFun)(const char *xml,
                                    LinkInTransferArray linkInTransferArray);

  typedef void (LINKIN_CALL *RunContextFun)(
                                int    nrRows,
                                int    nrCols,
                                double cellSize,
                                double xLowerLeftCorner,
                                double yLowerLeftCorner,
                                int    currentTimeStep);

  typedef const char* (LINKIN_CALL *CheckFun)(const char *xml);

  ExecuteFun                                    d_execute;
  RunContextFun                                 d_runContext;
  CheckFun                                      d_check;

  std::string                                   d_name;
  com::DynamicLibrary*                          d_dl;
  std::auto_ptr<pcrxml::LinkInLibraryManifest>  d_manifest;

  void throwException(std::string const& msg)
  {
    std::ostringstream s;
    s << "'" << d_name << "': " << msg;
    throw com::Exception(s.str());
  }



  //! wrapper
  void runContext(
      int    nrRows,
      int    nrCols,
      double cellSize,
      double xLowerLeftCorner,
      double yLowerLeftCorner,
      int    currentTimeStep) const
  {
    TRY_LINKIN_CALL {
    d_runContext(
      nrRows,
      nrCols,
      cellSize,
      xLowerLeftCorner,
      yLowerLeftCorner,
      currentTimeStep);
    } EXCEPT_LINKIN_CALL
  }

  void runContext(pcrxml::RunContext const& rc) const
  {
    PRECOND(d_runContext);

    PRECOND(rc.areaMap().cellSize().present());
    PRECOND(rc.areaMap().xLowerLeftCorner().present());
    PRECOND(rc.areaMap().yLowerLeftCorner().present());
    // PROBLEM MSC ICE's on fundamentalBaseCast to int
    runContext(
        pcrxsd::fundamentalBaseCast<size_t>((rc.areaMap().nrRows())),
        pcrxsd::fundamentalBaseCast<size_t>((rc.areaMap().nrCols())),
        rc.areaMap().cellSize().get(),
        rc.areaMap().xLowerLeftCorner().get(),
        rc.areaMap().yLowerLeftCorner().get(),
        pcrxsd::fundamentalBaseCast<size_t>((rc.timer().current())));
  }

public:

  LinkInLibraryPrivate(const std::string& name):
    d_name(name),
    d_dl(0)
  {

     try {
       d_dl= new com::DynamicLibrary(d_name);
     } catch(com::DynamicLibraryException const& e) {
       throwException(e.messages());
     }
     // ExecuteFun must be there
#if defined(_WIN32) && !defined(_WIN64)
     // for windows 32 bit only
     d_execute = (ExecuteFun)d_dl->addressAndSetLibPath("_pcr_LinkInExecute@8");
     d_check   = (CheckFun)d_dl->address("_pcr_LinkInCheck@4");
     d_runContext = (RunContextFun)d_dl->address("_pcr_LinkInRunContext@36");
#else
     // _WIN64 and linux
     d_execute =    (ExecuteFun)d_dl->addressAndSetLibPath("pcr_LinkInExecute");
     d_check   =    (CheckFun)d_dl->address("pcr_LinkInCheck");
     d_runContext = (RunContextFun)d_dl->address("pcr_LinkInRunContext");
#endif
     if (!d_execute)
       throwException("code library does not contain pcr_LinkInExecute");

     boost::filesystem::path xmlFile(d_dl->directory());
     xmlFile /= (d_name+".xml");
     pcrxsd::DOMInput d(pcrxsd::DOMInput::CompiledIn);
     d.setValidate(true);
     d.setFile(xmlFile.string());
     try {
      d_manifest.reset(pcrxml::linkInLibraryManifest(*d.document()).release());
     } catch(pcrxsd::Exception const& e) {
       throwException(e.msg());
     }
  }

  ~LinkInLibraryPrivate()
  {
    delete d_dl;
  }

  pcrxml::LinkInLibraryManifest const& manifest() const
  {
    PRECOND(d_manifest.get());
    return *(d_manifest.get());
  }


  //! wrapper
  void execute(const char *xml,
               void ** transferArray) const
  {
    TRY_LINKIN_CALL {
     d_execute(xml,transferArray);
    } EXCEPT_LINKIN_CALL
  }

  void execute(pcrxml::LinkInExecuteInput const& li,
               void ** transferArray) const
  {
    if (d_runContext)
      runContext(li.context());
    std::ostringstream s;
    pcrxml::linkInExecuteInput(s,li,pcrxsd::namespaceInfoMap("PCRaster.xsd"));
    execute(s.str().c_str(),transferArray);
  }

  //! wrapper
  const char* check(const char *xml) const
  {
    TRY_LINKIN_CALL {
     return d_check(xml);
    } EXCEPT_LINKIN_CALL
    return "implementation error"; // should never come here
  }

  pcrxml::LinkInCheckResult check(
    pcrxml::LinkInCheckInput const& in) const
  {
    if (d_check) {
      std::ostringstream inS;
      pcrxml::linkInCheckInput(inS,in,pcrxsd::namespaceInfoMap("PCRaster.xsd"));
      const char *outCstr=check(inS.str().c_str());

      std::istringstream outS(outCstr);
#ifdef DEBUG_DEVELOP
      return *pcrxml::linkInCheckResult(outS);
#else
      ::xsd::cxx::tree::flags f = xml_schema::flags::dont_validate;
      return *pcrxml::linkInCheckResult(outS, f);
#endif
    }
    // no check do the default thing
    pcrxml::LinkInCheckResult out;
    out.result(in.result());
    out.argument(in.argument());
    return out;
  }

};



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LINKINLIBRARY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LINKINLIBRARY MEMBERS
//------------------------------------------------------------------------------

LinkInLibrary::LinkInLibrary(const std::string& name):
  d_data(new LinkInLibraryPrivate(name))
{
}



/* NOT IMPLEMENTED
//! Copy constructor.
LinkInLibrary::LinkInLibrary(
         LinkInLibrary const& rhs)

  : Base(rhs)

{
}
*/



LinkInLibrary::~LinkInLibrary()
{
  delete d_data;
}



/* NOT IMPLEMENTED
//! Assignment operator.
LinkInLibrary& LinkInLibrary::operator=(
         LinkInLibrary const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/


pcrxml::LinkInLibraryManifest const& LinkInLibrary::manifest() const
{
  return d_data->manifest();
}

void LinkInLibrary::execute(
    pcrxml::LinkInExecuteInput const& li,
    void ** transferArray) const
{
 d_data->execute(li,transferArray);
}

pcrxml::LinkInCheckResult LinkInLibrary::check(
    pcrxml::LinkInCheckInput const& in) const
{
  return d_data->check(in);
}

/*
void LinkInLibrary::areaMap(geo::RasterSpace const& rs,
                            size_t currentTimeStep) const
{
  d_data->areaMap(rs,currentTimeStep);
}
*/


//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace calc
