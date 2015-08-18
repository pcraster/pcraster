#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_TEMPDIRECTORY
#include "com_tempdirectory.h"
#define INCLUDED_COM_TEMPDIRECTORY
#endif

// Library headers.
#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif
#ifndef INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#include "boost/filesystem/exception.hpp"
#define INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#endif
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif
// Module headers.
#ifndef INCLUDED_COM_UNIQUESTRINGGENERATOR
#include "com_uniquestringgenerator.h"
#define INCLUDED_COM_UNIQUESTRINGGENERATOR
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h" // tempDirectoryName
#define INCLUDED_COM_PATHINFO
#endif
/*!
  \file
  This file contains the implementation of the TempDirectory class.
*/



//------------------------------------------------------------------------------

/*
namespace com {

class TempDirectoryPrivate
{
public:

  TempDirectoryPrivate()
  {
  }

  ~TempDirectoryPrivate()
  {
  }

};

} // namespace com
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC TEMPDIRECTORY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF TEMPDIRECTORY MEMBERS
//------------------------------------------------------------------------------

namespace com {

//! ctor
/*!
    \throws com::FileError if for some reason creation fails
 */
TempDirectory::TempDirectory(const std::string& prefix)
{
  try {
    d_name=prefix; // tmp setting in case of throwFileError

    boost::filesystem::path tmpDir=tempDirectoryName().path();

    com::UniqueStringGenerator g;
    g.setPrefix(prefix);
    boost::filesystem::path  pname;
    do {
      pname = tmpDir;
      pname /= g.generate();
    } while (boost::filesystem::exists(pname));

    boost::filesystem::create_directory(pname);

    d_name=pname;

  } catch(const boost::filesystem::filesystem_error& e) {
    throwFileError("creating",e.what());
  } catch(const com::Exception& e) {
    throwFileError("creating",e.messages());
  } catch(...) {
    throwFileError("creating","unknown");
  }
}



/* NOT IMPLEMENTED
//! Copy constructor.
TempDirectory::TempDirectory(
         TempDirectory const& rhs)

  : Base(rhs)

{
}
*/



TempDirectory::~TempDirectory()
{
  remove();
}



/* NOT IMPLEMENTED
//! Assignment operator.
TempDirectory& TempDirectory::operator=(
         TempDirectory const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/

//! return the absolute name of this temporary directory
const boost::filesystem::path& TempDirectory::name() const
{
  return d_name;
}

//! create absolute path to member of directory named \a member
/*!
 * Example: if name() is /tmp/pcrcalcSwap1 then fileMember called with
 * fname=piet returns /tmp/pcrcalcSwap1/piet
 */
boost::filesystem::path TempDirectory::memberPath(const std::string& member) const
{
  boost::filesystem::path pn(d_name);
  pn /= member;
  return pn;
}


//! remove entire directory including its contents
/*!
    \throws com::FileError if for some reason removal fails
 */
void TempDirectory::remove()
{
  using namespace boost::filesystem;
  try {
   remove_all(d_name);
  } catch(const filesystem_error& e) {
    throwFileError("removing",
     com::replaceStrByStr(e.what(),"boost::filesystem::remove",""));
  } catch(const com::Exception& e) {
    throwFileError("removing",e.messages());
  } catch(...) {
    throwFileError("removing","unknown");
  }
}

void TempDirectory::throwFileError(
    const char *when,
    const std::string& what) const
{
  std::ostringstream str;
  str << "while " << when << " temporary directory:" << what;
  throw com::FileError(d_name.string(),str.str());
}

} // namespace com

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



