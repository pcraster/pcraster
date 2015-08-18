#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_STDEXCEPT
#include <stdexcept>
#define INCLUDED_STDEXCEPT
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifdef WIN32

  #ifndef INCLUDED_COM_WIN32
  #include "com_win32.h"
  #define INCLUDED_COM_WIN32
  #endif
#ifdef _MSC_VER
  // chdir / getcwd
  #ifndef INCLUDED_DIRECT
  #include <direct.h>
  #define INCLUDED_DIRECT
  #endif
#else
  #ifndef INCLUDED_DIR
  #include <dir.h>
  #define INCLUDED_DIR
  #endif
#endif
  #ifndef INCLUDED_IO
  #include <io.h>         // access
  #define INCLUDED_IO
  #endif

  #define   F_OK 0
  #define   R_OK 4
  #define   W_OK 2
  #define   access  _access
  #define   chdir   _chdir
  #define   getcwd  _getcwd
#else
  #ifndef INCLUDED_UNISTD
  #include <unistd.h>
  #define INCLUDED_UNISTD
  #endif
#endif

#ifndef INCLUDED_CSTDIO
#include <cstdio>
#define INCLUDED_CSTDIO
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif


/*!
  \file
  brief

  more elaborated
*/


namespace com {

namespace detail {

const size_t MAX_PATH_LENGTH = 2024;

} // namespace detail

} // namespace com

//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

void com::changeWorkingDirectory(const PathName& newWorkingDirectory)
{
  ::chdir(newWorkingDirectory.toString().c_str());
}

//! Returns the current working directory.
/*!
  \relates   PathInfo
  \return    Current working directory as PathName object.

  This is an absolute path by definition; non-absolute paths, relative
  paths depend on the currentWorkingDirectory() to make it an absolute
  path.
*/
com::PathName com::currentWorkingDirectory()
{
  PRECOND(com::detail::MAX_PATH_LENGTH < 2048);
  char buffer[2048];
  char *currentDir;

  if((currentDir = ::getcwd(buffer, com::detail::MAX_PATH_LENGTH)) == 0)
    throw std::logic_error(std::string(
                   "unable to determine current working directory"));
  PathName pn(currentDir);
  return pn;
}

//! Try to find the directory of executable
/*!
  \pre On linux (not win32 FTTB) \a argv0 must be set to an absolute path.
       Wether argv[0] of main() is absolute depends on how the app is called
       in the shell.


  \param     argv0  argv[0] argument of main() or empty string
  \relates   PathInfo
  \return    directory of executable
  \warning   DO NOT USE in production code, simply does ONLY work
             under win32, must replace this call.
*/
com::PathName com::directoryOfExecutable(const std::string&
#ifndef WIN32
    argv0
#endif
    )
{
#ifdef WIN32
    com::PathName execDir(win32GetModuleFileName());
#else
    // does not work, do something that look useful
    com::PathName execDir(argv0);
    PRECOND(execDir.isAbsolute());
#endif
    execDir.up();
    return execDir;
}



//! directory name were temporary files ought to be stored.
/*!
   The directory name returned is the so called "temporary directory". The
   current directory is returned if no other directory is found. The function
   does not test if the process has permission to read and/or write in that
   directory.
  \relates   PathInfo
  \return    Name of a directory.
*/
com::PathName com::tempDirectoryName()
{
  // BEGIN OLD CODE
  // Do not use mkstemp as gcc wants us to. mkstemp also creates the file and
  // here we are only interested in the name of a directory.
  // PathName tempDirName = std::tmpnam(NULL);
  // if(!tempDirName.isEmpty()) {
  //  tempDirName.up();
  // }
  // END OLD CODE
  // note gcc also has mkdtemp
  PathName tempDirName;
#ifdef WIN32
  char buf[4096];
  DWORD r = GetTempPath(4096,buf);
  if (r>0 && r <= 4095)
    tempDirName=buf;
#else
  char *r = getenv("TMP");
  if (r)
    tempDirName = r;
#endif
  if (!PathInfo(tempDirName).isDirectory())
     tempDirName=""; // make it set to current working directory

  if(tempDirName.isEmpty()) {
    tempDirName = currentWorkingDirectory();
  }

  POSTCOND(!tempDirName.isEmpty());

  return tempDirName;
}



/*!
  \relates   PathInfo
  \brief     Returns true if the path with name \a pn points to an existing
             file.
  \param     pn Path name.
  \return    True if \a pn points to an existing file.
*/
bool com::pathExists(const std::string &pn)
{
  if (pn == ".") // current directory exists by definition
        return true;
  return pn.empty() ? false : (::access(pn.c_str(), F_OK) == 0);
}



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS
//------------------------------------------------------------------------------

/*!
  \brief     Constructor.

  Constructs an empty PathInfo object. Use the setPathName(const PathName &)
  member to set the path name of the file to obtain information about.
*/
com::PathInfo::PathInfo()
{
}



/*!
  \brief     Constructor.
  \param     pn Path name to use.
*/
com::PathInfo::PathInfo(const PathName &pn)

  : d_pathName(pn)

{
}



/*!
  \brief     Copy constructor.
  \param     pi Path information object to copy.
*/
com::PathInfo::PathInfo(const PathInfo &pi)

  : d_pathName(pi.d_pathName)

{
}



/*!
  \brief     Destructor.
*/
com::PathInfo::~PathInfo()
{
}



/*!
  \brief     Assignment operator.
  \param     pi Path information to assign from.
  \return    Resulting path information object.
*/
com::PathInfo &com::PathInfo::operator=(const PathInfo &pi)
{
  if(&pi != this)
  {
    d_pathName = pi.d_pathName;
  }

  return *this;
}



/*!
  \brief     Sets the path name of the file to obtain information about to \a
             pn.
*/
void com::PathInfo::setPathName(const PathName &pn)
{
  d_pathName = pn;
}



/*!
  \brief     Returns the layered path name object.
  \return    Path name object.
*/
const com::PathName &com::PathInfo::pathName() const
{
  return d_pathName;
}



/*!
  \brief     Return true if the layered path name points to an existing file.
  \return    True if the path name exists.
  \sa        pathExists
*/
bool com::PathInfo::exists() const
{
  return pathExists(d_pathName.toString());
}



/*!
  \brief     Returns true if the layered path name points to an existing
             directory.
  \return    True if the path name is a directory.
*/
bool com::PathInfo::isDirectory() const
{
  return exists() && boost::filesystem::is_directory(d_pathName.path());
}



/*! Returns true if the layered path name points to an existing
    (regular) file, false otherwise
*/
bool com::PathInfo::isFile() const
{
  if (!exists())
    return false;
#ifndef WIN32
  struct stat s;
  if (stat(d_pathName.toString().c_str(), &s))
    throw std::runtime_error(d_pathName.toString() + ": " + strerror(errno));
  return S_ISREG(s.st_mode) || S_ISLNK(s.st_mode);
#else
  // MISCHIEN, WEET NIET TEST CASE MAKEN
  // DWORD attrs = GetFileAttributes(d_pathName.toString().c_str());
  // of _stat van MSC
  // FTTB: 
  return !isDirectory();
#endif
}

/*! Returns true if the layered path name points to an existing
    (regular) file and process has read permission, false otherwise
*/
bool com::PathInfo::isReadable() const
{
  return  exists() && (::access(d_pathName.toString().c_str(), R_OK) == 0);
}

/*! Returns true if the layered path name points to an existing
    (regular) file and process has write permission, false otherwise
*/
bool com::PathInfo::isWritable() const
{
  return  exists() && (::access(d_pathName.toString().c_str(), W_OK) == 0);
}


//! helper function
static void throwOpenError(
        const com::PathName& p,
        com::Errno no)
{
    throw com::OpenFileError(p.toString(),no);
}

//! Test if the layered path name is a valid name on platforms
/*!
  We do forbid names that are invalid on either win32 or unix.
  \exception com::OpenFileError Name not valid
  \todo each part (every directory part of) of path name must be tested, and all
  basenames, con and aux are forbidden with all extensions.
*/

void com::PathInfo::testValidName() const
{
   std::string base(d_pathName.baseName());
   const char *names[2] = { "aux", "con" };
   for (size_t i=0; i < 2; i++)
    if (base == names[i])
       throw com::OpenFileError(d_pathName.toString(),"Not a valid filename");
}

//! test if the layered path name is an existing regular file that can be read
/*!
   If no exceptions are thrown the file is garantueed to be a file ready
    for opening in read only mode
  \exception com::OpenFileError File could not be opened for reading. Message is one  of:
      - No such file or directory (com::E_NOENT)
      - Is a directory (com::E_ISDIR)
      - Is not a (regular) file (com::E_NOTREGFILE)
      - Permission denied for reading (com::E_ACCESREAD)
      - Not a valid file Name

  \bug
    It does not throw if on win32 if other process already opened 
    the file for writing. (see pcrxml::Document::Document())

  \todo
    fix above bug and test is with spawning seperate process?
    in unit-test or is a stream opend twice also ok, check if
    difference with in- and out-of-process re-opening.
*/
void com::PathInfo::testOpenForReading() const
{
  PRECOND(!d_pathName.toString().empty());
  testValidName();

  if (!exists())
       throwOpenError(d_pathName,E_NOENT);
  if(isDirectory())
       throwOpenError(d_pathName,E_ISDIR);
  if(!isFile())
       throwOpenError(d_pathName,E_NOTREGFILE);
  if(!isReadable())
       throwOpenError(d_pathName,E_ACCESREAD);
}

//! test if the layered path name can be written
/*!
   If no exceptions are thrown the file is garantueed to be a file ready
    for creating or appending to in write mode
  \exception com::OpenFileError Message is one  of:
      - Is a directory (com::E_ISDIR)
      - Permission denied for writing (com::E_ACCESWRITE)
      - Permission denied for creating (no write access parent directory) (com::E_ACCESCREATE)
      - if it is not a valid fileName: Directory/Folder part of file does not exist (com::E_DIRPARTNOENT)
*/
void com::PathInfo::testOpenForWriting() const
{
  PRECOND(!d_pathName.toString().empty());
  testValidName();

  if(isDirectory()) {
       throwOpenError(d_pathName,E_ISDIR);
  }

  if(exists()) {
       if(!isWritable()) // not overwritable
         throwOpenError(d_pathName,E_ACCESWRITE);
  }
  else {
    PathInfo di;

    // does not exists
    // check if directory is writable
    // so we can add a file
    if(!d_pathName.directoryName().empty()) {
      di = PathInfo(d_pathName.directoryName());
    }
    else {
      // Default directory is current directory.
      di = PathInfo(currentWorkingDirectory());
    }

    if (!di.exists())
      throwOpenError(d_pathName,E_DIRPARTNOENT);
    if (!di.isWritable())
      throwOpenError(d_pathName,E_ACCESCREATE);
  }
}

//! verifies if it is readable and the name is in correct casing
/*!
 * This is important on win32 where the file system is not case
 * sensitive, but the filenames are stored in a certain case.
 * this function check if the specified name is identical to the
 * name stored.
 * First a testOpenForReading() is done, then the case check is
 * done for win32
 * \todo
 *   MINOR: the set of skipComponents can be initialize statically
 */
void  com::PathInfo::testCaseSensitiveName() const
{
  testOpenForReading();
# ifdef WIN32
  /* it is already known to be a valid existing
   * filename, now check if each component (directory
   * or file name, of the path is in correct casing.
   * The win32 call FindFirstFile only returns the in
   * cFileName, the name of the file, with the directory
   * components stripped.
   * What we do is comparing the last component for all components
   * loop1: path=a\b\c\d.map comp=d.map
   * loop2: path=a\b\c       comp=c   (etc)
   */

  std::set<std::string> skipComponent;
  skipComponent.insert("..");
  skipComponent.insert(".");
  skipComponent.insert("\\");

  PathName pn(d_pathName);
  while(pn.hasBaseName()) {
    std::string base=pn.baseName();
    if (skipComponent.count(base) == 0) {
      WIN32_FIND_DATA fInfo;
      HANDLE h = FindFirstFile(pn.toString().c_str(),&fInfo);
      if (h == INVALID_HANDLE_VALUE)
        throw com::OpenFileError(d_pathName.toString(),win32GetLastError());
      FindClose(h);

      if (base != fInfo.cFileName) {
          std::string msg = " has mixed case on disk: ";
          msg += fInfo.cFileName;
          throw com::OpenFileError(d_pathName.toString(),msg);
      }
    }
    pn.up();
  }
# endif
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


