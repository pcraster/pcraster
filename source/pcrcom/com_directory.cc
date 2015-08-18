#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_DIRECTORY
#include "com_directory.h"
#define INCLUDED_COM_DIRECTORY
#endif

#ifndef INCLUDED_CSTDIO
#include <cstdio>
#define INCLUDED_CSTDIO
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include "boost/filesystem/operations.hpp"
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif


#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif

/*!
  \file
  brief

  more elaborated
*/


//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

/*!
  \brief     Constructor.
  \warning   No check is made to ensure that the directory exists.

  Creates an empty directory object.
*/
com::Directory::Directory()
{
}



/*!
  \brief     Constructor.
  \param     pn Path name of the directory this objects points to.
  \warning   No check is made to ensure that the directory exists.
*/
com::Directory::Directory(const PathName &pn)

  : d_path(pn)

{
}



//! Copy constructor.
/*!
  \param     d Directory object to copy from.
*/
com::Directory::Directory(const Directory& d)

  : d_path(d.d_path)

{
}



/*!
  \brief     Destructor.
*/
com::Directory::~Directory()
{
}



//! Assignment operator.
/*!
  \param     d Directory object to copy from.
  \return    Reference to *this.
*/
com::Directory& com::Directory::operator=(const Directory& d)
{
  if(this != &d) {
    d_path = d.d_path;
  }

  return *this;
}



/*!
  \brief     Sets the path name of the directory to \a pn.
  \param     pn Path name of the directory.
*/
void com::Directory::setPathName(const PathName &pn)
{
  d_path = pn;
}

/*!
  \brief     Creates the layered directory path.
  \param     makeParentDirectories If true, all parent directories will be
             created too.
  \exception com::OpenFileError If it already exists but is not a directory or
             if it or part of it can not be created.
  \warning   The result is undefined if no path name is set.
  \sa        create(const PathName &, bool), erase(bool)

  \code
    Directory dir("/usr/local");

    // Creates local in /usr.
    dir.create(false);

    // Creates /usr/local.
    dir.create(true);
  \endcode
*/
void com::Directory::create(bool makeParentDirectories)
{
  PRECOND(!d_path.toString().empty());

  if(makeParentDirectories)
  {
    std::vector<std::string> dirs;
    PathName pn(d_path);
    std::string dirPart(pn.directoryName());

    while (dirPart != "")
    {
      dirs.push_back(pn.baseName());
      pn = dirPart;
      dirPart = pn.directoryName();
    }

    dirs.push_back(pn.baseName());

    size_t i  = dirs.size();

    pn = PathName("");
    while (i > 0)
    {
      i--;
      pn += dirs[i];
      try {
        Directory dir(pn);
        dir.create(false);
      }
      catch (const OpenFileError& er) {
        std::string msg("while creating " + d_path.toString() + ":");
        throw OpenFileError(er.fileName(),msg+er.diagnosis());
      }
    }
  }
  else
  {
    PathInfo pi(d_path);
    if(pi.isDirectory())
      return; // already exists as an directory, done
    try {
      boost::filesystem::create_directory(d_path.path());
    } catch(...) {
       throw OpenFileErrnoMsg(d_path, "can not create directory");
    }
  }
}



/*!
  \brief     Creates a sub directory.
  \param     pn Path name of the sub directory to create.
  \param     makeParentDirectories If true, all parent directories will be
             created too.
  \exception com::OpenFileError If it already exists but is not a directory or
             if it or part of it can not be created.
  \warning   The result is undefined if no path name is set.
  \sa        create(bool), erase(bool)

  \a pn is the name of a sub directory relative to the layered path name.

  \code
    Directory dir("/usr/local");

    // Creates bin in /usr/local.
    dir.create("bin", false);

    // Creates /usr/local/bin.
    dir.create("bin", true);
  \endcode
*/
void com::Directory::create(const PathName &pn, bool makeParentDirectories)
{
  PRECOND(!d_path.toString().empty());

  Directory dir(d_path + pn);
  dir.create(makeParentDirectories); 
}



//! Erases the layered directory path.
/*!
  \param     recurse If true, then recursively delete all subdirectories.
  \exception com::FileError If the directory path could not be deleted.
  \sa        erase(const PathName&, bool), create(bool),
             create(const PathName&, bool)
  \todo
    seems pcrcom uses both erase and remove as names for identical functions
    fix that, use remove.
*/
void com::Directory::erase(bool recurse)
{
  try {
    if (recurse)
      boost::filesystem::remove_all(d_path.path());
    else {
      // Erase files and empty directories.
      boost::filesystem::remove(d_path.path());
    }
  } catch (...) {
      throw FileErrnoMsg(d_path, "can not delete directory: ");
  }
}



//! Erases the sub-directory \a pn from the layered one.
/*!
  \param     pn Sub-directory name.
  \param     recurse If true, then recursively delete all subdirectories.
  \exception com::FileError If \a pn could not be deleted.
  \warning   \a pn must be a normalized path name.
  \sa        erase(bool), create(bool), create(const PathName&, bool)
*/
void com::Directory::erase(const PathName& pn, bool recurse)
{
  PathName npn = d_path + pn;          // 'Normalized' path name.
  Directory subDir(npn);
  subDir.erase(recurse);
}


//! Return the PathName of the Directory.
/*!
  \return    PathName of the Directory.
*/
const com::PathName& com::Directory::pathName() const
{
  return d_path;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------

//! Creates a directory.
/*!
 * If \a pn does not exist as an directory then create it.
  \relates   Directory, createNewDirectory
  \param     pn Path name of the directory to create.
  \param     makeParentDirectories If true, all parent directories will be
             created too.
  \exception com::OpenFileError If it already exists but is not a directory or
             if it or part of it can not be created.

  If \a pn is not an absolute path name, then it is taken to be relative to
  the current working directory.

  \code
    // Creates bin in /usr/local.
    createDirectory("/usr/local/bin", false);

    // Creates /usr/local/bin.
    createDirectory("/usr/local/bin", true);

    // Creates bin in /home/bouke/local (eg).
    createDirectory("local/bin", false);

    // Creates /home/bouke/local/bin (eg).
    createDirectory("local/bin", true);
  \endcode
*/
void com::createDirectory(const PathName &pn, bool makeParentDirectories)
{
  Directory dir;

  if(pn.isRelative())
    dir.setPathName(currentWorkingDirectory() + pn);
  else
    dir.setPathName(pn);

  dir.create(makeParentDirectories);
}

//! Creates a not yet existant directory.
/*! 
 *  \param pn path to a directory to create, directoryName part of path must
 *            be existant.
 *   \exception com::OpenFileError If it already exists as file or directory or
 *           if it or part of it can not be created.
 */
void com::createNewDirectory(const PathName &pn)
{
  Directory dir;

  if(PathInfo(pn).exists())
    throw OpenFileError(pn.toString(),E_EXIST);
  createDirectory(pn, false);
}

//------------------------------------------------------------------------------
// DOCUMENTATION OF ENUMERATIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DOCUMENTATION OF PURE VIRTUAL FUNCTIONS
//------------------------------------------------------------------------------


