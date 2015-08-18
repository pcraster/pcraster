#ifndef INCLUDED_COM_PATHINFO
#define INCLUDED_COM_PATHINFO



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_SYS_STAT
#include <sys/stat.h>
#define INCLUDED_SYS_STAT
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif


namespace com {

void changeWorkingDirectory(const PathName& newWorkingDirectory);

PathName currentWorkingDirectory();

PathName directoryOfExecutable(const std::string& argv0="");

PathName tempDirectoryName ();

bool        pathExists          (const std::string &pn);

//!  The PathInfo class provides system-independent file information.
/*!
  \sa PathName, Directory

  \todo Put case sensitive Windows Pathname code of pcrcalc (calc::File::testCaseOfName) here

  A path can point to a regular file, a directory or a link.
  Use this class if you want information about the object (file, directory, etc.) the path refers to.
  Use the PathName class if you want to manipulate the name of the path.

  This class is modelled after Qt's QFileInfo class. The naming convention are
  taken from Python's os.path module.
*/
//       1         2         3         4         5         6         7         8
class PathInfo
{

private:

  //! The name of the path.
  PathName         d_pathName;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PathInfo            ();

                   PathInfo            (const PathName &pn);

                   PathInfo            (const PathInfo &pi);

  /* virtual */    ~PathInfo           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  PathInfo &       operator=           (const PathInfo &pi);

  void             setPathName         (const PathName &pn);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const PathName & pathName            () const;

  bool             exists              () const;

  bool             isDirectory         () const;
  bool             isFile              () const;
  bool             isReadable          () const;
  bool             isWritable          () const;

  void             testValidName        () const;
  void             testOpenForReading   () const;
  void             testOpenForWriting   () const;
  void             testCaseSensitiveName() const;
};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
