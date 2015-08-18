#ifndef INCLUDED_PATHNAME
#define INCLUDED_PATHNAME



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_IOSFWD
#include <iosfwd>
#define INCLUDED_IOSFWD
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include <boost/filesystem/path.hpp>
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif



namespace com {

extern const char DIR_PATH_DELIM;

#ifdef WIN32
extern const char DRIVE_DELIM;
#endif



//! The PathName class is for objects which represent the name of a path.
/*!
   CVS 2.0 and up is based on boost::filesystem::path, prior (1.X is own code).

  \sa PathInfo, Directory

  A path points to an entity in a file system (eg: a regular file, a directory, a link,
  disk, etc.).

  This class has functionality for manipulating the name of a path. Wether the
  file actually exists or not is not important. Think of a PathName object as
  a <b>smart kind of string</b> with knowledge of the path name conventions of the
  OS platform and functionality to translate between platforms (makeNative()).
  A PathName does not have to point to an existing entity.

  When a PathName is constructed, no changes are made. The toString() method will return
  the identical string passed to the constructor.<br>
  A path name to similar entities can be expressed in more than one way. For example, a
  relative versus absolute path name, Windows delimiters versus Unix
  delimiters, extraneous delimiters.<br>
  To garantuee identical PathName objects, who are all either absolute or
  relative, to the same entity one has to call normalize() on the PathName
  objects.

  The following concepts are important:
  <dl>
  <dt>path</dt>
   <dd>The full path: location + base</dd>
  <dt>location</dt>
   <dd>The location of the file system and a directory. On Windows the location
       of the file system is the drive name.
       This can be empty</dd>
  <dt>base</dt>
   <dd>The last part of the path: the entity residing at location with name base.
       This can be empty, iff directory is also empty.</dd>
  </dl>

  These definitions imply that if we have a path name /usr/bin, then baseName()
  is bin, even if bin is a directory.

  Additionally there's the notion of <b>location</b>. The location is the path to
  the base name, either relative or absolute. The parts location consists of
  depends on OS. On MSWindows location contains at least the drive name or the
  location. Using the directory for this is not portable.

  An empty path (empty string) is a legal constructor

  Above naming of methods and functions are modelled after os.path of the
  python base library (mas o menos).

  For a complete database of examples see the unit tests in com::PathNameTest.

 \bug Win32-UNC paths not supported
   C:\Documents and Settings\Cees>cd \\HOMEP4
    '\\HOMEP4'
    CMD does not support UNC paths as current directories.
    see unc.txt

  PORTING TO BOOST NOTES:
   - Drive FTTB as Root also on non win32
*/
class PathName
{

public:
  //! as string
  static const std::string& dirPathDelimNative();

  static int  compare( const std::string& pathFragment1,
                      const std::string& pathFragment2);
  static bool equal( const std::string& pathFragment1,
                      const std::string& pathFragment2);

private:
  typedef boost::filesystem::path  Path;
  Path                             d_path;

  void set(const std::string& path);

public:

  //! Strict weak ordering.
  struct LessThen {
    bool operator()(const PathName& lhs, const PathName& rhs) const
    {
      return lhs.compare(rhs) < 0;
    }
  };

  //! EqualityComparable.
  struct Equals {
    bool operator()(const PathName& lhs, const PathName& rhs) const
    {
      return lhs.compare(rhs) == 0;
    }
  };

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PathName            ();

                   PathName            (const char* pn);

                   PathName            (const std::string& pn);

                   PathName            (boost::filesystem::path const& path);

                   PathName            (const PathName& pn);

  /* virtual */    ~PathName           ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  PathName&        operator=           (const PathName& pn);

  PathName&        operator+=          (const PathName& addAtEnd);

  friend PathName  operator+           (const PathName& firstPart,
                                        const PathName& secondPart);

  void             clear               ();

  void             join                (const PathName& addAtEnd);

  void             up                  ();

  void             makeNative          ();

  void             makeAbsolute        ();

  void             addExtension        (const std::string& e);

  void             setExtension        (const std::string& e);

  void             removeExtension     ();


  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const Path&      path                () const;

  bool             equals              (const PathName& pn) const;

  int              compare             (const PathName& pathName) const;

  bool             startsWith          (const std::string& aString) const;

  std::string      toString            () const;

  bool             hasDirectoryName    () const;

  bool             hasBaseName         () const;

  std::string      directoryName       () const;

  std::string      baseName            () const;

  std::string      extension           () const;

  size_t           length              () const;

  bool             isAbsolute          () const;

  bool             isRelative          () const;

  bool             isEmpty             () const;

  bool             empty               () const;

  bool             hasExtension        () const;

  friend std::ostream& operator<<      (std::ostream& stream,
                                        const PathName& pathName);

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (const PathName &rhs,
                                        const PathName &lhs);

bool               operator!=          (const PathName &rhs,
                                        const PathName &lhs);

bool               operator<           (const PathName &rhs,
                                        const PathName &lhs);

PathName           operator+           (const PathName &lhs,
                                        const PathName &rhs);

std::ostream&      operator<<          (std::ostream& stream,
                                        const PathName& pathName);

PathName           nativePathName      (const std::string& relativePathName);


//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
