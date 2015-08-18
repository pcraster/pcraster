#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"              // currentWorkingDirectory()
#define INCLUDED_COM_PATHINFO
#endif

#ifndef INCLUDED_COM_STRLIB
#include "com_strlib.h"
#define INCLUDED_COM_STRLIB
#endif



/*!
  \file
  brief

  more elaborated
*/


namespace com {

const char DIR_PATH_DELIM=DIR_PATH_DELIM_CHAR;

#ifdef WIN32
const char DRIVE_DELIM=':';
#endif

} // namespace com



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASS MEMBERS
//------------------------------------------------------------------------------

const std::string& com::PathName::dirPathDelimNative()
{
  static std::string str(1,DIR_PATH_DELIM);
  return str;
}

//! test if two path fragments are equal
bool com::PathName::equal(
                      const std::string& pathFragment1,
                      const std::string& pathFragment2)
{
 return com::PathName::compare(pathFragment1,pathFragment2) == 0;
}

//! compare two path fragments
/*!
  \return    -1,0,1 as in std::string::compare()
*/
int com::PathName::compare(
    const std::string& pathFragment1,
    const std::string& pathFragment2)
{
#ifdef WIN32
      return com::compareNoCase(pathFragment1,pathFragment2);
#else
      return pathFragment1.compare(pathFragment2);
#endif
}

//------------------------------------------------------------------------------
// DEFINITION OF CLASS MEMBERS 
//------------------------------------------------------------------------------

//! Constructor.
/*!

  The default constructor creates an empty path name object.
*/
com::PathName::PathName()
{
}



//! Constructor.
/*!
    \param     pn Character string representation of a path name.
*/
com::PathName::PathName(const char *pn)
  : d_path(pn)
{
}



/*!
  \brief     Constructor.
  \param     pn String representation of a path name.
*/
com::PathName::PathName(const std::string &pn)
  : d_path(pn)
{
}



//! Constructor.
/*!
  \param     path Path object to copy.
  \warning   In contrast to the other constructors no name checking is
             performed by this constructor. It is assumed that \a path already
             is valid.
*/
com::PathName::PathName(boost::filesystem::path const& path)

  : d_path(path)

{
}



void com::PathName::set(const std::string& path)
{
  d_path=Path(path);
}



/*!
  \brief     Copy constructor.
  \param     pn Path name object to copy.
*/
com::PathName::PathName(const PathName &pn)
  : d_path(pn.d_path)
{
}



/*!
  \brief     Destructor.
*/
com::PathName::~PathName()
{
}



//! Returns true if the path name has a directory name.
/*!
  \return    true or false
  \sa        hasDriveName(), hasBaseName()
*/
bool com::PathName::hasDirectoryName() const
{
  return path().has_parent_path();
}


//! Returns true if the path name has a base name.
/*!
  \return    true or false
  \sa        hasDriveName(), hasDirectoryName()
*/
bool com::PathName::hasBaseName() const
{
  return path().has_filename();
}



/*!
  \brief     Assignment operator.
  \param     pn Path name to assign from.
  \return    Resulting path name.
*/
com::PathName &com::PathName::operator=(const PathName &pn)
{
  if(&pn != this) {
    d_path = pn.path();
  }

  return *this;
}



//! Concatenates path name \a addAtEnd to this.
/*!
  \param     addAtEnd Path name to concatenate to this.
  \warning   Concatenating an absolute path  may lead to unexpected results

*/
void com::PathName::join(const PathName &addAtEnd)
{
  d_path /= addAtEnd.path();
}

//! reset to empty
void com::PathName::clear()
{
  *this = com::PathName();
}



//! Concatenates \a addAtEnd to the end of this.
/*!
  \param     addAtEnd Path name to concatenate to this.
  \return    Resulting path name.
  \sa        join(const PathName &)

  For more info see join(const PathName &)
*/
com::PathName &com::PathName::operator+=(const PathName &addAtEnd)
{
  join(addAtEnd);

  return *this;
}



//! Converts foreign path names to native ones.
/*!
  For example the unix path 'project/scripts/rain.mod' will be converted to
  the windows path 'project\scripts\rain.mod' if the target platform is WIN32.
  Of course it can not do 'magic' like converting root<->drives,
  like / <-> c:\.

  Call this function only if you expect foreign path names.
*/
void com::PathName::makeNative()
{
 // BUGGED d_path.make_preferred();
#ifdef WIN32
 std::string str = toString();
 std::replace(str.begin(), str.end(),'/','\\');
 d_path = str;
#endif
}



//! Makes the path name an absulute path name.
/*!
  \relates   com::currentWorkingDirectory()

  If the path name is not yet absolute, it will be made absolute by prepending
  the current directory.

*/
void com::PathName::makeAbsolute()
{
  if(!path().is_absolute()) {
    *this = currentWorkingDirectory() + *this;
  }
  POSTCOND(isAbsolute());
}




//! Go one directory up; Removes a file or last directory name from the end of the path name.
/*!
  d_path is replaced by path().parent_path();
  \warning   since boost 1.34  up() behaves different with trailing slashed.
             We are not fixing that, if that results in a bug, use boost::filesystem
             directly and get rid of PathName where the bug occurs.
             For current behavior check the unittests of PathName
*/
void com::PathName::up()
{
  d_path=path().parent_path();
}



//! Returns the layerd Path object.
/*!
  \return    Path object.
*/
const com::PathName::Path& com::PathName::path() const
{
  return d_path;
}



/*!
  \brief     Returns true if \a pn is equal to this.
  \param     pn Path name to test this against.
  \return    True if \a pn equals this.
  \sa        compare(const PathName&), startsWith(const PathName&)

  This function takes into account if case matters. So on MSWindows for
  example, the path names "bla" and "BLA" are equal.
*/
bool com::PathName::equals(const PathName &pn) const
{
  return compare(pn) == 0;
}



//! Compares this with \a pathName.
/*!
  \param     pathName Path name to compare with.
  \return    0 if this equals to \a pathName, a negative number if this is
             smaller than \a pathName and a positive number otherwise.
  \sa        equals(const PathName&), startsWith(const PathName&)

  A path name is smaller than another path name if it is lexicographically
  before it. This function takes into account if case matters. So on MSWindows
  for example, the path names "bla" and "BLA" compare equal.
*/
int com::PathName::compare(const PathName& pathName) const
{
      return compare(toString(), pathName.toString());
}



//! Returns true if this starts with \a aString.
/*!
  \param     aString A string to compare to the beginning of this.
  \return    true or false.
  \sa        equals(const PathName&), compare(const PathName&)

  The comparison is done case insensitive on platforms where path names are
  case insensitive. Otherwise a case sensitive comparison made.
*/
bool com::PathName::startsWith(const std::string& aString) const
{
  std::string pathName = toString();
#ifdef WIN32
  return pathName.length() >= aString.length() && com::compareNoCase(
                   pathName.substr(0, aString.length()), aString) == 0;
#else
  return pathName.length() >= aString.length() &&
                   pathName.substr(0, aString.length()) == aString;
#endif
}



/*!
  \brief     Returns the path name as a string.
  \return    Path name as a string.
*/
std::string com::PathName::toString() const
{
  return path().string();
}

//! Returns the name of the directory as a string. (parent_path)
/*!
  \return    Directory name.
  \sa        baseName()
*/
std::string com::PathName::directoryName() const
{
  return path().parent_path().string();
}

//! Returns the name of the base as a string.
/*!
  \brief     Returns the name of the base as a string.
  \return    Base name.
  \sa        directoryName()

  \warning   since boost 1.34  baseName behaves different. We are not
             fixing that, if that results in a bug, use boost::filesystem
             directly and get rid of PathName where the bug occurs.
             For current behavior check the unittests of PathName
*/
std::string com::PathName::baseName() const
{
  return path().filename().string();
}



//! Returns the extension, without the "." itself.
/*!
  \return    Extension or an empty string if the path name doesn't contain
             an extension.
  \sa        addExtension(const std::string&), hasExtension()
*/
std::string com::PathName::extension() const
{
  std::string filename(path().filename().string());
  size_t i = filename.find_last_of('.');
  // found and not as last char
  if (i != std::string::npos && i < filename.size() - 1)
   return filename.substr(i+1); // +1 do not include "."
  return "";
}

//! Returns true if the path name contains an extension.
/*!
  \return    True if path name contains extension.
  \sa        addExtension(const std::string&), extension(), removeExtension()
*/
bool com::PathName::hasExtension() const
{
  return !extension().empty();
}

//! Sets new extension \a e to the pathname.
/*!
 * overwrite the extension already present or add extension if not yet present
 */
void com::PathName::setExtension(const std::string& e)
{
  removeExtension();
  addExtension(e);
}

namespace boost {
  namespace filesystem {
    int fooCeesBug(void)
    {
           std::cout << "fooCeesBug \n";
           return 0;
    }
  }
}


//! remove the extension, including the ".", if there is an extension
/*!
  \sa   hasExtension(), extension(), removeExtension()
*/
void com::PathName::removeExtension()
{
  std::string ext(extension());
  if (!ext.empty()) {
    std::string s(path().string());
    PRECOND(s.rfind(ext)!=std::string::npos);
    size_t dotPos = s.rfind(ext)-1;
    s.erase(dotPos);
    set(s);
  }
}


//! Adds extension \a e to the pathname.
/*!
  \param     e Extension to add, without the extension delimiter (.).
  \sa        hasExtension(), extension(), removeExtension(), setExtenstion()

  This function will adds extension delimiter. Nothing will be done if \a e is
  empty. The extension is always appended, even when an extension is already
  present. setExtension() will overwrite the last extension present
*/
void com::PathName::addExtension(const std::string& e)
{
  if (e.empty())
    return;
  std::string s(path().string());
  std::string dot;
  if (s.empty() || s[s.size()-1] != '.')
   dot=".";
  s+=dot+e;
  set(s);
}

//! Returns the length of the path name.
/*!
  \return    Length.
*/
size_t com::PathName::length() const
{
  return toString().length();
}



/*!
  \brief     Returns true if the path name is relative.
  \return    True if path name is relative.

  A path name is relative if it does not start with the root path name (e.g. '/'
  under UNIX or a drive letter ('c:' or another one) under Windows).

  An empty path name is considered a relative path name.
*/
bool com::PathName::isRelative() const
{
  return !path().is_absolute();
}



/*!
  \brief     Returns true if the path name is absolute.
  \return    True if path name is absolute.
  \sa        isRelative()
*/
bool com::PathName::isAbsolute() const
{
  return path().is_absolute();
}



//! Returns true if the path name is empty.
/*!
  \return    True if path name is empty.

  A path name is empty if it has no drive name, no directory name and no base
  name.
*/
bool com::PathName::isEmpty() const
{
  return empty();
}



bool com::PathName::empty() const
{
  return path().empty();
}




//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS 
//------------------------------------------------------------------------------

/*!
  \relates   PathName
  \brief     Equality operator.
  \param     lhs Left hand side path name.
  \param     rhs Right hand side path name.
  \return    True if \a lhs is equal to \a rhs.
*/
bool com::operator==(const PathName &lhs, const PathName &rhs)
{
  return lhs.equals(rhs);
}



/*!
  \relates   PathName
  \brief     Inequality operator.
  \param     lhs Left hand side path name.
  \param     rhs Right hand side path name.
  \return    True if \a lhs is not equal to \a rhs.
*/
bool com::operator!=(const PathName &lhs, const PathName &rhs)
{
  return !lhs.equals(rhs);
}



bool com::operator<(const PathName &rhs, const PathName &lhs)
{
  return lhs.compare(rhs) < 0;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS 
//------------------------------------------------------------------------------

//! Concatenates \a secondPart to \a firstPart.
/*!
  \relates   PathName
  \param     firstPart Path name to concatenate to.
  \param     secondPart Path name to concatenate.
  \return    Path name with result.
  \sa        join(const PathName&)
*/
com::PathName com::operator+(const PathName &firstPart, const PathName &secondPart)
{
  PathName pn(firstPart);
  pn.join(secondPart);
  return pn;
}



//! Writes \a pathName to \a stream.
/*!
  \relates   PathName
  \param     pathName Path name to write.
  \param     stream Stream to write on.
  \return    The stream.
  \sa        toString()

  This operator calls toString() on \a pathName and writes the result to
  \a stream.
*/
std::ostream& com::operator<<(std::ostream& stream, const PathName& pathName)
{
  stream << pathName.toString();
  return stream;
}

//! Create a PathName object, from a relative path name
/*!
 * Returns a PathName object that with native slashes, where
 * \a relativePathName can have foreigne slashes
 * This is handy to write portable tests:
 * \code
 *   com::PathInfo pi(nativePathName("test\\file"));
 * \endcode
 * \pre relativePathName must be relative
  \relates   PathName
*/
com::PathName      com::nativePathName      (const std::string& relativePathName)
{
  com::PathName pn(relativePathName);
  PRECOND(pn.isRelative());
  pn.makeNative();
  return pn;
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


