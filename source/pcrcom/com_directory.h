#ifndef INCLUDED_COM_DIRECTORY
#define INCLUDED_COM_DIRECTORY

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif

namespace com {

//! The Directory class models a directory (Deprecated).
/*!
 * Deprecated, try boost::filesystem instead
 *
  \sa PathName, PathInfo

*/
class Directory
{

private:

  //! Path name of the directory.
  PathName         d_path;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Directory           ();

                   Directory           (const PathName &pn);

                   Directory           (const Directory &d);

  /* virtual */    ~Directory          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Directory &      operator=           (const Directory &d);

  void             setPathName         (const PathName &pn);

  void             create              (bool makeParentDirectories = false);

  void             create              (const PathName &pn,
                                        bool makeParentDirectories = false);

  void             erase               (bool recurse = false);

  void             erase               (const PathName& pn,
                                        bool recurse = false);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  const PathName&  pathName            () const;

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

void               createDirectory     (const PathName &pn,
                                        bool makeParentDirectories = false);
void               createNewDirectory   (const PathName &pn);



} // namespace com

#endif
