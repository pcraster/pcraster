#ifndef INCLUDED_COM_TEMPDIRECTORY
#define INCLUDED_COM_TEMPDIRECTORY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
#ifndef INCLUDED_BOOST_FILESYSTEM_PATH
#include "boost/filesystem/path.hpp"
#define INCLUDED_BOOST_FILESYSTEM_PATH
#endif

// PCRaster library headers.

// Module headers.



namespace com {
  // TempDirectory declarations.
}



namespace com {



//! A temporary directory created and deleted in the scope of this object lifetime
/*!
   Create a directory with a certain prefix and remove it completely later
*/
class TempDirectory
{

  friend class TempDirectoryTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TempDirectory&           operator=           (TempDirectory const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   TempDirectory               (TempDirectory const& rhs);

  //! full name
  /*!  - basename/leaf starts with ctor prefix arg
   *   - directorypart is from PathInfo::tempDirectoryName()
   */
  boost::filesystem::path  d_name;

  void throwFileError(const char *when, const std::string& what) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   TempDirectory               (const std::string& prefix);

  /* virtual */    ~TempDirectory              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void              remove                     ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const boost::filesystem::path& name          () const;
  boost::filesystem::path        memberPath    (const std::string& member) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace com

#endif
