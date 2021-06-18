#ifndef INCLUDED_COM_TEMPDIRECTORY
#define INCLUDED_COM_TEMPDIRECTORY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#include <filesystem>
#include <string>


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
  std::filesystem::path  d_name;

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
  const std::filesystem::path& name          () const;
  std::filesystem::path        memberPath    (const std::string& member) const;

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
