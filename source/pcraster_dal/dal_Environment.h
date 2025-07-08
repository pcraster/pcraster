#ifndef INCLUDED_DAL_ENVIRONMENT
#define INCLUDED_DAL_ENVIRONMENT



// External headers.

// Project headers.

// Module headers.

#include <filesystem>
#include <string>
#include <vector>


namespace dal {
  // Environment declarations.
}



namespace dal {

//! Class for storing settings found in the runtime environment.
/*!
  Upon construction Environment objects query the environment for relevant
  settings. Currently these variable are read:
  - PCRASTER_DAL_FORMATS: List of names of formats to use, instead of the full
    set.
  - GDAL_DATA: Path to directory with GDAL support files directory.
*/
class Environment
{

  friend class EnvironmentTest;

private:

  //! Collection for the contents of $PCRASTER_DAL_FORMATS.
  std::vector<std::string> _formatNames;

  std::string      _gdalData;

  void             readFormatNames     ();

  void             handleGdalData      (std::filesystem::path const& prefix);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Environment         (std::filesystem::path const& prefix);

                   Environment         (Environment const& other) = delete;

  Environment&     operator=           (Environment const& other) = delete;

  /* virtual */    ~Environment        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  std::vector<std::string> const& formatNames() const;

  std::string const& gdalData          () const;

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

} // namespace dal

#endif
