#ifndef INCLUDED_DAL_ENVIRONMENT
#define INCLUDED_DAL_ENVIRONMENT



// External headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM
#include <boost/filesystem.hpp>
#define INCLUDED_BOOST_FILESYSTEM
#endif

#ifndef INCLUDED_BOOST_NONCOPYABLE
#include <boost/noncopyable.hpp>
#define INCLUDED_BOOST_NONCOPYABLE
#endif

// Project headers.

// Module headers.



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
class Environment: private boost::noncopyable
{

  friend class EnvironmentTest;

private:

  //! Collection for the contents of $PCRASTER_DAL_FORMATS.
  std::vector<std::string> _formatNames;

  std::string      _gdalData;

  void             readFormatNames     ();

  void             handleGdalData      (boost::filesystem::path const& prefix);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Environment         (boost::filesystem::path const& prefix);

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
