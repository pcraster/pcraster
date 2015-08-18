#ifndef INCLUDED_AG_AGUILAPROGRAMOPTIONS
#define INCLUDED_AG_AGUILAPROGRAMOPTIONS



// Library headers.
#include <string>
#include <vector>
#include <boost/noncopyable.hpp>

// PCRaster library headers.

// Module headers.



namespace pcrxml {
  class Aguila;
  class StringSet;
}

namespace ag {



//! Get program options from argv style.
/*!
    All needed for configuring Aguila are set in the base pcrxml::Aguila object
    others are added local.
*/
class AguilaProgramOptions: private boost::noncopyable
{

  friend class AguilaProgramOptionsTest;

private:

  // Configuration option values provided by the user.
  bool             d_license;

  bool             d_version;

  //! The string to show as help if help option is obtained, empty otherwise.
  std::string      d_help;

  std::string      d_lockFileName;

  std::string      d_configFileName;

  pcrxml::Aguila*  d_configuration;

  void             obtainProgramOptions(int    argc,
                                        char **argv);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   AguilaProgramOptions(int    argc,
                                        char **argv);

  /* virtual */    ~AguilaProgramOptions();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool             hasMultiView        () const;

  bool             license             () const;

  bool             version             () const;

  std::string const& help              () const;

  std::string const& lockFileName      () const;

  pcrxml::Aguila const& configuration  () const;

  static std::vector<std::vector<std::string> > viewPlusSyntaxToViewCtor(
                                  std::vector<std::string> const& viewValues);

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



} // namespace ag

#endif
