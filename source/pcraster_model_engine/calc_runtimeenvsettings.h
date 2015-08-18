#ifndef INCLUDED_CALC_RUNTIMEENVSETTINGS
#define INCLUDED_CALC_RUNTIMEENVSETTINGS



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
// Module headers.

namespace geo {
  class RasterSpace;
}

namespace pcrxml {
  class Script;
}

namespace calc {

class IOStrategy;
class Timer;
class ASTSymbolTable;
class XMLReflection;


//! Settings used to create a RunTimeEnv object
class RunTimeEnvSettings
{
public:
  typedef enum ExitValueType {
    //! -e option
    LAST_VAL,
    //! -E option
    EXIT_ON_0,
    //! just 0 on succes (default)
    ALWAYS_0
  } ExitValueType;



private:

  IOStrategy*      d_ioStrategy;
  com::PathName    d_externalBindingFile;

  //! print the argument expansion ($) instead of usual action
  bool             d_printShellExpansionOnly;
  //! only check the script on syntax and presence of all input instead of usual action
  bool             d_testScriptRunableOnly;

  bool             d_testCaseTypeOnExistingName;

  //! empty if script is not created from a script file
  /*!
   * empty cases are a command line argument command or ModelBuilder type
   * created scripts.
   */
  com::PathName    d_scriptFile;


  // needed/updated first in execution phase

  //! use C compiler
  bool             d_compile;

  ExitValueType    d_exitValueType;
  size_t           d_seed;

  bool             d_profile;
  bool             d_useDiskStorage;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  RunTimeEnvSettings&           operator=           (const RunTimeEnvSettings& rhs);
                   RunTimeEnvSettings               (const RunTimeEnvSettings& rhs);

                   RunTimeEnvSettings               ();

  /* virtual */    ~RunTimeEnvSettings              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
//void             configureSymbols    (ASTSymbolTable& symbols,
//                                      const XMLReflection& xr);

  std::string      setSettingsFromXML   (pcrxml::Script const& script);

  void             setMVCompression    (bool mvCompression);
  void             setDebugMVAssignments(const std::string& debugMap);
  void             setWriteEachTimeStep(bool writeEachTimeStep);
  void             setRunDirectory     (const com::PathName& runDirectory);

  void             setExitValueType    (const ExitValueType exitValueType);
  void             setSeed             (size_t seed);

  void             setExternalBindingFile(const com::PathName& externalBindingFile);
  void             setPrintShellExpansionOnly(bool printShellExpansionOnly);
  void             setTestScriptRunableOnly(bool testScriptRunableOnly);
  void             setTestCaseTypeOnExistingName(bool testCaseTypeOnExistingName);
  void             setScriptFile       (const com::PathName& scriptFile);

  void             setCompile          (bool compile);



  void             resolve             (ASTSymbolTable& symbols,
                                        std::string const& areaMap,
                                        Timer const& timer);
  void             setProfile          (bool profile);
  void             setUseDiskStorage   (bool useDiskStorage);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  const IOStrategy&   ioStrategy                 () const;

  ExitValueType       exitValueType              () const;
  size_t              seed                       () const;

  const com::PathName& externalBindingFile       () const;
  bool                 printShellExpansionOnly   () const;
  bool                 testScriptRunableOnly     () const;
  bool                 testCaseTypeOnExistingName() const;
  const com::PathName& scriptFile                () const;

  bool                 compile                   () const;
  bool                 profile                   () const;
  bool                 useDiskStorage            () const;

  const geo::RasterSpace& rasterSpace            () const;

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



} // namespace calc

#endif
