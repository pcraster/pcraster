#ifndef INCLUDED_CALC_RUNDIRECTORY
#define INCLUDED_CALC_RUNDIRECTORY



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
#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif



namespace com {
}



namespace calc {

class  ASTNodeVector;

//! the run directory object for the Model Run Organizer
/*!
   If the current working directory is a
   parent directory of the specified run directory then a search
   path for input files is established. Searching for input files
   and model parameter settings then starts at the runDirectory, then
   its parent, up to the current working directory.


   An alternative considered was:<i>
   If the specified run directory is not empty then a search
   path for input files is established. Searching for input files
   and model parameter settings then starts at the runDirectory, then
   its parent, up to a parent directory that is not accessible or the
   root is encountered.
   </i> The alternative is more straightforward, and seems to only traverse
   the whole path in case of error, file not found. Problem is where to stop
   for the parameter overriding scheme (external binding file).
 */
class RunDirectory
{
private:
  //  Assignment operator. DEFAULT
  // RunDirectory&           operator=           (const RunDirectory&);

  //  Copy constructor. DEFAULT
  //               RunDirectory               (const RunDirectory&);

  RunSettings   d_runSettings;

  //! if empty (default), output directory is current directory
  com::PathName d_outputDirectory;

  //! list of search paths, if empty then only current directory
  /*!
      this are all absolute paths
   */
  std::vector<com::PathName> d_searchPaths;

  void collectRunSettings();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RunDirectory               ();

                   RunDirectory               (const com::PathName& rdPn);

  /* virtual */    ~RunDirectory              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setRunDirectory(const com::PathName& runDirectory,
                       const com::PathName& externalBindingsFile);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  void checkOutputFilePath               (const std::string& fileName) const;
  std::string outputFilePath             (const std::string& fileName) const;

  std::string inPath(bool& found, const std::string& fileName) const;

  bool empty() const;

  const ASTNodeVector& bindings() const;
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



} // namespace calc

#endif
