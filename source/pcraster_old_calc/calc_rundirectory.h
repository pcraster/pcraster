#ifndef INCLUDED_OLDCALC_RUNDIRECTORY
#define INCLUDED_OLDCALC_RUNDIRECTORY

#include "stddefx.h"
#include "pcrdll.h"
#include "calc_runsettings.h"



namespace com {
  class PathName;
}



namespace calc {

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
  class RunDirectoryPrivate *d_data;

private:
  //! Assignment operator. NOT IMPLEMENTED.
  RunDirectory&           operator=           (const RunDirectory&);

  //! Copy constructor. NOT IMPLEMENTED.
                   RunDirectory               (const RunDirectory&);

   void collectRunSettings();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RunDirectory               ();

  /* virtual */    ~RunDirectory              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------
  void setRunDirectory(const com::PathName& runDirectory,
                       const com::PathName& externalBindingsFile);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  void setupForExecution() const;

  std::string outputFilePath(const std::string& fileName) const;
  std::string inputFilePath(bool& found, const std::string& fileName) const;

  bool isDefault() const;

  const std::map<calc::ExtSym,calc::ExtSym>& bindings() const;
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
