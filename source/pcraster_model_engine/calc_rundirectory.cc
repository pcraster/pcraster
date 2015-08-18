#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_RUNDIRECTORY
#include "calc_rundirectory.h"
#define INCLUDED_CALC_RUNDIRECTORY
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif

// PCRaster library headers.
#ifndef INCLUDED_COM_DIRECTORY
#include "com_directory.h"
#define INCLUDED_COM_DIRECTORY
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_MEMORY
#include <memory>
#define INCLUDED_MEMORY
#endif

// Module headers.
#ifndef INCLUDED_CALC_POSEXCEPTION
#include "calc_posexception.h"
#define INCLUDED_CALC_POSEXCEPTION
#endif



/*!
  \file
  This file contains the implementation of the RunDirectory class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC RUNDIRECTORY MEMBERS
//------------------------------------------------------------------------------




//------------------------------------------------------------------------------
// DEFINITION OF RUNDIRECTORY MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::RunDirectory::RunDirectory()
{
}

calc::RunDirectory::RunDirectory(
    const com::PathName& rdPn)
{
  setRunDirectory(rdPn,"");
}

//! dtor
calc::RunDirectory::~RunDirectory()
{
}

//! set the run directory
/*!
   this will reset the output directory, and the search paths.

   \param runDirectory must be a valid name for a directory to
   write all output to. If empty then the output directory and search
   paths are set to default, none. all path delimeters are accepted.
   \param externalBindingsFile file path set from pcrcalc -b option, empty
   if not set
 */
void calc::RunDirectory::setRunDirectory(
    const com::PathName& runDirectory,
    const com::PathName& externalBindingsFile)
{
    d_searchPaths.clear();

    if (externalBindingsFile.isEmpty())
      d_runSettings.clear();
    else
      d_runSettings= RunSettings(externalBindingsFile);

    d_outputDirectory=runDirectory;
    d_outputDirectory.makeNative();

    if (d_outputDirectory.isEmpty())
      return; // output in current dir and no search paths
    com::PathName sp(d_outputDirectory);

    if (!com::PathInfo(sp).exists()) {
      // last part is to be created sub-directory
      sp.up();
      if (sp.isEmpty())
       return; // we do have an output directory but no search paths
    }

    PRECOND(!sp.isEmpty());
    // above sp.up() can make sp empty now
    if (!com::PathInfo(sp).isDirectory()) {
      // pcrcalc352
      std::ostringstream stream;
      stream << "-r: '" << sp << "' is not an existing directory";
      throw PosException(stream.str());
    }

    sp.makeNative();
    sp.makeAbsolute();
    com::PathName currentDir(com::currentWorkingDirectory());
    while (currentDir != sp && !sp.isEmpty()) {
      d_searchPaths.push_back(sp);
      sp.up();
    }
    if (sp.isEmpty()) {
       // current working dir is not a parent of d_runDirectory
       // no search paths
       d_searchPaths.clear();
    } else {
      d_searchPaths.push_back(currentDir);
    }

    collectRunSettings();
}

//! make a list of modelsettings to use in the run
/*!
 * check on all paths if there is a binding.ipcr with ModelRunSettings
 */
void calc::RunDirectory::collectRunSettings()
{
/*
 *  std::vector<com::PathName> cwd;
 *  cwd.push_back(com::currentWorkingDirectory());
 *  const std::vector<com::PathName> &paths(
 *      d_searchPaths.size() ? d_searchPaths:cwd);

 *  for(size_t p=0; p < paths.size(); p++)  {
 *   com::PathName b = paths[p]+"binding.ipcr";
 *   try {
 *    if (!com::PathInfo(b).isFile())
 *        continue;
 *    // if existant, we assume it is xml
 *    pcrxml::Document doc(b);
 *    QDomElement  mrsElement=doc.firstMatchByTagName("ModelRunSettings");
 *    if (mrsElement.isNull())
 *      continue;
 *    d_runSettings.addNewOnly(mrsElement);
 *  } catch (const com::BadStreamFormat &msg) {
 *    throw com::FileError(b, msg.messages());
 *  }
 * }
 */
}

//! check if \a fileName does not contain a directory name part
/*
 * \throws
 *  PosException if it does
 */
void calc::RunDirectory::checkOutputFilePath(
    const std::string& fileName) const
{
    PRECOND(!fileName.empty());
    if (empty())
      return;

    com::PathName pn(fileName);
    pn.makeNative();
    if (pn.baseName() != fileName) {
      std::ostringstream stream;
      // pcrcalc355
      stream << "-r: output '" << pn << "' has illegal directory part";
      throw PosException(stream.str());
    }
}

//! return a path, including \a fileName, where \a fileName is found
/*!
   Searches along search paths to find \a fileName. If \a fileName
   exists somewhere in the search paths, then \a fileName
   as absolute path name where found is returned.
   If not found in search paths, \a fileName is returned as is.
   \par found set if found yes/no
 */
std::string calc::RunDirectory::inPath(
    bool& found,
    const std::string& fileName) const
{
    PRECOND(!fileName.empty());
    com::PathName fnPn(fileName);
    for (size_t i=0; i < d_searchPaths.size(); i++) {
      // sPn=fnPn if fnPn is absolute, that is OK.
      com::PathName sPn(d_searchPaths[i]+fnPn);
      com::PathInfo sPi(sPn);
      if (sPi.exists()) {
        found=true;
        sPn.makeNative();
        return sPn.toString();
      }
    }
    found=false;
    return fileName;
}

std::string calc::RunDirectory::outputFilePath(const std::string& fileName) const
{
  if(!d_outputDirectory.isEmpty())
    com::createDirectory(d_outputDirectory,false);

  PRECOND(!fileName.empty());
  if (d_outputDirectory.isEmpty())
      return fileName;
  com::PathName pn(fileName);
  pn.makeNative();
  if (pn.isAbsolute())
    return pn.toString();
  return (d_outputDirectory+pn).toString();
}

const calc::ASTNodeVector& calc::RunDirectory::bindings() const
{
  return d_runSettings;
}

//! is this empty/default; does it default to the current directory?
/*!
 * In other wordt no run directory effective other then the current
 * directory which is default
 */
bool calc::RunDirectory::empty() const
{
  return d_outputDirectory.isEmpty();
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------
