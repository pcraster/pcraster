#ifndef INCLUDED_DAL_FILESYSTEMUTILS
#include "dal_FilesystemUtils.h"
#define INCLUDED_DAL_FILESYSTEMUTILS
#endif

// Library headers.
#ifndef INCLUDED_CSTDIO
#include <cstdio>
#define INCLUDED_CSTDIO
#endif

#ifdef WIN32
  #ifndef INCLUDED_IO
  #include <io.h>            // access
  #define INCLUDED_IO
  #endif

  #define   F_OK 0
  #define   R_OK 4
  #define   W_OK 2
#else
  #ifndef INCLUDED_UNISTD
  #include <unistd.h>
  #define INCLUDED_UNISTD
  #endif
#endif

#ifndef INCLUDED_BOOST_DATE_TIME_GREGORIAN_GREGORIAN
#include <boost/date_time/gregorian/gregorian.hpp>
#define INCLUDED_BOOST_DATE_TIME_GREGORIAN_GREGORIAN
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#include <boost/filesystem/exception.hpp>
#define INCLUDED_BOOST_FILESYSTEM_EXCEPTION
#endif

#ifndef INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#include <boost/filesystem/operations.hpp>
#define INCLUDED_BOOST_FILESYSTEM_OPERATIONS
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_BOOST_FORMAT
#include <boost/format.hpp>
#define INCLUDED_BOOST_FORMAT
#endif

#ifndef INCLUDED_BOOST_LEXICAL_CAST
#include <boost/lexical_cast.hpp>
#define INCLUDED_BOOST_LEXICAL_CAST
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_EXCEPTION
#include "dal_Exception.h"
#define INCLUDED_DAL_EXCEPTION
#endif

#ifndef INCLUDED_DAL_STACKINFO
#include "dal_StackInfo.h"
#define INCLUDED_DAL_STACKINFO
#endif



namespace dal {

void testPathnameIsEmpty (
         std::string const& pathName)
{
  if(pathName.empty()) {
    throw Exception(
         (boost::format("Pathname '%1%': Empty")
         % pathName).str());
  }
}



// //!
// /*!
//   \param     .
//   \return    .
//   \exception .
//   \warning   .
//   \sa        .
//   \todo      From the boost docs: Path name checking functionality has
//              been moved out of class path and into separate
//              free-functions. This still provides name checking for those
//              who need it, but with much less impact on those who don't
//              need it.
//              So: change this function and others relying on exception throwing
//              by the path constructor!!!
// */
// PCR_DAL_DECL bool pathnameIsNative(
//          std::string const& pathname)
// {
//   // bool result = false;
// 
//   // if(!pathname.empty()) {
//   //   try {
//   //     boost::filesystem::path(pathname);
//   //     result = true;
//   //   }
//   //   catch(boost::filesystem::filesystem_error const&) {
//   //   }
//   // }
// 
//   // return result;
// 
//   return boost::filesystem::native(pathname);
// }



// //! Tests whether \a pathname is valid on the current platform.
// /*!
//   \param     pathname Name to test.
//   \exception Exception When \a pathname is not valid.
//   \warning   Always test your pathnames before pushing them around in your app,
//              especially when pathnames come from external sources, like a user.
// */
// PCR_DAL_DECL void testPathnameIsNative(
//          std::string const& pathname)
// {
//   if(pathname.empty()) {
//     throw Exception(
//          // Leaks according to valgrind...
//          (boost::format("Pathname '%1%': Empty")
//          % pathname).str());
//   }
//   else {
//     boost::filesystem::path path(pathname);
//     std::string rootName(path.root_path().string());
//     std::string bla(pathname);
// 
//     bla.erase(0, rootName.size());
// 
//     std::cout << bla << std::endl;
// 
//     if(!(!rootName.empty() && bla.empty())) {
//       if(!pathnameIsNative(bla)) {
//         throw Exception(
//              (boost::format("Pathname '%1%': Not valid on the current platform")
//              % pathname).str());
//       }
//     }
//   }
// 
// 
// 
//   // else if(!pathnameIsNative(pathname)) {
//   //   throw Exception(
//   //        (boost::format("Pathname '%1%': Not valid on the current platform")
//   //        % pathname).str());
//   // }
// }



void testPathExists(
         boost::filesystem::path const& path)
{
  if(!boost::filesystem::exists(path)) {
    throw Exception(
         (boost::format("Pathname '%1%': Path does not exist")
         % path.string()).str());
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      In case path is the name of a link the current implementation
             does not check whether it points to a regular file.
*/
void testPathIsFileOrLinkToFile(
         boost::filesystem::path const& path)
{
  if(boost::filesystem::is_directory(path)) {
    throw Exception(
         (boost::format("Pathname '%1%': Path is not a file or link to a file")
         % path.string()).str());
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   This function assumes that the file exists.
  \sa        Test.
*/
PCR_DAL_DECL bool isReadable(
         boost::filesystem::path const& path)
{
  assert(boost::filesystem::exists(path));

  return ::
#ifdef _MSC_VER
    _access
#else
    access
#endif
         (path.string().c_str(), R_OK) == 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   This function assumes that the file exists.
  \sa        Test.
*/
PCR_DAL_DECL bool isWritable(
         boost::filesystem::path const& path)
{
  assert(boost::filesystem::exists(path));

  return ::
#ifdef _MSC_VER
    _access
#else
    access
#endif
         (path.string().c_str(), W_OK) == 0;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Test.
*/
void testPathIsReadable(
         boost::filesystem::path const& path)
{
  if(!isReadable(path)) {
    throw Exception(
         (boost::format("Pathname '%1%': Path is not readable")
         % path.string()).str());
  }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Test.
*/
PCR_DAL_DECL void testPathIsWritable(
         boost::filesystem::path const& path)
{
  if(!isWritable(path)) {
    throw Exception(
         (boost::format("Pathname '%1%': Path is not writable")
         % path.string()).str());
  }
}



//! Tests whether \a path is a file or link to a file which can be opened for reading.
/*!
  \param     path Path to test.
  \exception Exception When \a path is not a file or link, or can not be opened.
  \sa        testPathExists(boost::filesystem::path const&),
             testPathIsFileOrLinkToFile(boost::filesystem::path const&),
             testPathIsReadable(boost::filesystem::path const&),
             canBeOpenedForReading(boost::filesystem::path const&)
*/
PCR_DAL_DECL void testFileCanBeOpenedForReading(
         boost::filesystem::path const& path)
{
  testPathExists(path);
  testPathIsFileOrLinkToFile(path);
  testPathIsReadable(path);
}



//! Creates a path object with time step information.
/*!
  \param     parent Parent path to \a filename.
  \param     filename Path object pointed to by \a parent.
  \param     timeStep Time step.
  \return    path object.
  \warning   \a filename must really by the filename and not contain any path
             information.

  This function respects the old 8.3 DOS path naming convention if possible.
  If there is no room for the \a timeStep (\a filename takes op all 8.3 characters)
  than \a parent / \a filename is returned. Also, if \a filename has an extension of
  size 3 \a parent / \a filename is returned (this can be considered a
  HACK/FEATURE).

  Because of the limited room in the 8.3 naming scheme, best results are
  obtained when \a filename and \a timeStep are as small as possible.

  When needed, \a filename will overwrite the \a timeStep information in the
  result.
*/
PCR_DAL_DECL boost::filesystem::path timeStepPath83(
         std::string const& parent,
         std::string filename,
         size_t timeStep)
{
  size_t lastPoint = filename.find_last_of(".");

  // CW: this is a hack, feature if a calc script
  // CW: has a dynamic report of 8+3 length then at
  // CW: each timestep the same map is written,
  // CW: result: only keep the last step
  if(filename.size() <= 11 &&
         (lastPoint == std::string::npos || filename.size() - lastPoint < 4)) {
    // There is room for a time step and there is no extension or there is
    // one which conforms to the .3 DOS convention.

    //  ----------1-
    //  012345678901
    //  xxxxxxxx.xxx
    char result[13];

    // Print as 8+3.
    (void)std::sprintf(result, "%011d", (int)timeStep);

    // Move overlapping last 3 digits + '\0' to insert '.'.
    (void)std::memmove(result + 9, result + 8, 4);
    result[8] = '.';

    // Overprint prefix result with the name part excluding '\0'.
    (void)std::memcpy(result, filename.c_str(), filename.size());

    filename = result;
  }

  return boost::filesystem::path(parent) / filename;
}



PCR_DAL_DECL boost::filesystem::path timeStepPathNewStyle(
         std::string const& parent,
         std::string filename,
         size_t timeStep)
{
  /*
  size_t lastPoint = filename.find_last_of(".");

  if(lastPoint != std::string::npos) {
    // Use extension to determine format.
    assert(Library::isInitialised());
    Format const* format = library->knownFormats().formatByExtension(
         filename.substr(lastPoint + 1, filename.size() - lastPoint + 1));

    if(format && format->isFileBased() && format->isForGraphics()) {
      return timeStepPathNewStyle(parent, filename, timeStep);
    }
  }

  // If we end up here, filename has to be formatted according to the 8.3 DOS
  // convention.
  return timeStepPath83(parent, filename, timeStep);
  */


  size_t lastPoint = filename.find_last_of(".");

  if(lastPoint == std::string::npos) {
    // No extension present.
    filename = (boost::format("%1%_%2%")
         % filename
         % timeStep
         // % boost::io::group(std::setw(fieldWidth), std::setfill('0'), timeStep)
         ).str();
  }
  else {
    // Extension present (possibly empty).
    filename = (boost::format("%1%_%2%.%3%")
         % filename.substr(0, lastPoint)
         % timeStep
         // % boost::io::group(std::setw(fieldWidth), std::setfill('0'), timeStep)
         % filename.substr(lastPoint + 1, filename.size() - lastPoint + 1)
         ).str();
  }

  return boost::filesystem::path(parent) / filename;
}



//! Formats a path name for data set file \a filename for a \a timeStep, according to a file name \a convention.
/*!
  \param     parent Parent directory of the filename.
  \param     filename Name of the data set file, without time step information.
  \param     timeStep Time step.
  \param     convention File name convention.
  \return    Path name.
  \exception .
  \warning   In case \a convention equals PCRConvention, \a filename must not
             contain an extension of more than 2 characters after the dot.
             DALConvention does support combining extensions and time steps
             in the path name.
  \sa        .
*/
PCR_DAL_DECL boost::filesystem::path timeStepPath(
         std::string const& parent,
         std::string const& filename,
         size_t timeStep,
         FilenameConvention convention)
{
  assert(!filename.empty());

  boost::filesystem::path result;

  switch(convention) {
    case PCRConvention: {
      assert(boost::filesystem::path(filename).extension().string().size() < 4);
      result = timeStepPath83(parent, filename, timeStep);
      break;
    }
    default: {
      result = timeStepPathNewStyle(parent, filename, timeStep);
      break;
    }
  }

  return result;
}



PCR_DAL_DECL boost::filesystem::path timeStepPath83(
         boost::filesystem::path const& path,
         size_t timeStep)
{
  // Split filename from the parent path.
  std::string parent = path.parent_path().string();
  std::string filename = path.filename().string();

  return timeStepPath83(parent, filename, timeStep);
}



PCR_DAL_DECL boost::filesystem::path timeStepPath(
         boost::filesystem::path const& path,
         size_t timeStep,
         FilenameConvention convention)
{
  // Split filename from the parent path.
  std::string parent = path.parent_path().string();
  std::string filename = path.filename().string();

  return timeStepPath(parent, filename, timeStep, convention);
}



// boost::filesystem::path timeStepPath(
//          boost::filesystem::path const& path,
//          size_t timeStep)
// {
//   return timeStepPath(path, timeStep);
// }



PCR_DAL_DECL boost::filesystem::path timeStepPath(
         boost::filesystem::path const& path,
         boost::gregorian::date const& date,
         FilenameConvention convention)
{
  size_t dateNr = static_cast<size_t>(std::strtol(
         boost::gregorian::to_iso_string(date).c_str(), 0, 10));
  return timeStepPath(path, dateNr, convention);
}



//! Returns whether \a path is a file or link to a file which can be opened for reading.
/*!
  \param     path Path to test.
  \return    True or false.
  \sa        testCanBeOpenedForReading(boost::filesystem::path const&)
  \todo      Add test for readability!
*/
bool canBeOpenedForReading(
         boost::filesystem::path const& path)
{
  return dal::exists(path) && !boost::filesystem::is_directory(path);
    // && isReadable(path);
}



PCR_DAL_DECL bool exists(
         boost::filesystem::path const& path)
{
  return boost::filesystem::exists(path);
}



//! Returns whether file \a name exists at \a address in \a space.
/*!
  \param     name Name of data set.
  \param     space Data space.
  \param     address Dat space address.
  \param     convention File naming convention.
  \return    true or false

  Returns also false when \a name is not a valid filename on the current
  platform (will not throw an Exception in that case).
*/
PCR_DAL_DECL bool pathExists(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         FilenameConvention convention)
{
  try {
    return dal::exists(pathForDataSpaceAddress(
         name, space, address, convention));
  }
  catch(Exception const&) {
    return false;
  }
}



//! Removes the file or link with name \a path and returns whether the file or path existed before removal.
/*!
  \param     path Name of file or link to remove.
  \return    true or false
  \warning   This function does not check whether a file or link with name \a path exists.
*/
bool remove(
         boost::filesystem::path const& path)
{
  return boost::filesystem::remove(path);
}



//! Parses \a name as if it is formatted according to the previous stack naming conventions, and returns its name and data space.
/*!
  \param     name Name of the stack.
  \return    Tuple with the name of the stack and the data space with the
             information about the time dimension found in the \a name
             passed in.
  \sa        StackInfo

  When \a name is not a valid old stack name, than a tuple of \a name and an
  empty data space will be returned.
*/
PCR_DAL_DECL boost::tuple<std::string, dal::DataSpace>
oldStackName2NameSpaceTuple(
         std::string const& name)
{
  boost::tuple<std::string, DataSpace> result;
  DataSpace space;

  try {
    StackInfo info(name, false);

    if(info.isDynamic()) {
      std::vector<size_t> timeSteps;
      timeSteps.push_back(info.first());
      timeSteps.push_back(info.last());
      timeSteps.push_back(1);

      space.addDimension(Dimension(Time, timeSteps));
    }

    result = boost::make_tuple(info.name(), space);
  }
  catch(Exception&) {
    // name is an illegal old stack name, nothing to do.
    assert(space.size() == 0);
    result = boost::make_tuple(name, space);
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \exception Exception When the filename created is not valid on the current
             platform.
  todo       Some combinations never occur? Samples AND CumulativeProbabilities
             dimensions are never present in one space I guess, see merge in
             DataSpace class.
*/
PCR_DAL_DECL boost::filesystem::path pathForDataSpaceAddress(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address,
         FilenameConvention convention)
{
  assert(!name.empty());

  size_t scenariosIndex = space.indexOf(Scenarios);
  size_t cumProbIndex   = space.indexOf(CumulativeProbabilities);
  size_t samplesIndex   = space.indexOf(Samples);
  size_t timeIndex      = space.indexOf(Time);

  bool hasScenarioName = scenariosIndex < space.rank() &&
         address.isValid(scenariosIndex);
  bool hasQuantile = cumProbIndex < space.rank() &&
         address.isValid(cumProbIndex);
  bool hasSampleNumber = samplesIndex < space.rank() &&
         address.isValid(samplesIndex);
  bool hasTimeStep = timeIndex < space.rank() &&
         address.isValid(timeIndex);

  std::string scenarioName = hasScenarioName
         ? address.coordinate<std::string>(scenariosIndex)
         : "";
  float quantile = hasQuantile
         ? address.coordinate<float>(cumProbIndex)
         : -1.0f;
  size_t sampleNumber = hasSampleNumber
         ? address.coordinate<size_t>(samplesIndex)
         : 0;
  size_t timeStep = hasTimeStep
         ? address.coordinate<size_t>(timeIndex)
         : 0;

  // size_t timeStepFieldWidth = 0;

  // if(hasTimeStep) {
  //   // Determine field width of highest time step.
  //   Dimension const& dimension = space.dimension(timeIndex);
  //   timeStepFieldWidth = (boost::format("%1%") % dimension.coordinate<size_t>(
  //        dimension.nrCoordinates() - 1)).str().size();
  // }

  boost::filesystem::path path;

  if(hasScenarioName) {
    if(hasQuantile) {
      if(hasSampleNumber) {
        if(hasTimeStep) {
          path = pathForScenarioQuantileSampleTime(name, scenarioName,
              quantile, sampleNumber, timeStep, convention);
        }
        else {
          path = pathForScenarioQuantileSample(name, scenarioName, quantile,
              sampleNumber, convention);
        }
      }
      else {
        if(hasTimeStep) {
          path = pathForScenarioQuantileTime(name, scenarioName, quantile,
              timeStep, convention);
        }
        else {
          path = pathForScenarioQuantile(name, scenarioName, quantile, convention);
        }
      }
    }
    else {
      if(hasSampleNumber) {
        if(hasTimeStep) {
          path = pathForScenarioSampleTime(name, scenarioName, sampleNumber,
              timeStep, convention);
        }
        else {
          path = pathForScenarioSample(name, scenarioName, sampleNumber, convention);
        }
      }
      else {
        if(hasTimeStep) {
          path = pathForScenarioTime(name, scenarioName, timeStep, convention);
        }
        else {
          path = pathForScenario(name, scenarioName, convention);
        }
      }
    }
  }
  else {
    if(hasQuantile) {
      if(hasSampleNumber) {
        if(hasTimeStep) {
          path = pathForQuantileSampleTime(name, quantile, sampleNumber,
              timeStep, convention);
        }
        else {
          path = pathForQuantileSample(name, quantile, sampleNumber, convention);
        }
      }
      else {
        if(hasTimeStep) {
          path = pathForQuantileTime(name, quantile, timeStep, convention);
        }
        else {
          path = pathForQuantile(name, quantile, convention);
        }
      }
    }
    else {
      if(hasSampleNumber) {
        if(hasTimeStep) {
          path = pathForSampleTime(name, sampleNumber, timeStep, convention);
        }
        else {
          path = pathForSample(name, sampleNumber, convention);
        }
      }
      else {
        if(hasTimeStep) {
          path = pathForTime(name, timeStep, convention);
        }
        else {
          path = pathFor(name);
        }
      }
    }
  }

  assert(!path.empty());

  return path;
}



boost::filesystem::path pathForScenarioQuantileSampleTime(
         std::string const& name,
         std::string const& scenarioName,
         float quantile,
         size_t sampleNumber,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForScenario(
           pathForQuantile(
             pathForSample(
               pathForTime(name, timeStep, convention).string(),
               sampleNumber).string(),
             quantile).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioQuantileSample(
         std::string const& name,
         std::string const& scenarioName,
         float quantile,
         size_t sampleNumber,
         FilenameConvention /* convention */)
{
  return pathForScenario(
           pathForQuantile(
             pathForSample(name, sampleNumber).string(),
             quantile).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioQuantileTime(
         std::string const& name,
         std::string const& scenarioName,
         float quantile,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForScenario(
           pathForQuantile(
             pathForTime(name, timeStep, convention).string(),
             quantile).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioQuantile(
         std::string const& name,
         std::string const& scenarioName,
         float quantile,
         FilenameConvention /* convention */)
{
  return pathForScenario(
           pathForQuantile(name, quantile).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioSampleTime(
         std::string const& name,
         std::string const& scenarioName,
         size_t sampleNumber,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForScenario(
           pathForSample(
             pathForTime(name, timeStep, convention).string(),
             sampleNumber).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioSample(
         std::string const& name,
         std::string const& scenarioName,
         size_t sampleNumber,
         FilenameConvention /* convention */)
{
  return pathForScenario(
           pathForSample(name, sampleNumber).string(),
           scenarioName);
}



boost::filesystem::path pathForScenarioTime(
         std::string const& name,
         std::string const& scenarioName,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForScenario(
           pathForTime(name, timeStep, convention).string(),
           scenarioName);
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
boost::filesystem::path pathForScenario(
         std::string const& name,
         std::string const& scenarioName,
         FilenameConvention /* convention */)
{
  boost::filesystem::path path(name);

  return path.parent_path() / pathFor(scenarioName) / pathFor(path.filename().string());
}



boost::filesystem::path pathForQuantileSampleTime(
         std::string const& name,
         float quantile,
         size_t sampleNumber,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForQuantile(
           pathForSample(
             pathForTime(name, timeStep, convention).string(),
             sampleNumber).string(),
           quantile);
}



boost::filesystem::path pathForQuantileSample(
         std::string const& name,
         float quantile,
         size_t sampleNumber,
         FilenameConvention /* convention */)
{
  return pathForQuantile(
           pathForSample(name, sampleNumber).string(),
           quantile);
}



boost::filesystem::path pathForQuantileTime(
         std::string const& name,
         float quantile,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForQuantile(
           pathForTime(
             name,
             timeStep,
             convention).string(),
           quantile);
}



boost::filesystem::path pathForQuantile(
         std::string const& name,
         float quantile,
         FilenameConvention /* convention */)
{
  testPathnameIsEmpty(name);

  boost::filesystem::path result;
  size_t lastPoint = name.find_last_of(".");

  if(lastPoint == std::string::npos) {
    // No extension present.
    result = boost::filesystem::path((boost::format("%1%_%2%")
         % name
         % quantile).str());
  }
  else {
    // Extension present (possibly empty).
    result = boost::filesystem::path((boost::format("%1%_%2%.%3%")
         % name.substr(0, lastPoint)
         % quantile
         % name.substr(lastPoint + 1, name.size() - lastPoint + 1)).str());
  }

  return result;
}



boost::filesystem::path pathForSampleTime(
         std::string const& name,
         size_t sampleNumber,
         size_t timeStep,
         FilenameConvention convention)
{
  return pathForSample(
           pathForTime(name, timeStep, convention).string(),
           sampleNumber);
}



boost::filesystem::path pathForSample(
         std::string const& name,
         size_t sampleNumber,
         FilenameConvention /* convention */)
{
  boost::filesystem::path path(name);

  return path.parent_path() /
         boost::lexical_cast<std::string>(sampleNumber) /
         pathFor(path.filename().string());
}



boost::filesystem::path pathForTime(
         std::string const& name,
         size_t timeStep,
         FilenameConvention convention)
{
  return timeStepPath(pathFor(name), timeStep, convention);
}



PCR_DAL_DECL boost::filesystem::path pathFor(
         std::string const& name)
{
  testPathnameIsEmpty(name);

  return boost::filesystem::path(name);
}



/*!
  \overload
*/
PCR_DAL_DECL boost::filesystem::path addExtensionIfNeeded(
         std::string const& name,
         std::string const& extension)
{
  return addExtensionIfNeeded(pathFor(name), extension);
}



//! Returns a copy of \a path with \a extension added if an extension is not already present.
/*!
  \param     path Path to copy.
  \param     extension Extension to add.
  \return    Copy of the \a path passed in.
  \warning   It is assumed that \a path and \a extension passed in are not
             empty. Also, \a extension should start with a '.'.
*/
boost::filesystem::path addExtensionIfNeeded(
         boost::filesystem::path const& path,
         std::string const& extension)
{
  assert(!path.empty() && !extension.empty());
  assert(extension[0] == '.');

  boost::filesystem::path result(path);
  std::string currentExtension(path.extension().string());

  if(currentExtension.empty() || currentExtension == ".") {
    result = result.replace_extension(extension);
  }

  return result;
}



void possibleFileBasedAttributeFileNames(
         boost::filesystem::path const& path,
         std::vector<std::string>& leaves)
{
  namespace bfs = boost::filesystem;

  if(!bfs::is_directory(path)) {
    return;
  }

  std::string filename, baseName, extension;

  std::vector<std::string> fileTypesToSkip;
  fileTypesToSkip.push_back(".zip");
#ifdef _WIN32
  fileTypesToSkip.push_back(".bat");
  fileTypesToSkip.push_back(".exe");
  fileTypesToSkip.push_back(".com");
#endif

  for(bfs::directory_iterator it(path); it != bfs::directory_iterator(); ++it) {
    if(bfs::is_directory(it->status())) {
      continue;
    }

    // Try to safely skip as many files as we can by only looking at the name.
    // This is more efficient than trying to open files.
    filename = it->path().filename().string();
    baseName = bfs::path(filename).stem().string();
    extension = bfs::path(filename).extension().string();

#ifdef linux
    if(baseName.empty() || baseName[0] == '.') {
      continue;
    }
#endif

    if(std::find(fileTypesToSkip.begin(), fileTypesToSkip.end(), extension) !=
         fileTypesToSkip.end()) {
      continue;
    }

    leaves.push_back(std::string(filename.c_str()));
  }

  std::sort(leaves.begin(), leaves.end());
}



// std::string fixPathname(
//          std::string const& name)
// {
//   std::string result(name);
// 
// #ifdef WIN32
//   // boost filesystem lib doesn't like backslashes.
//   while(result.find('\\') != std::string::npos) {
//     result.replace(result.find('\\'), 1, 1, '/');
//   }
// 
//   // and adjacent slashes.
//   while(result.find("//") != std::string::npos) {
//     result.erase(result.find("//"), 1);
//   }
// #endif
// 
//   return result;
// }

} // namespace dal

