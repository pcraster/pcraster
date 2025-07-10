#include "dal_RasterDriver.h"
#include "dal_Def.h"
#include "dal_FilesystemUtils.h"
#include "dal_Library.h"
#include "dal_RasterDimensions.h"
#include "dal_RegularExpressions.h"

#include <boost/function.hpp>
#include <boost/lexical_cast.hpp>

#include <cmath>
#include <format>


/*!
  \file
  This file contains the implementation of the RasterDriver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RASTERDRIVER MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RASTERDRIVER MEMBERS
//------------------------------------------------------------------------------

RasterDriver::RasterDriver(
         Format const& format)

  : Driver(format)

{
}



RasterDriver::~RasterDriver()
{
}



//! If raster \a name exists in \a space at \a address its filenaming characteristics are stored for later reference.
/*!
  \param     name Name of dataset
  \param     space Data space \a name is defined in.
  \param     address Address to look for name.
  \todo      Test the default extension support: does a file get picked up
             even when the name does not contain its extension? Is information
             about the extension registered ok. Test optional character of this
             property: does not always gets set, don't use during write when
             it is not set.
  \todo      Move stuff like this in a Namer policy which is driver instance
             specific and handles the naming of data sets and their individual
             parts (files, tables, whatever), given a name, data space and
             data space address. So, drivers should be templates with such a
             policy. Naming stuff does not belong in this hierarchy.

  Filenaming characteristics are the filename convention used for incorporating
  coordinate information in the filename and whether the/a default extension
  is used to name the file on disk.

  Nothing happens if the filenaming characteristics can not be determined.
  Nothing happens if the filenaming characteristics for \a name are already
  known.
*/
std::tuple<bool, FilenameConvention, std::string>
RasterDriver::determineFilenameCharacteristics(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.isValid(address));

  bool found = false;
  FilenameConvention convention;
  std::string extension;

  std::string key(propertiesKey(name, space));

  if(hasProperties(key)) {
    // Short-cutting. Information about this dataset has already be determined
    // and is cached. I may not have been found, but we don't need to look
    // for the information a second time. We get default values if the
    // information was not found earlier.
    found = !this->properties(key).isEmpty();
    convention = filenameConvention(key);
    extension = defaultExtension(key);
  }
  else {
    typedef boost::function< bool (std::string const&)> CallBack;

    CallBack callBack(dal::exists);
    std::tie(found, convention, extension) =
         dal::determineFilenameCharacteristics<CallBack>(callBack,
              name, space, address, format().extensions());
  }

  return std::tie(found, convention, extension);
}



void RasterDriver::cacheDatasetInfo(
    std::string const& key,
    FilenameConvention convention,
    std::string const& extension) const
{
    Properties& properties(this->properties(key));
    assert(properties.isEmpty());  // No properties stored yet.
    assert(convention != UnknownFilenameConvention);

    properties.setValue<FilenameConvention>(DAL_FILENAME_CONVENTION,
        convention);

    if(!extension.empty()) {
        properties.setValue<std::string>(DAL_DEFAULT_EXTENSION,
            extension);
    }
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      See todo of determineFilenameCharacteristics.
*/
std::filesystem::path RasterDriver::pathFor(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(space.isValid(address));

  bool found = false;
  FilenameConvention convention;
  std::string extension;
  std::tie(found, convention, extension) = determineFilenameCharacteristics(
      name, space, address);

  if(library()->cacheDatasetInfo() && found) {
    std::string key(propertiesKey(name, space));

    if(!hasProperties(key)) {
      cacheDatasetInfo(key, convention, extension);
    }
  }

  return dal::pathForDataSpaceAddress(name + extension, space, address,
      convention);
}



Raster* RasterDriver::open(
         std::string const& name) const
{
  return open(name, DataSpace(), DataSpaceAddress(), TI_NR_TYPES);
}



Raster* RasterDriver::open(
         std::string const& name,
         TypeId typeId) const
{
  return open(name, DataSpace(), DataSpaceAddress(), typeId);
}



Raster* RasterDriver::open(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  return open(name, space, address, TI_NR_TYPES);
}



/*!
  \overload
  \warning   In case the raster contains no cells, an empty DataSpace object is returned.

  This function assumes that the dataset consists of seperate files that don't
  contain information about the overall dataspace. It probes one specific
  raster for information about the number of rows and columns.

  The resulting dataspace is empty or contains two spatial dimensions
  representing the rows and columns in the raster. Overloaded versions of this
  function might be able to find out more about the dimensions present in the
  dataset.
*/
DataSpace RasterDriver::dataSpace(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  assert(!space.hasSpace());

  std::shared_ptr<Raster> raster(open(name, space, address));

  if(!raster) {
    throwCannotBeOpened(name, RASTER, space, address);
  }

  DataSpace result;

  if(raster->nrCells() > 0) {
    result.addDimension(Dimension(Space, RegularDiscretisation,
         RasterDimensions(raster->nrRows(), raster->nrCols(),
              raster->cellSize(), raster->west(), raster->north())));
  }

  return result;
}



Raster* RasterDriver::read(
         std::string const& name) const
{
  return read(name, DataSpace(), DataSpaceAddress(), TI_NR_TYPES);
}



Raster* RasterDriver::read(
         std::string const& name,
         TypeId typeId) const
{
  return read(name, DataSpace(), DataSpaceAddress(), typeId);
}



// Raster* RasterDriver::read(
//          std::string const& name,
//          DataSpaceAddress const& address) const
// {
//   return read(name, DataSpace(), address, TI_NR_TYPES);
// }



Raster* RasterDriver::read(
         std::string const& name,
         DataSpace const& space,
         DataSpaceAddress const& address) const
{
  return read(name, space, address, TI_NR_TYPES);
}



//! Reads raster \a name. Cell values are stored in \a raster.
/*!
  \param     name Name of raster.
  \param     raster Raster to store values in. If Matrix::cellsAreCreated() then the values are stored in the existing cell space of the raster. Otherwise raster will allocate new space with Matrix::createCells() In case of exception the exact state of raster is undefined (e..g. what if any values will it contain) but it is still correctly destructable.

  \exception Exception If the properties of the raster in \a name do not
             correspond with the properties of \a raster. If values in
             \a name cannot be converted to the type of the values in
             \a raster.
  \warning   The underlying I/O library is responsible for converting values
             to \a typeId. It is possible that certain conversions are not
             supported by the library. The caller should check whether the
             conversion is feasible.
*/
void RasterDriver::read(
         Raster& raster,
         std::string const& name) const
{
  read(raster, name, DataSpace(), DataSpaceAddress());
}



bool RasterDriver::extremes(
         boost::any& min,
         boost::any& max,
         TypeId typeId,
         std::string const& name,
         DataSpace const& space) const
{
  assert(!space.hasSpace());

  switch(typeId) {
    case TI_UINT1: {
      UINT1 i = 0;
      UINT1 a = 0;
      if(extremes<UINT1>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    case TI_INT4: {
      INT4 i = 0;
      INT4 a = 0;
      if(extremes<INT4>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    case TI_REAL4: {
      REAL4 i = NAN;
      REAL4 a = NAN;
      if(extremes<REAL4>(i, a, name, space, typeId)) {
        min = i;
        max = a;
        return true;
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return false;
}



/// //!
/// /*!
///   \param     .
///   \return    .
///   \exception .
///   \warning   .
///   \sa        .
///   \todo      Test whether only time or cum probs is defined and the other
///              dimensions are empty or contain only one coordinate.
///   \todo      Create and call template versions of read.
///
///   This function resizes the table before all values are written.
/// */
/// void RasterDriver::read(
///          Table& table,
///          std::string const& name,
///          DataSpace const& space) const
/// {
///   assert(space.nrWideDimensions() == 1);
///   assert(space.hasRaster());
///   assert(table.nrCols() == 1);
///   assert(table.typeId(0) == TI_REAL4);
///
///   // if(space.hasScenarios()) {
///   //   assert(space.dimension(
///   //        space.indexOf(Scenarios)).nrValues() == 1);
///   // }
///
///   // assert(!space.hasSamples());
///   // assert(!(space.hasTime() && space.hasCumProbabilities()));
///
///   table.resize(space.dimension(space.indexOfWideDimension()).nrCoordinates());
///   Array<REAL4>& array = table.col<REAL4>(0);
///
///   size_t rec = 0;
///
///   for(DataSpaceIterator it = space.begin(); it != space.end(); ++it, ++rec) {
///     assert(rec < array.size());
///
///     if(!exists(name, space, *it)) {
///       pcr::setMV(array[rec]);
///     }
///     else {
///       read(&(array[rec]), table.typeId(0), name, space, *it);
///     }
///   }
///
///   assert(rec == array.size());
/// }



//! Writes \a raster to file \a name.
/*!
  \param     raster Raster object to write from.
  \param     name Name of raster file to write.
  \exception Exception If something goes wrong while opening the file or
                       writing the values.

  \todo Suggestie van Cees voor properties in gdal (papszOptions) style maar
        dan stonger typed.
        \code
          dal::Properties props;
          props["valueScale"] = VS_NOMINAL;
          props["angle"]      = static_cast<double>(0.0);

          //! mv value to write out where T matches r.typeId();
          props["mvOut"]      = static_cast<T>(v);

          const dal::Table& legend;
          props["legend"]     = legend
        \endcode
   \todo Hoe komen we te weten dat deze driver:
         - schrijven voor raster.typeId() ondersteunt? KDJ: RasterDriver class moet een member hebben waaruit blijkt welke typeId's ie kan schrijven.
         - welke MV's hij verwacht/ondersteunt? KDJ: in-app MV's zijn altijd de CSF MV's. in-file MV's zijn volgens mij in de formaat driver bekend(?).
   \todo make pure virtual once all driver implement it.

 */
void RasterDriver::write(
         Raster const& raster,
         std::string const& name) const
{
  write(raster, DataSpace(), DataSpaceAddress(), name);
}



//! Writes the data in \a raster to a dataset with name \a name.
/*!
  \param     raster Data to write.
  \param     space Data space to write in.
  \param     address Address in space to write at.
  \param     name Name of dataset to write.
  \exception .
  \warning   .
  \sa        .
  \todo      Make pure virtual once all drivers implement it.
*/
void RasterDriver::write(
         Raster const& /* raster */,
         DataSpace const& /* space */,
         DataSpaceAddress const& /* address */,
         std::string const& /* name */) const
{
  assert(false);
}



void RasterDriver::browseFileBasedRasterAttributes(
         std::vector<BrowseInfo>& attributes,
         std::filesystem::path const& path,
         FilenameConventions conventions) const
{
  // Determine list of candidate file names of files to consider.
  std::vector<std::string> leaves;
  possibleFileBasedAttributeFileNames(path, leaves);

  // Order checks is relevant!
  assert(conventions & DALConvention);

  std::vector<size_t> ids;
  std::set<size_t> steps;
  std::set<float> quantiles;
  std::string name;
  std::string step;
  std::string quantile;
  std::string extension;
  std::regex regex;
  std::smatch match;
  Raster* raster = nullptr;

  std::vector<std::string> const& extensions(format().extensions());

  // Quantiles of temporal rasters.
  // <name>_<step>_<quantile>.{extension}
  for(int i = 0; i < int(leaves.size()); ++i) {
    if(std::regex_match(leaves[i], match,
         quantileOfTemporalRasterRegex)) {
      name = std::string(match[1].first, match[1].second);
      step = std::string(match[2].first, match[2].second);
      quantile = std::string(match[3].first, match[3].second);
      extension = match[4].matched ?
         std::string(match[4].first, match[4].second) : "";

      // Find all members of the data set.

      regex = std::regex(std::format(
         "{0}_({1})_({2}){3}",
         name,
         timeStepPattern,
         quantilePattern,
         extension));

      steps.clear();
      quantiles.clear();
      ids.clear();
      steps.insert(boost::lexical_cast<size_t>(step));
      quantiles.insert(boost::lexical_cast<float>(quantile));
      ids.push_back(i);

      for(size_t j = i + 1; j < leaves.size(); ++j) {
        if(std::regex_match(leaves[j], match, regex)) {
          step = std::string(match[1].first, match[1].second);
          steps.insert(boost::lexical_cast<size_t>(step));
          quantile = std::string(match[2].first, match[2].second);
          quantiles.insert(boost::lexical_cast<float>(quantile));
          ids.push_back(j);
        }
      }

      assert(!steps.empty());
      assert(!quantiles.empty());

      DataSpace space(Dimension(Time, *steps.begin(), *(--steps.end()),
         size_t(1)));
      space.addDimension(Dimension(CumulativeProbabilities, *quantiles.begin(),
         *(--quantiles.end()), float(0.01)));
      DataSpaceAddress address(space.address());
      address.setCoordinate<size_t>(0, *steps.begin());
      address.setCoordinate<float>(1, *quantiles.begin());

      if(std::find(extensions.begin(), extensions.end(),
         extension) == extensions.end()) {
        // Not a default extension, add it to the attribute name.
        name += extension;
      }

      raster = open((path / name).string(), space,
         address);

      if(raster) {
        {
          size_t first = 0;
          size_t last = 0;
          size_t interval = 0;

          if(isRegularIncreasingRange(first, last, interval, steps.begin(),
              steps.end())) {
            space.dimension(0).setValues(first, last, interval);
          }
        }

        {
          float first = NAN;
          float last = NAN;
          float interval = NAN;

          if(isRegularIncreasingRange(first, last, interval, quantiles.begin(),
              quantiles.end())) {
            space.dimension(1).setValues(first, last, interval);
          }
        }

        Properties const& properties(raster->properties());

        CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
          ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
          : VS_NOTDETERMINED;

        attributes.push_back(BrowseInfo(name, space, raster->type(),
              raster->typeId(), valueScale, this->name()));
      }

      // Erase all file names that belong to the stack.
      for(long j = ids.size() - 1; j >= 0; --j) {
        leaves.erase(leaves.begin() + ids[j]);
      }

      --i;
    }
  }

  // Quantiles of rasters.
  // <name>_<quantile>.{extension}
  for(int i = 0; i < int(leaves.size()); ++i) {
    if(std::regex_match(leaves[i], match, quantileOfRasterRegex)) {
      name = std::string(match[1].first, match[1].second);
      quantile = std::string(match[2].first, match[2].second);
      extension = match[3].matched ?
         std::string(match[3].first, match[3].second) : "";

      // Find all members of the data set.
      regex = std::regex(std::format(
         "{0}_({1}){2}",
         name,
         quantilePattern,
         extension));

      quantiles.clear();
      ids.clear();
      quantiles.insert(boost::lexical_cast<float>(quantile));
      ids.push_back(i);

      for(size_t j = i + 1; j < leaves.size(); ++j) {
        if(std::regex_match(leaves[j], match, regex)) {
          quantile = std::string(match[1].first, match[1].second);
          quantiles.insert(boost::lexical_cast<float>(quantile));
          ids.push_back(j);
        }
      }

      assert(!quantiles.empty());

      DataSpace space(Dimension(CumulativeProbabilities, *quantiles.begin(),
         *quantiles.begin(), float(0.01)));
      DataSpaceAddress address(space.address());
      address.setCoordinate<float>(0, *quantiles.begin());

      if(std::find(extensions.begin(), extensions.end(),
         extension) == extensions.end()) {
        // Not a default extension, add it to the attribute name.
        name += extension;
      }

      raster = open((path / name).string(), space,
         address);

      if(raster) {
        float first = NAN;
        float last = NAN;
        float interval = NAN;

        if(isRegularIncreasingRange(first, last, interval, quantiles.begin(),
              quantiles.end())) {
          space.dimension(1).setValues(first, last, interval);
        }

        Properties const& properties(raster->properties());

        CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
          ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
          : VS_NOTDETERMINED;

        attributes.push_back(BrowseInfo(name, space, raster->type(),
              raster->typeId(), valueScale, this->name()));
      }

      for(long j = ids.size() - 1; j >= 0; --j) {
        leaves.erase(leaves.begin() + ids[j]);
      }

      --i;
    }
  }

  // Temporal rasters.
  // <name>_<timestep>{.extension}
  for(int i = 0; i < int(leaves.size()); ++i) {
    if(std::regex_match(leaves[i], match, temporalRasterRegex)) {
      name = std::string(match[1].first, match[1].second);
      step = std::string(match[2].first, match[2].second);
      extension = match[3].matched ?
         std::string(match[3].first, match[3].second) : "";

      // Find all members of the stack.
      regex = std::regex(std::format("{0}_({1}){2}",
         name,
         timeStepPattern,
         extension));

      steps.clear();
      ids.clear();
      steps.insert(boost::lexical_cast<size_t>(step));
      ids.push_back(i);

      for(size_t j = i + 1; j < leaves.size(); ++j) {
        if(std::regex_match(leaves[j], match, regex)) {
          step = std::string(match[1].first, match[1].second);
          steps.insert(boost::lexical_cast<size_t>(step));
          ids.push_back(j);
        }
      }

      assert(!steps.empty());

      DataSpace space(Dimension(Time, *steps.begin(), *steps.begin(),
         size_t(1)));
      DataSpaceAddress address(space.address());
      address.setCoordinate<size_t>(0, *steps.begin());

      if(std::find(extensions.begin(), extensions.end(),
         extension) == extensions.end()) {
        // Not a default extension, add it to the attribute name.
        name += extension;
      }

      raster = open((path / name).string(), space, address);

      if(raster) {
        size_t first = 0;
        size_t last = 0;
        size_t interval = 0;

        if(isRegularIncreasingRange(first, last, interval, steps.begin(),
              steps.end())) {
          space.dimension(0).setValues(first, last, interval);
        }

        Properties const& properties(raster->properties());

        CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
          ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
          : VS_NOTDETERMINED;

        attributes.push_back(BrowseInfo(name, space, raster->type(),
              raster->typeId(), valueScale, this->name()));
      }

      // Erase all file names that belong to the stack.
      for(long j = ids.size() - 1; j >= 0; --j) {
        leaves.erase(leaves.begin() + ids[j]);
      }

      --i;
    }
  }

  if(conventions & PCRConvention) {
    // Temporal rasters.
    // 8.3 Dos convention.
    // <name><timestep>
    for(int i = 0; i < int(leaves.size()); ++i) {
      if(std::regex_match(leaves[i], match, dosRegex)) {
        std::string fileName(leaves[i]);
        assert(fileName[8] == '.');
        fileName.erase(8, 1);

        if(std::regex_match(fileName, match, stackRegex)) {
          name = std::string(match[1].first, match[1].second);
          step = std::string(match[2].first, match[2].second);
          auto const f1 = 8 - name.size();
          std::string pattern = std::vformat(
              "{0}([[:digit:]]{{{1}}}.[[:digit:]]{{3}})",
              std::make_format_args(name,
              f1));
          regex = std::regex(pattern);

          steps.clear();
          ids.clear();
          steps.insert(boost::lexical_cast<size_t>(step));
          ids.push_back(i);

          for(int j = i + 1; j < int(leaves.size()); ++j) {
            if(std::regex_match(leaves[j], match, regex)) {
              step = std::string(match[1].first, match[1].second);

              if(name.size() < 8) {
                step.erase(8 - name.size(), 1);
              }

              steps.insert(boost::lexical_cast<size_t>(step));
              ids.push_back(j);
            }
          }

          assert(!steps.empty());

          DataSpace space(Dimension(Time, *steps.begin(), *steps.begin(),
              size_t(1)));
          DataSpaceAddress address(space.address());
          address.setCoordinate<size_t>(0, *steps.begin());

          if(std::find(extensions.begin(), extensions.end(),
              extension) == extensions.end()) {
            // Not a default extension, add it to the attribute name.
            name += extension;
          }

          raster = open((path / name).string(), space,
              address);

          if(raster) {
            size_t first = 0;
            size_t last = 0;
            size_t interval = 0;

            if(isRegularIncreasingRange(first, last, interval, steps.begin(),
                  steps.end())) {
              space.dimension(0).setValues(first, last, interval);
            }

            Properties const& properties(raster->properties());

            CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
              ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
              : VS_NOTDETERMINED;

            attributes.push_back(BrowseInfo(name, space, raster->type(),
              raster->typeId(), valueScale, this->name()));
          }

          // Erase all file names that belong to the stack.
          for(long j = ids.size() - 1; j >= 0; --j) {
            leaves.erase(leaves.begin() + ids[j]);
          }

          --i;
        }
      }
    }
  }

  // Single rasters.
  for(int i = 0; i < int(leaves.size()); ++i) {
    raster = open((path / leaves[i]).string());

    if(raster) {
      Properties const& properties(raster->properties());

      CSF_VS valueScale = properties.hasValue(DAL_CSF_VALUESCALE)
        ? properties.value<CSF_VS>(DAL_CSF_VALUESCALE)
        : VS_NOTDETERMINED;

      std::filesystem::path path(leaves[i]);
      name = path.stem().string();
      extension = path.extension().string();

      if(std::find(extensions.begin(), extensions.end(),
         extension) == extensions.end()) {
        // Not a default extension, add it to the attribute name.
        name += extension;
      }

      attributes.push_back(BrowseInfo(name, DataSpace(), raster->type(),
         raster->typeId(), valueScale, this->name()));
      leaves.erase(leaves.begin() + i);
      --i;
    }
  }
}

} // namespace dal



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



