#ifndef INCLUDED_DAL_DRIVER
#include "dal_Driver.h"
#define INCLUDED_DAL_DRIVER
#endif

// Library headers.
#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

#ifndef INCLUDED_DAL_DATASPACEADDRESS
#include "dal_DataSpaceAddress.h"
#define INCLUDED_DAL_DATASPACEADDRESS
#endif

#ifndef INCLUDED_DAL_DATASPACEITERATOR
#include "dal_DataSpaceIterator.h"
#define INCLUDED_DAL_DATASPACEITERATOR
#endif

#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif

#ifndef INCLUDED_DAL_TABLE
#include "dal_Table.h"
#define INCLUDED_DAL_TABLE
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



/*!
  \file
  This file contains the implementation of the Driver class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRIVER MEMBERS
//------------------------------------------------------------------------------

Properties Driver::d_datasetProperties;



Properties& Driver::datasetProperties()
{
  return d_datasetProperties;
}



//------------------------------------------------------------------------------
// DEFINITION OF DRIVER MEMBERS
//------------------------------------------------------------------------------

// //! Constructor.
// /*!
//   \param     name Name of the driver.
//   \param     description Description of the driver.
//   \param     datasetType Type of datasets this driver handles.
// 
//   \a description should be a one liner (so without line breaks).
// */
// Driver::Driver(
//          std::string const& name,
//          std::string const& description,
//          DatasetType datasetType)
// 
//   : d_name(name),
//     d_description(description),
//     d_datasetType(datasetType)
// 
// {
//   assert(!d_name.empty());
//   assert(!d_description.empty());
// 
//   properties().setValue<DriverProperties>(DAL_DRIVER_GENERAL, 0);
// }



Driver::Driver(
         Format const& format)

  : d_format(format)

{
  properties().setValue<DriverProperties>(DAL_DRIVER_GENERAL, 0);
}



//! Destructor.
/*!
*/
Driver::~Driver()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   This is about driver properties.
  \sa        .

*/
Properties& Driver::properties()
{
  return d_properties;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   This is about driver properties.
  \sa        .

*/
Properties const& Driver::properties() const
{
  return d_properties;
}



std::string Driver::propertiesKey(
    std::string const& name,
    DataSpace const& space) const
{
  return dal::nameToString(name, space, datasetType());
}



bool Driver::hasProperties(
    std::string const& key) const
{
  return d_datasetProperties.hasValue(key);
}



Properties& Driver::properties(
    std::string const& key) const
{
  if(!hasProperties(key)) {
    const_cast<Properties&>(d_datasetProperties).setValue<Properties>(key,
         Properties());
  }

  return d_datasetProperties.value<Properties>(key);
}



//! Returns the properties stored for dataset \a name / \a space.
/*!
  \param     name Name of dataset.
  \param     space Data space of dataset.

  Initialises the properties object for dataset \a name, \a space, if needed.

  This is about dataset properties.
*/
Properties& Driver::properties(
         std::string const& name,
         DataSpace const& space) const
{
  return this->properties(propertiesKey(name, space));
}



Format& Driver::format()
{
  return d_format;
}



Format const& Driver::format() const
{
  return d_format;
}



//! Returns the name of this driver.
/*!
  \return    name
*/
std::string const& Driver::name() const
{
  return d_format.name();
}



//! Returns the description of this driver.
/*!
  \return    Description.
*/
std::string const& Driver::description() const
{
  return d_format.description();
}



//! Returns the DatasetType supported by this driver.
/*!
  \return    DatasetType.
*/
DatasetType Driver::datasetType() const
{
  return d_format.datasetType();
}



bool Driver::exists(
         std::string const& name) const
{
  return exists(name, DataSpace(), DataSpaceAddress());
}



Dataset* Driver::open(
         std::string const& name) const
{
  return open(name, DataSpace(), DataSpaceAddress());
}



Dataset* Driver::open(
         DataSpaceQueryResult const& result) const
{
  assert(result);

  return open(result.name(), result.space(), result.address());
}



DataSpaceQueryResult Driver::search(
         std::string const& name,
         DataSpace const& space,
         SearchHaltCondition haltCondition) const
{
  boost::shared_ptr<Dataset> firstDataset;
  DataSpaceAddress firstAddress;
  std::vector<DataSpaceAddress> validAddresses;

  if(space.rank() == 0) {
    firstDataset.reset(open(name));
  }
  else {
    boost::shared_ptr<Dataset> dataset;

    for(DataSpaceIterator it = space.begin(); it != space.end(); ++it) {
      dataset.reset(open(name, space, *it));
      if(dataset) {
        if(!firstDataset) {
          firstDataset = dataset; // This resets dataset.
          firstAddress = *it;
        }

        validAddresses.push_back(*it);

        if(haltCondition == HaltOnFirstItemFound) {
          break;
        }
      }
    }
  }

  DataSpace dataSpaceOfDataFound;

  switch(haltCondition) {
    case HaltOnFirstItemFound: {
      // Return the space passed in. We really don't know much more.
      // TODO Maybe update the first coordinate if space is not empty?
      // TODO For example, space contains time steps 1 through 100,
      // TODO but the first data item was found at 20.
      dataSpaceOfDataFound = space;
      break;
    }
    case SearchForAllItems: {
      dataSpaceOfDataFound = DataSpace(space, validAddresses);
      break;
    }
  }

  return DataSpaceQueryResult(name,
         firstDataset ? firstDataset->type() : NR_DATASET_TYPES,
         dataSpaceOfDataFound, firstAddress);
}



//! Returns information about the dimensionality of the dataset the driver supports.
/*!
  \param     name Name of source of dataset.
  \return    DataSpace object.
  \exception Exception If reading (part of) the dataset is required to
             determine the properties of the dataset it must exist.
  \warning   The fact that no exception is thrown does not imply that a
             dataset with name \a name exists. The driver might not need to
             open it.
*/
DataSpace Driver::dataSpace(
         std::string const& name) const
{
  return dataSpace(name, DataSpace(), DataSpaceAddress());
}



void Driver::remove(
         std::string const& name) const
{
  remove(name, DataSpace());
}



void Driver::remove(
         std::string const& /* name */,
         DataSpace /* space */) const
{
  assert(false);
}



FilenameConvention Driver::filenameConvention(
         std::string const& key) const
{
  // Properties for this dataset.
  Properties const& properties(this->properties(key));

  FilenameConvention result = DALConvention;

  if(properties.hasValue(DAL_FILENAME_CONVENTION)) {
    result = properties.value<FilenameConvention>(DAL_FILENAME_CONVENTION);
  }

  return result;
}



//! Returns the filenaming convention for \a name.
/*!
  \param     name Name of the dataset to return convention for.
  \return    filename convention
  \todo      See todo of determineFilenameCharacteristics.

  If no convention is stored for \a name yet the default is returned:
  DALConvention.
*/
FilenameConvention Driver::filenameConvention(
         std::string const& name,
         DataSpace const& space) const
{
  return filenameConvention(propertiesKey(name, space));
}



std::string Driver::defaultExtension(
         std::string const& key) const
{
  // Properties for this dataset.
  Properties const& properties(this->properties(key));

  std::string result;

  if(properties.hasValue(DAL_DEFAULT_EXTENSION)) {
    result = properties.value<std::string>(DAL_DEFAULT_EXTENSION);
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
  \todo      See todo of determineFilenameCharacteristics.

  An empty string is returned if no extension is stored for \a name.
*/
std::string Driver::defaultExtension(
         std::string const& name,
         DataSpace const& space) const
{
  return defaultExtension(propertiesKey(name, space));
}



//! Reads the data in \a name and returns a pointer to a newly created Dataset object.
/*!
  \param     name Name of source of dataset.
  \return    Dataset object.
  \exception Exception If the dataset could not be read successfully.

  All data in \a name is read into the Dataset object.
*/
Dataset* Driver::read(
         std::string const& name) const
{
  return read(name, DataSpace(), DataSpaceAddress());
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Test whether only time or cum probs is defined and the other
             dimensions are empty or contain only one coordinate.
  \todo      Create and call template versions of read.

  This function resizes the table before all values are written.
*/
void Driver::read(
         Table& table,
         std::string const& name,
         DataSpace const& space) const
{
  assert(space.nrWideDimensions() == 1);
  assert(table.nrCols() == 1);
  assert(table.typeId(0) == TI_REAL4);

  // if(space.hasScenarios()) {
  //   assert(space.dimension(
  //        space.indexOf(Scenarios)).nrValues() == 1);
  // }

  // assert(!space.hasSamples());
  // assert(!(space.hasTime() && space.hasCumProbabilities()));

  table.resize(space.dimension(space.indexOfWideDimension()).nrCoordinates());
  Array<REAL4>& array = table.col<REAL4>(0);

  size_t rec = 0;

  for(DataSpaceIterator it = space.begin(); it != space.end(); ++it, ++rec) {
    assert(rec < array.size());

    if(!exists(name, space, *it)) {
      pcr::setMV(array[rec]);
    }
    else {
      read(&(array[rec]), table.typeId(0), name, space, *it);
    }
  }

  assert(rec == array.size());
}



//! Temporary implementation untill all drivers have an implementation of their own.
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Make pure virtual.
*/
void Driver::browse(
         std::vector<BrowseInfo>& /* attributes */,
         std::string const& /* location */) const
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace dal
