#ifndef INCLUDED_DAL_FORMAT
#include "dal_Format.h"
#define INCLUDED_DAL_FORMAT
#endif

// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Format class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC FORMAT MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FORMAT MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  \warning   Constructs an object in an invalid state.
*/
Format::Format()

  : d_datasetType(NR_DATASET_TYPES),
    d_medium(NrMedia),
    d_discretisation(NrDiscretisations),
    d_type(NrTypes)

{
}



Format::Format(
         std::string const& name,
         std::string const& description,
         DatasetType datasetType,
         StorageMedium medium)

  : d_name(name),
    d_description(description),
    d_datasetType(datasetType),
    d_medium(medium),
    d_discretisation(NrDiscretisations),
    d_type(NrTypes)

{
}



Format::Format(
         std::string const& name,
         std::string const& description,
         DatasetType datasetType,
         StorageMedium medium,
         Type type)

  : d_name(name),
    d_description(description),
    d_datasetType(datasetType),
    d_medium(medium),
    d_discretisation(NrDiscretisations),
    d_type(type)

{
}



//! Constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Format::Format(
         std::string const& name,
         std::string const& description,
         DatasetType datasetType,
         StorageMedium medium,
         Discretisation discretisation,
         Type type)

  : d_name(name),
    d_description(description),
    d_datasetType(datasetType),
    d_medium(medium),
    d_discretisation(discretisation),
    d_type(type)

{
}



//! Constructor.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
Format::Format(
         std::string const& name,
         std::string const& description,
         DatasetType datasetType,
         std::vector<std::string> const& extensions,
         Discretisation discretisation,
         Type type)

  : d_name(name),
    d_description(description),
    d_datasetType(datasetType),
    d_extensions(extensions),
    d_medium(File),
    d_discretisation(discretisation),
    d_type(type)

{
}



//! Destructor.
/*!
*/
Format::~Format()
{
}



void Format::setExtensions(
         std::vector<std::string> const& extensions)
{
  d_extensions = extensions;
}



//! Returns the name.
/*!
  \return    Name.
*/
std::string const& Format::name() const
{
  return d_name;
}



//! Returns the description.
/*!
  \return    Description.
*/
std::string const& Format::description() const
{
  return d_description;
}



//! Returns the dataset type of the format.
/*!
  \return    Dataset type.
*/
DatasetType Format::datasetType() const
{
  return d_datasetType;
}



//! Returns whether the format is file based.
/*!
  \return    true or false
*/
bool Format::isFileBased() const
{
  return d_medium == File;
}



//! Returns whether the format is for graphics.
/*!
  \return    true or false
  \warning   This does not say anything about the medium of the format (file,
             memory, ...), only that it is used to store graphics.
*/
bool Format::isForGraphics() const
{
  return d_type == Graphics;
}



//! Returns whether the format is for raster graphics.
/*!
  \return    true or false
  \warning   This does not say anything about the medium of the format (file,
             memory, ...), only that it is used to store raster graphics.
*/
bool Format::isForRasterGraphics() const
{
  return d_discretisation == Raster && isForGraphics();
}



//! Returns the first, most default extension.
/*!
  \return    Extension.
*/
std::string const& Format::extension() const
{
  assert(!d_extensions.empty());

  return d_extensions.front();
}



//! Returns the collection of extensions.
/*!
  \return    Extensions.
*/
std::vector<std::string> const& Format::extensions() const
{
  return d_extensions;
}



//! Returns whether one of the extensions matches \a extension.
/*!
  \param     extension Extension to check.
  \return    true or false
*/
bool Format::extensionMatches(std::string const& extension) const
{
  bool result = false;

  for(std::vector<std::string>::const_iterator it = d_extensions.begin();
         it != d_extensions.end(); ++it) {
    if((*it) == extension) {
      result = true;
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

