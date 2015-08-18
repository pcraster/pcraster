#ifndef INCLUDED_DAL_DATASET
#include "dal_Dataset.h"
#define INCLUDED_DAL_DATASET
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Dataset class.
*/



//------------------------------------------------------------------------------

/*
namespace dal {

class DatasetPrivate
{
public:

  DatasetPrivate()
  {
  }

  ~DatasetPrivate()
  {
  }

};

} // namespace dal
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATASET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DATASET MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     datasetType Dataset type.
*/
  // \param     name Name of the dataset.
dal::Dataset::Dataset(DatasetType datasetType /* , std::string const& name */)

  : d_datasetType(datasetType) /* , d_name(name) */

{
}



//! Copy constructor.
dal::Dataset::Dataset(Dataset const& rhs)

  : d_datasetType(rhs.d_datasetType),
    d_properties(rhs.d_properties)

{
}



//! Destructor.
/*!
*/
dal::Dataset::~Dataset()
{
}



//! Assignment operator.
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
dal::Dataset& dal::Dataset::operator=(Dataset const& rhs)
{
  if(this != &rhs) {
    d_datasetType = rhs.d_datasetType;
    d_properties = rhs.d_properties;
  }

  return *this;
}



//! Returns the name of the data for which this Dataset is created.
/*!
  \return    Name
*/
/*
std::string const& dal::Dataset::name() const
{
  return d_name;
}
*/



//! Returns the DatasetType of this Dataset.
/*!
  \return    DatasetType
*/
dal::DatasetType dal::Dataset::type() const
{
  return d_datasetType;
}



namespace dal {

//! Returns the properties of the dataset.
/*!
  \return    properties
*/
Properties& Dataset::properties()
{
  return d_properties;
}



//! Returns the properties of the dataset.
/*!
  \return    properties
*/
Properties const& Dataset::properties() const
{
  return d_properties;
}

} // namespace



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



