#ifndef INCLUDED_DAL_DATASET
#define INCLUDED_DAL_DATASET



// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_PROPERTIES
#include "dal_Properties.h"
#define INCLUDED_DAL_PROPERTIES
#endif



namespace dal {
  // Dataset declarations.
}



namespace dal {



//! Dataset is the toplevel data set class.
/*!
  More specialized classes for Table and Raster datasets (among others?) are
  deduced from Dataset.

  Dataset objects are the objects where data read or to be written is stored.
  All fundamental DatasetType s have a corresponding Dataset equivalent in
  DAL. To prevent copying large amounts of data values, ownership of the
  memory with the values can be transferred from the Dataset objects to objects
  in the application and back, if needed.

  The specialised Dataset classes like the Raster class have member variables
  which are present in every instance. For example, every Matrix object has
  nrRows and nrCols member variables, they are a fundamental part of the class
  of objects. Besides these, there might be optional properties which might
  be set for a dateset. An example of these is the value scale which is set
  by the CSFRasterDriver and which is only stored in CSF formatted rasters.
*/
class PCR_DAL_DECL Dataset
{

  friend class DatasetTest;

private:

  //! Dataset type of dataset.
  DatasetType      d_datasetType;

  //! Optional properties.
  Properties       d_properties;

protected:

                   Dataset             (DatasetType datasetType);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Dataset             (Dataset const& rhs);

  virtual          ~Dataset            ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Dataset&         operator=           (Dataset const& rhs);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  DatasetType      type                () const;

  Properties&      properties          ();

  Properties const& properties         () const;

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



} // namespace dal

#endif
