#ifndef INCLUDED_AG_DATAPROPERTIESDIALOG
#define INCLUDED_AG_DATAPROPERTIESDIALOG



// Library headers.
#include <memory>

// PCRaster library headers.

// Module headers.
#include "ag_VisualisationDialog.h"



namespace ag {
  // DataPropertiesDialog declarations.
  class DataGuide;
  class DataObject;
  class DataPropertiesDialogPrivate;
}



namespace ag {



//! The DataPropertiesDialog class is for editing visualisation properties.
/*!
  Visualisation properties related to data are for example: colours,
  classification, visibility.
*/
class DataPropertiesDialog:
         public ag::VisualisationDialog<DataGuide, DataPropertiesDialog>
{

private:

  Q_OBJECT

  std::auto_ptr<DataPropertiesDialogPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  DataPropertiesDialog& operator=      (const DataPropertiesDialog&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataPropertiesDialog(const DataPropertiesDialog&);

private Q_SLOTS:

  void             reject              ();

  void             accept              ();

  void             apply               ();

protected:

                   DataPropertiesDialog(DataObject* object,
                                        DataGuide const& dataGuide);

  void             createInterface     ();

  void             rescan              ();

public:

  static DataPropertiesDialog* instance(DataObject* object,
                                        DataGuide const& guide);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~DataPropertiesDialog();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace ag

#endif
