#ifndef INCLUDED_AG_DATAPROPERTIESDIALOG
#define INCLUDED_AG_DATAPROPERTIESDIALOG

#include "ag_VisualisationDialog.h"

#include <memory>


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

  std::unique_ptr<DataPropertiesDialogPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  DataPropertiesDialog& operator=      (const DataPropertiesDialog&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DataPropertiesDialog(const DataPropertiesDialog&);

private Q_SLOTS:

  void             reject              () override;

  void             accept              () override;

  void             apply               ();

protected:

                   DataPropertiesDialog(DataObject* object,
                                        DataGuide const& dataGuide);

  void             createInterface     ();

  void             rescan              () override;

public:

  static DataPropertiesDialog* instance(DataObject* object,
                                        DataGuide const& guide);

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~DataPropertiesDialog() override;

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
