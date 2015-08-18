#ifndef INCLUDED_AG_VISUALISATIONDIALOG
#define INCLUDED_AG_VISUALISATIONDIALOG



// Library headers.
#include <map>
#include <QDialog>

// PCRaster library headers.

// Module headers.
#include "ag_IVisualisation.h"



namespace ag {
  // VisualisationDialog declarations.
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
template<class T, class Dialog>
class VisualisationDialog: public QDialog,
                           public IVisualisation
{

  friend class VisualisationDialogTest;

private:

  // Q_OBJECT

  static std::map<DataObject*, std::map<T, Dialog*> > d_dialogs;

  static void      removeReference     (VisualisationDialog<T, Dialog>* dialog);

  //! Assignment operator. NOT IMPLEMENTED.
  VisualisationDialog& operator=       (VisualisationDialog const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   VisualisationDialog (VisualisationDialog const& rhs);

protected:

  static void      addInstance         (DataObject* dataObject,
                                        T object,
                                        Dialog* dialog);

  static Dialog*   instance            (DataObject* dataObject,
                                        T object);

  static bool      instanceCreated     (DataObject* dataObject,
                                        T object);

                   VisualisationDialog (DataObject* object,
                                        std::string const& visualisationName,
                                        QWidget* parent = 0,
                                        bool modal = false,
                                        Qt::WindowFlags flags = 0);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~VisualisationDialog();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               ();

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace ag

#endif
