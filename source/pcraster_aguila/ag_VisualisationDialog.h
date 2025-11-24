#ifndef INCLUDED_AG_VISUALISATIONDIALOG
#define INCLUDED_AG_VISUALISATIONDIALOG

#include "ag_IVisualisation.h"

#include <QDialog>
#include <QFlags>

#include <map>


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
                                        QWidget* parent = nullptr,
                                        bool modal = false,
                                        Qt::WindowFlags flags = QFlag(0));

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

           ~VisualisationDialog() override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  bool             close               () override;

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
